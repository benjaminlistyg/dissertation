#!/usr/bin/env python3
"""Build a UGA Graduate School (2025 style guide) formatted PDF from the dissertation .docx.

Usage:
    python3 build_uga_pdf.py INPUT.docx OUTPUT_DIR

Produces OUTPUT_DIR/<stem>-UGA.docx and OUTPUT_DIR/<stem>-UGA.pdf.

What it does (all formatting, no content changes):
  * Times New Roman 12 pt everywhere, double spacing in the body, 1" margins.
  * Front matter rebuilt per the UGA sample pages: abstract (unnumbered), title page (i),
    copyright (ii), approval (iii), [dedication/acknowledgements if present], table of contents,
    list of tables, list of figures (roman numerals, bottom center).
  * Body page numbers restart at 1 at Chapter 1 and run continuously through the appendix
    (bottom center, also on landscape pages).
  * 1.75" top margin on first pages of sections (two blank double-spaced lines, as the guide instructs).
  * Chapter openings: "CHAPTER N" + title, centered, each on a new page.
  * Tables 1-45 and Figures 1-3 are moved to the end of the Results chapter (guide option c).
  * APA 7 caption order for both tables and figures: bold number, italic title, object, Note.
  * Oversized tables/figures scaled to fit within the landscape text width.
  * TOC / list of tables / list of figures regenerated with real page numbers (two-pass build
    using PDF bookmarks exported by LibreOffice).
Requires: LibreOffice (soffice), pypdf, lxml.
"""
import copy
import json
import os
import re
import shutil
import subprocess
import sys
import zipfile

from lxml import etree

W_NS = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'
R_NS = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships'
WP_NS = 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing'
A_NS = 'http://schemas.openxmlformats.org/drawingml/2006/main'
PKG_REL_NS = 'http://schemas.openxmlformats.org/package/2006/relationships'
CT_NS = 'http://schemas.openxmlformats.org/package/2006/content-types'
XML_NS = 'http://www.w3.org/XML/1998/namespace'

FONT = 'Times New Roman'
PORTRAIT_TEXT_W = 9360          # 6.5"  in twips
LANDSCAPE_TEXT_W = 12960        # 9.0"  in twips
EMU_PER_INCH = 914400
LANDSCAPE_MAX_IMG_W_IN = 8.9
LANDSCAPE_MAX_IMG_H_IN = 5.2
PORTRAIT_MAX_IMG_W_IN = 6.5
SYMBOL_CHARS = set('\u2713\u2717\u2714\u2718')   # check / cross marks (not in Times New Roman)


def w(tag):
    return '{%s}%s' % (W_NS, tag)


def wa(name):
    return '{%s}%s' % (W_NS, name)


# ----------------------------------------------------------------------------- text helpers
def run_text(r):
    out = []
    for el in r.iter():
        if el.tag == w('t'):
            out.append(el.text or '')
        elif el.tag == w('tab'):
            out.append('\t')
        elif el.tag == w('br') and el.get(wa('type')) != 'page':
            out.append('\n')
    return ''.join(out)


def ptext(p):
    out = []
    for el in p.iter():
        if el.tag == w('t'):
            out.append(el.text or '')
        elif el.tag == w('tab') and el.getparent().tag == w('r'):
            out.append('\t')
    return ''.join(out)


def has_drawing(p):
    return p.find('.//' + w('drawing')) is not None


def is_empty_p(p):
    return ptext(p).strip() == '' and not has_drawing(p) and p.find('.//' + w('footnoteReference')) is None


# ----------------------------------------------------------------------------- element builders
def sub(parent, tag, **attrs):
    el = etree.SubElement(parent, w(tag))
    for k, v in attrs.items():
        el.set(wa(k), str(v))
    return el


def build_ppr(*, style=None, keep_next=False, keep_lines=False, pbb=False, numpr=None, tabs=None,
              spacing=None, ind=None, jc=None, outline=None, rpr=None, contextual=False,
              widow=True):
    ppr = etree.Element(w('pPr'))
    if style:
        sub(ppr, 'pStyle', val=style)
    if keep_next:
        sub(ppr, 'keepNext')
    if keep_lines:
        sub(ppr, 'keepLines')
    if pbb:
        sub(ppr, 'pageBreakBefore')
    if widow:
        sub(ppr, 'widowControl')
    if numpr is not None:
        ppr.append(copy.deepcopy(numpr))
    if tabs:
        t = sub(ppr, 'tabs')
        for val, pos, leader in tabs:
            a = {'val': val, 'pos': pos}
            if leader:
                a['leader'] = leader
            sub(t, 'tab', **a)
    sp = {'before': 0, 'after': 0, 'line': 480, 'lineRule': 'auto'}
    if spacing:
        sp.update(spacing)
    sub(ppr, 'spacing', **sp)
    if ind:
        sub(ppr, 'ind', **ind)
    if contextual:
        sub(ppr, 'contextualSpacing')
    if jc:
        sub(ppr, 'jc', val=jc)
    if outline is not None:
        sub(ppr, 'outlineLvl', val=outline)
    if rpr is not None:
        ppr.append(copy.deepcopy(rpr))
    return ppr


def make_run(text, bold=False, italic=False, size=None, font=None):
    r = etree.Element(w('r'))
    rpr = sub(r, 'rPr')
    f = font or FONT
    sub(rpr, 'rFonts', ascii=f, hAnsi=f, cs=f, eastAsia=f)
    if bold:
        sub(rpr, 'b')
        sub(rpr, 'bCs')
    if italic:
        sub(rpr, 'i')
        sub(rpr, 'iCs')
    if size:
        sub(rpr, 'sz', val=size)
        sub(rpr, 'szCs', val=size)
    parts = re.split(r'(\t|\n)', text)
    for part in parts:
        if part == '\t':
            sub(r, 'tab')
        elif part == '\n':
            sub(r, 'br')
        elif part:
            t = sub(r, 't')
            t.text = part
            t.set('{%s}space' % XML_NS, 'preserve')
    return r


def make_p(runs, **ppr_kwargs):
    """runs: str or list of (text, dict(bold=, italic=))."""
    p = etree.Element(w('p'))
    p.append(build_ppr(**ppr_kwargs))
    if isinstance(runs, str):
        runs = [(runs, {})]
    for text, fmt in runs:
        p.append(make_run(text, **fmt))
    return p


def blank(n=1, pbb=False, **kw):
    """n empty double-spaced paragraphs; only the first carries pageBreakBefore."""
    return [make_p('', pbb=(pbb and i == 0), **kw) for i in range(n)]


def page_field_p():
    p = etree.Element(w('p'))
    p.append(build_ppr(jc='center', spacing={'line': 240}))
    r1 = make_run('')
    sub(r1, 'fldChar', fldCharType='begin')
    r2 = make_run('')
    it = sub(r2, 'instrText')
    it.text = ' PAGE '
    it.set('{%s}space' % XML_NS, 'preserve')
    r3 = make_run('')
    sub(r3, 'fldChar', fldCharType='separate')
    r4 = make_run('1')
    r5 = make_run('')
    sub(r5, 'fldChar', fldCharType='end')
    for r in (r1, r2, r3, r4, r5):
        p.append(r)
    return p


def sect_pr(landscape=False, num_fmt=None, num_start=None, header_id=None, footer_id=None, as_body=False):
    s = etree.Element(w('sectPr'))
    if header_id:
        h = sub(s, 'headerReference', type='default')
        h.set('{%s}id' % R_NS, header_id)
    if footer_id:
        f = sub(s, 'footerReference', type='default')
        f.set('{%s}id' % R_NS, footer_id)
    sub(s, 'type', val='nextPage')
    if landscape:
        sub(s, 'pgSz', w=15840, h=12240, orient='landscape')
    else:
        sub(s, 'pgSz', w=12240, h=15840)
    sub(s, 'pgMar', top=1440, right=1440, bottom=1440, left=1440, header=720, footer=720, gutter=0)
    if num_fmt or num_start:
        a = {}
        if num_fmt:
            a['fmt'] = num_fmt
        if num_start:
            a['start'] = num_start
        sub(s, 'pgNumType', **a)
    sub(s, 'cols', space=720)
    sub(s, 'docGrid', linePitch=360)
    if as_body:
        return s
    p = etree.Element(w('p'))
    ppr = sub(p, 'pPr')
    ppr.append(s)
    return p


# ----------------------------------------------------------------------------- normalization
def strip_breaks(p):
    for br in list(p.iter(w('br'))):
        if br.get(wa('type')) == 'page':
            r = br.getparent()
            r.remove(br)
            if len([c for c in r if c.tag != w('rPr')]) == 0:
                r.getparent().remove(r)
    for el in list(p.iter(w('lastRenderedPageBreak'))):
        el.getparent().remove(el)


def strip_leading_tabs(p):
    changed = True
    while changed:
        changed = False
        for r in p.findall(w('r')):
            kids = [c for c in r if c.tag != w('rPr')]
            if not kids:
                continue
            if kids[0].tag == w('tab'):
                r.remove(kids[0])
                changed = True
                if len([c for c in r if c.tag != w('rPr')]) == 0:
                    p.remove(r)
            break


def normalize_p(p, *, ind=None, jc=None, spacing=None, keep_next=False, keep_lines=False, pbb=False,
                outline=None, strip_tab=False, keep_num=True, body_font_size=True):
    old = p.find(w('pPr'))
    numpr = None
    rpr = None
    if old is not None:
        numpr = old.find(w('numPr')) if keep_num else None
        rpr = old.find(w('rPr'))
        p.remove(old)
    new = build_ppr(keep_next=keep_next, keep_lines=keep_lines, pbb=pbb, numpr=numpr, spacing=spacing,
                    ind=ind, jc=jc, outline=outline, rpr=rpr)
    p.insert(0, new)
    strip_breaks(p)
    if strip_tab:
        strip_leading_tabs(p)
    if body_font_size:
        for r in p.iter(w('r')):
            rp = r.find(w('rPr'))
            if rp is None:
                continue
            for tag in ('sz', 'szCs'):
                el = rp.find(w(tag))
                if el is not None:
                    rp.remove(el)
    return p


RPR_ORDER = ['rStyle', 'rFonts', 'b', 'bCs', 'i', 'iCs', 'caps', 'smallCaps', 'strike', 'dstrike', 'outline',
             'shadow', 'emboss', 'imprint', 'noProof', 'snapToGrid', 'vanish', 'webHidden', 'color', 'spacing', 'w',
             'kern', 'position', 'sz', 'szCs', 'highlight', 'u', 'effect', 'bdr', 'shd', 'fitText', 'vertAlign', 'rtl',
             'cs', 'em', 'lang', 'eastAsianLayout', 'specVanish', 'oMath']


def insert_rpr(rp, tag, val):
    """Insert <w:tag w:val=val/> into a run-properties element at its schema position."""
    el = etree.Element(w(tag))
    el.set(wa('val'), str(val))
    rank = RPR_ORDER.index(tag)
    for i, c in enumerate(rp):
        name = c.tag.split('}')[1]
        if name in RPR_ORDER and RPR_ORDER.index(name) > rank:
            rp.insert(i, el)
            return el
    rp.append(el)
    return el


def set_bold(p):
    """Make every run in the paragraph bold (headings whose bold came from a style)."""
    for r in p.iter(w('r')):
        rp = r.find(w('rPr'))
        if rp is None:
            rp = etree.Element(w('rPr'))
            r.insert(0, rp)
        if rp.find(w('b')) is None:
            rf = rp.find(w('rFonts'))
            idx = list(rp).index(rf) + 1 if rf is not None else 0
            b = etree.Element(w('b'))
            bcs = etree.Element(w('bCs'))
            rp.insert(idx, bcs)
            rp.insert(idx, b)
    return p


def fix_fonts(root):
    """Force Times New Roman on every run except check/cross-mark glyphs."""
    for r in root.iter(w('r')):
        txt = run_text(r)
        rp = r.find(w('rPr'))
        if rp is None:
            rp = etree.Element(w('rPr'))
            r.insert(0, rp)
        rf = rp.find(w('rFonts'))
        if rf is None:
            rf = etree.Element(w('rFonts'))
            rp.insert(0, rf)
        for k in list(rf.attrib):
            del rf.attrib[k]
        if txt and all((c in SYMBOL_CHARS or c.isspace()) for c in txt):
            f = 'DejaVu Sans'
        else:
            f = FONT
        for a in ('ascii', 'hAnsi', 'cs', 'eastAsia'):
            rf.set(wa(a), f)
    # paragraph-mark run properties
    for ppr in root.iter(w('pPr')):
        rp = ppr.find(w('rPr'))
        if rp is not None:
            rf = rp.find(w('rFonts'))
            if rf is not None:
                for k in list(rf.attrib):
                    del rf.attrib[k]
                for a in ('ascii', 'hAnsi', 'cs', 'eastAsia'):
                    rf.set(wa(a), FONT)


def fix_table(tbl, max_w):
    tblpr = tbl.find(w('tblPr'))
    if tblpr is None:
        tblpr = etree.Element(w('tblPr'))
        tbl.insert(0, tblpr)
    grid = tbl.find(w('tblGrid'))
    cols = [int(g.get(wa('w'))) for g in grid.findall(w('gridCol'))]
    total = sum(cols)
    factor = 1.0
    if total > max_w:
        factor = max_w / total
        for g in grid.findall(w('gridCol')):
            g.set(wa('w'), str(int(int(g.get(wa('w'))) * factor)))
        for tcw in tbl.iter(w('tcW')):
            if tcw.get(wa('type')) == 'dxa':
                tcw.set(wa('w'), str(int(int(tcw.get(wa('w'))) * factor)))
        total = sum(int(g.get(wa('w'))) for g in grid.findall(w('gridCol')))
    tblw = tblpr.find(w('tblW'))
    if tblw is None:
        tblw = sub(tblpr, 'tblW')
    tblw.set(wa('w'), str(total))
    tblw.set(wa('type'), 'dxa')
    ind = tblpr.find(w('tblInd'))
    if ind is not None:
        tblpr.remove(ind)
    # repeat header row on page breaks
    tr0 = tbl.find(w('tr'))
    if tr0 is not None:
        trpr = tr0.find(w('trPr'))
        if trpr is None:
            trpr = etree.Element(w('trPr'))
            # trPr goes after tblPrEx if present, else first
            idx = 1 if (len(tr0) and tr0[0].tag == w('tblPrEx')) else 0
            tr0.insert(idx, trpr)
        if trpr.find(w('tblHeader')) is None:
            sub(trpr, 'tblHeader')
    # single spacing inside cells, no extra paragraph spacing
    for p in tbl.iter(w('p')):
        ppr = p.find(w('pPr'))
        if ppr is None:
            ppr = etree.Element(w('pPr'))
            p.insert(0, ppr)
        sp = ppr.find(w('spacing'))
        if sp is None:
            sp = etree.Element(w('spacing'))
            # insert before ind/jc/rPr if present, else append
            placed = False
            for i, c in enumerate(ppr):
                if c.tag in (w('ind'), w('contextualSpacing'), w('jc'), w('rPr'), w('outlineLvl')):
                    ppr.insert(i, sp)
                    placed = True
                    break
            if not placed:
                ppr.append(sp)
        sp.set(wa('before'), '0')
        sp.set(wa('after'), '0')
        sp.set(wa('line'), '240')
        sp.set(wa('lineRule'), 'auto')
        strip_breaks(p)
    ncols = len(grid.findall(w('gridCol')))
    nrows = len(tbl.findall(w('tr')))
    # Type size inside table bodies (guide: same typeface, smaller size permitted):
    # 12 pt normally, 10 pt for very long tables, 8 pt for very wide (>= 12 column) tables.
    size = 24
    if nrows >= 60:
        size = 20
    if ncols >= 12:
        size = 16
    for r in tbl.iter(w('r')):
        rp = r.find(w('rPr'))
        if rp is None:
            rp = etree.Element(w('rPr'))
            r.insert(0, rp)
        for tag in ('sz', 'szCs'):
            el = rp.find(w(tag))
            if el is not None:
                rp.remove(el)
        if size != 24:
            insert_rpr(rp, 'sz', size)
            insert_rpr(rp, 'szCs', size)
    cell_margin = 30 if ncols >= 12 else 108
    if ncols >= 12:
        set_cell_margins(tblpr, cell_margin)
        # cells in these tables carry their own padding as paragraph indents / cell margins; tighten them
        for tcmar in tbl.iter(w('tcMar')):
            for side in ('left', 'right'):
                e = tcmar.find(w(side))
                if e is not None:
                    e.set(wa('w'), str(cell_margin))
        for ind in tbl.iter(w('ind')):
            for side in ('left', 'right'):
                if ind.get(wa(side)) is not None:
                    ind.set(wa(side), '0')
    rebalance_columns(tbl, size, cell_margin)
    return factor


def set_cell_margins(tblpr, lr):
    old = tblpr.find(w('tblCellMar'))
    if old is not None:
        tblpr.remove(old)
    mar = etree.Element(w('tblCellMar'))
    for side in ('left', 'right'):
        e = sub(mar, side, w=lr, type='dxa')
    look = tblpr.find(w('tblLook'))
    if look is not None:
        look.addprevious(mar)
    else:
        tblpr.append(mar)


def cell_text(tc):
    return ' '.join(ptext(p) for p in tc.findall(w('p')))


def rebalance_columns(tbl, size, cell_margin):
    """Content-aware column widths: only when some column's longest entry would wrap to more
    than ~1.5 lines, redistribute the table's total width (never changes the total)."""
    char_w = {24: 110, 20: 92, 16: 73}[size]
    grid = tbl.find(w('tblGrid'))
    cols = [int(g.get(wa('w'))) for g in grid.findall(w('gridCol'))]
    n = len(cols)
    total = sum(cols)
    rows = tbl.findall(w('tr'))
    maxlen = [0] * n
    header = [0] * n
    for ri, tr in enumerate(rows):
        ci = 0
        for tc in tr.findall(w('tc')):
            sp = tc.find(w('tcPr/') + w('gridSpan')) if tc.find(w('tcPr')) is not None else None
            span = int(sp.get(wa('val'))) if sp is not None else 1
            if span == 1 and ci < n:
                L = max((len(x) for x in cell_text(tc).split('\n')), default=0)
                maxlen[ci] = max(maxlen[ci], L)
                if ri == 0:
                    header[ci] = L
            ci += span
    usable = [c - 2 * cell_margin for c in cols]
    cramped = any(maxlen[j] * char_w > 1.5 * usable[j] for j in range(n))
    if not cramped:
        return
    floor = [max(int(header[j] * char_w * 1.3) + 2 * cell_margin, 700) for j in range(n)]
    need = [max(floor[j], min(maxlen[j] * char_w + 2 * cell_margin, int(0.45 * total))) for j in range(n)]
    if sum(need) <= total:
        extra = total - sum(need)
        new = [need[j] + extra * need[j] // sum(need) for j in range(n)]
    else:
        rem = total - sum(floor)
        if rem < 0:
            return
        gap = [need[j] - floor[j] for j in range(n)]
        sg = sum(gap) or 1
        new = [floor[j] + rem * gap[j] // sg for j in range(n)]
    new[-1] += total - sum(new)
    for g, wv in zip(grid.findall(w('gridCol')), new):
        g.set(wa('w'), str(wv))
    for tr in rows:
        ci = 0
        for tc in tr.findall(w('tc')):
            tcpr = tc.find(w('tcPr'))
            sp = tcpr.find(w('gridSpan')) if tcpr is not None else None
            span = int(sp.get(wa('val'))) if sp is not None else 1
            width = sum(new[ci:ci + span])
            if tcpr is None:
                tcpr = etree.Element(w('tcPr'))
                tc.insert(0, tcpr)
            tcw = tcpr.find(w('tcW'))
            if tcw is None:
                tcw = etree.Element(w('tcW'))
                tcpr.insert(0, tcw)
            tcw.set(wa('w'), str(width))
            tcw.set(wa('type'), 'dxa')
            ci += span


def fix_drawing(p, max_w_in, max_h_in):
    for ext in p.iter('{%s}extent' % WP_NS):
        cx, cy = int(ext.get('cx')), int(ext.get('cy'))
        f = min(1.0, max_w_in * EMU_PER_INCH / cx, max_h_in * EMU_PER_INCH / cy)
        if f < 1.0:
            ext.set('cx', str(int(cx * f)))
            ext.set('cy', str(int(cy * f)))
            for ae in p.iter('{%s}ext' % A_NS):
                ae.set('cx', str(int(int(ae.get('cx')) * f)))
                ae.set('cy', str(int(int(ae.get('cy')) * f)))


# ----------------------------------------------------------------------------- roman numerals
def roman(n):
    vals = [(10, 'x'), (9, 'ix'), (5, 'v'), (4, 'iv'), (1, 'i')]
    out = ''
    for v, s in vals:
        while n >= v:
            out += s
            n -= v
    return out


# ----------------------------------------------------------------------------- main build
CAPTION_RE = re.compile(r'^\s*Table\s+(A?\d+)\.?\s*$')
FIG_RE = re.compile(r'^\s*Figure\s+(\d+)\.?\s*$')
CHAPTER_RE = re.compile(r'^\s*CHAPTER\s+(\d+)\s*$')
HYP_RE = re.compile(r'^\s*(H\d+[a-z]?|RQ\d+)\s*:')


class Builder:
    def __init__(self, src_docx, workdir, page_map=None):
        self.src = src_docx
        self.work = workdir
        self.page_map = page_map or {}
        self.entries = []        # TOC entries: (level, label, text, key)
        self.tables = []         # (key, number, title)
        self.figures = []        # (key, number, title)
        self.notes = []

    # ---- package handling
    def unpack(self):
        self.unp = os.path.join(self.work, 'unpacked')
        if os.path.exists(self.unp):
            shutil.rmtree(self.unp)
        with zipfile.ZipFile(self.src) as z:
            z.extractall(self.unp)
        self.doc_path = os.path.join(self.unp, 'word', 'document.xml')
        self.tree = etree.parse(self.doc_path)
        self.root = self.tree.getroot()
        self.body = self.root.find(w('body'))
        self.items = list(self.body)

    def pg(self, key, default='000'):
        return str(self.page_map.get(key, default))

    # ---- content extraction
    def extract_front(self):
        items = self.items
        texts = [(i, ptext(el).strip()) for i, el in enumerate(items) if el.tag == w('p')]
        # locate page-break-only paragraphs in the front matter region (before CHAPTER 1)
        ch1 = next(i for i, el in enumerate(items) if el.tag == w('p') and CHAPTER_RE.match(ptext(el) or ''))
        self.ch1 = ch1
        pbs = [i for i, el in enumerate(items[:ch1]) if el.tag == w('p') and
               any(b.get(wa('type')) == 'page' for b in el.iter(w('br')))]
        secs = [i for i, el in enumerate(items[:ch1]) if el.tag == w('p') and el.find('.//' + w('sectPr')) is not None]
        nonempty = lambda a, b: [ptext(items[i]).strip() for i in range(a, b) if items[i].tag == w('p') and ptext(items[i]).strip()]
        # abstract block: start -> first page break
        ab = nonempty(0, pbs[0])
        self.title = ab[0]
        assert ab[1].lower() == 'by', ab[:3]
        self.author = ab[2]
        self.direction = ab[3]
        assert ab[4].upper() == 'ABSTRACT'
        iw = next(k for k, t in enumerate(ab) if t.upper().startswith('INDEX WORDS'))
        self.abstract_paras = ab[5:iw]
        self.index_words = ab[iw]
        # title page block
        tp = nonempty(pbs[0] + 1, pbs[1])
        self.tp_title = tp[0]
        self.tp_name = tp[2]
        k = 3
        self.degrees = []
        while not tp[k].startswith('A Dissertation'):
            self.degrees.append(tp[k])
            k += 1
        self.tp_statement = tp[k]
        self.tp_degree = tp[k + 1]
        self.tp_city = tp[k + 2]
        self.tp_year = tp[k + 3]
        # copyright block
        cp = nonempty(pbs[1] + 1, pbs[2])
        self.copyright_lines = cp
        # approval block (up to first section break)
        ap = nonempty(pbs[2] + 1, secs[0] + 1)
        self.ap_title = ap[0]
        self.ap_name = ap[2]
        self.ap_major = None
        self.ap_committee = []
        self.ap_bottom = []
        mode = 'top'
        for t in ap[3:]:
            tt = re.sub(r'\s+', ' ', t.replace('\t', ' ')).strip()
            if tt.startswith('Major Professor'):
                self.ap_major = tt.split(':', 1)[1].strip()
                mode = 'committee'
            elif tt.startswith('Committee'):
                self.ap_committee.append(tt.split(':', 1)[1].strip())
            elif tt.startswith('Electronic Version Approved'):
                mode = 'bottom'
                self.ap_bottom.append(tt)
            elif mode == 'committee':
                self.ap_committee.append(tt)
            elif mode == 'bottom':
                self.ap_bottom.append(tt)
        # dedication / acknowledgements text (optional)
        self.dedication = []
        self.acknowledgements = []
        for i, el in enumerate(items[secs[0] + 1:ch1], start=secs[0] + 1):
            if el.tag != w('p'):
                continue
            t = ptext(el).strip()
            if t.upper() == 'DEDICATION':
                j = i + 1
                while j < ch1 and not (items[j].tag == w('p') and any(b.get(wa('type')) == 'page' for b in items[j].iter(w('br')))):
                    if items[j].tag == w('p') and ptext(items[j]).strip() and ptext(items[j]).strip().upper() not in ('ACKNOWLEDGEMENTS',):
                        self.dedication.append(items[j])
                    j += 1
            if t.upper() == 'ACKNOWLEDGEMENTS' and i < ch1:
                j = i + 1
                while j < ch1 and not (items[j].tag == w('p') and any(b.get(wa('type')) == 'page' for b in items[j].iter(w('br')))):
                    if items[j].tag == w('p') and ptext(items[j]).strip() and not ptext(items[j]).strip().upper().startswith('TABLE OF CONTENTS'):
                        self.acknowledgements.append(items[j])
                    j += 1
        # stop acknowledgements collection at the TOC heading
        self.acknowledgements = [p for p in self.acknowledgements if 'TABLE OF CONTENTS' not in ptext(p).upper()]

    # ---- front matter output
    def front_matter(self):
        out = []
        # ABSTRACT (no page number, not counted)
        out += blank(2)
        out.append(make_p(self.title, jc='center'))
        out.append(make_p('by', jc='center'))
        out.append(make_p(self.author, jc='center'))
        out.append(make_p(self.direction, jc='center'))
        out.append(make_p('ABSTRACT', jc='center'))
        for t in self.abstract_paras:
            out.append(make_p(t.lstrip('\t '), ind={'firstLine': 720}))
        iw_label, iw_body = self.index_words.split(':', 1)
        out.append(make_p(iw_label.strip() + ':\t' + iw_body.strip(), ind={'left': 2160, 'hanging': 2160},
                          tabs=[('left', 2160, None)]))
        out.append(sect_pr(header_id='rIdHdrBlank', footer_id='rIdFtrBlank'))
        # TITLE PAGE (i)
        out += blank(2)
        out.append(make_p(self.tp_title, jc='center'))
        out += blank(1)
        out.append(make_p('by', jc='center'))
        out += blank(1)
        out.append(make_p(self.tp_name, jc='center'))
        for d in self.degrees:
            out.append(make_p(d, jc='center'))
        out += blank(5)
        out.append(make_p(self.tp_statement, jc='center'))
        out += blank(1)
        out.append(make_p(self.tp_degree, jc='center'))
        out += blank(1)
        out.append(make_p(self.tp_city, jc='center'))
        out.append(make_p(self.tp_year, jc='center'))
        # COPYRIGHT (ii) - lower half of page
        first = True
        for _ in range(18):
            out.append(make_p('', jc='center', pbb=first))
            first = False
        for t in self.copyright_lines:
            out.append(make_p(t, jc='center'))
        # APPROVAL (iii)
        out.append(make_p('', pbb=True))
        out += blank(1)
        out.append(make_p(self.ap_title, jc='center'))
        out += blank(1)
        out.append(make_p('by', jc='center'))
        out += blank(1)
        out.append(make_p(self.ap_name, jc='center'))
        out += blank(3)
        tabs = [('left', 4320, None), ('left', 6480, None)]
        single = {'line': 240}
        out.append(make_p('\tMajor Professor:\t' + self.ap_major, tabs=tabs, spacing=single))
        for k, name in enumerate(self.ap_committee):
            label = 'Committee:' if k == 0 else ''
            out.append(make_p('\t' + label + '\t' + name, tabs=tabs, spacing=single))
        out += blank(1, spacing=single)
        out += blank(10, spacing=single)
        for k, t in enumerate(self.ap_bottom):
            out.append(make_p(t, spacing=single))
            if k == 0:
                out.append(make_p('', spacing=single))
        out.append(sect_pr(num_fmt='lowerRoman', num_start=1, header_id='rIdHdrBlank', footer_id='rIdFtrBlank'))
        # DEDICATION (optional, not in TOC)
        first_in_section = True
        if self.dedication:
            out += blank(2)
            out.append(make_p('DEDICATION', jc='center'))
            for p in self.dedication:
                out.append(normalize_p(copy.deepcopy(p), ind={'firstLine': 720}, strip_tab=True))
            first_in_section = False
        # ACKNOWLEDGEMENTS (optional, in TOC)
        if self.acknowledgements:
            out += blank(2, pbb=not first_in_section)
            out.append(make_p('ACKNOWLEDGEMENTS', jc='center', outline=0))
            for p in self.acknowledgements:
                out.append(normalize_p(copy.deepcopy(p), ind={'firstLine': 720}, strip_tab=True))
            first_in_section = False
            self.front_entries = [('ACKNOWLEDGEMENTS', 'ACKNOWLEDGEMENTS')]
        else:
            self.front_entries = []
        self.toc_insert_at = len(out)
        self.toc_pbb = not first_in_section
        out.append(sect_pr(num_fmt='lowerRoman', header_id='rIdHdrBlank', footer_id='rIdFtrNum'))
        return out

    def toc_block(self):
        """Build TOC + LOT + LOF paragraphs (requires self.entries/tables/figures)."""
        out = []
        dot = ('right', PORTRAIT_TEXT_W, 'dot')
        # ---- TABLE OF CONTENTS
        out += blank(2, pbb=self.toc_pbb)
        out.append(make_p('TABLE OF CONTENTS', jc='center'))
        out.append(make_p('Page', jc='right'))
        for text, key in self.front_entries:
            out.append(make_p(text + '\t' + self.pg(key), tabs=[dot], ind={'left': 0}))
        out.append(make_p('LIST OF TABLES\t' + self.pg('LIST OF TABLES'), tabs=[dot]))
        out.append(make_p('LIST OF FIGURES\t' + self.pg('LIST OF FIGURES'), tabs=[dot]))
        out.append(make_p('CHAPTER'))
        for level, label, text, key in self.entries:
            if level == 0 and label:          # chapter: number + title
                out.append(make_p(label + '\t' + text + '\t' + self.pg(key),
                                  tabs=[('left', 1080, None), dot], ind={'left': 1080, 'hanging': 720}))
            elif level == 0:                  # REFERENCES / APPENDIX at margin
                out.append(make_p(text + '\t' + self.pg(key), tabs=[dot], ind={'left': 0}))
            else:                             # subsection
                out.append(make_p(text + '\t' + self.pg(key), tabs=[dot], ind={'left': 1440}))
        # ---- LIST OF TABLES
        out += blank(2, pbb=True)
        out.append(make_p('LIST OF TABLES', jc='center', outline=0))
        out.append(make_p('Page', jc='right'))
        for key, num, title in self.tables:
            out.append(make_p('Table %s:\t%s\t%s' % (num, title, self.pg(key)),
                              tabs=[('left', 1440, None), dot], ind={'left': 1440, 'hanging': 1440}))
        # ---- LIST OF FIGURES
        out += blank(2, pbb=True)
        out.append(make_p('LIST OF FIGURES', jc='center', outline=0))
        out.append(make_p('Page', jc='right'))
        for key, num, title in self.figures:
            out.append(make_p('Figure %s:\t%s\t%s' % (num, title, self.pg(key)),
                              tabs=[('left', 1440, None), dot], ind={'left': 1440, 'hanging': 1440}))
        return out

    # ---- body
    def style_name(self, p):
        ppr = p.find(w('pPr'))
        if ppr is None:
            return 'Normal'
        ps = ppr.find(w('pStyle'))
        return ps.get(wa('val')) if ps is not None else 'Normal'

    def body_para(self, p, in_appendix=False):
        """Normalize a body text paragraph (reusing its runs)."""
        t = ptext(p)
        st = self.style_name(p)
        q = copy.deepcopy(p)
        if HYP_RE.match(t):
            # keep paired hypotheses / research questions (H7a+H7b, RQ7+RQ8) on the same page
            nxt = p.getnext()
            keep = nxt is not None and nxt.tag == w('p') and HYP_RE.match(ptext(nxt) or '') is not None
            return normalize_p(q, ind={'left': 720}, strip_tab=True, keep_next=keep)
        if st in ('DISS3SUBSECTION', 'DISS4APPENDIXTITLE') and (len(t.strip()) > 70 or t.strip().endswith('.')):
            self.notes.append('paragraph styled as subsection treated as body text: ' + t.strip()[:60])
            return normalize_p(q, ind={'firstLine': 720}, strip_tab=True)
        if st in ('DISS3SUBSECTION', 'DISS4APPENDIXTITLE'):
            self.entries.append((1, '', t.strip(), t.strip()))
            return set_bold(normalize_p(q, keep_next=True, keep_lines=True, outline=1))
        ppr = p.find(w('pPr'))
        has_num = ppr is not None and ppr.find(w('numPr')) is not None
        if has_num:
            return normalize_p(q, ind={'left': 720, 'hanging': 360}, strip_tab=True)
        return normalize_p(q, ind={'firstLine': 720}, strip_tab=True)

    def caption_pair(self, label, title, pbb, key):
        a = make_p([(label, {'bold': True})], keep_next=True, pbb=pbb, outline=2)
        b = make_p([(title, {'italic': True})], keep_next=True)
        return [a, b]

    def collect_tables(self, start, end, max_w):
        """Return list of blocks [(caption_label, title, tbl, notes)] from items[start:end]."""
        items = self.items
        blocks = []
        i = start
        while i < end:
            el = items[i]
            if el.tag == w('p') and CAPTION_RE.match(ptext(el) or ''):
                num = CAPTION_RE.match(ptext(el)).group(1)
                j = i + 1
                title = ''
                while j < end and items[j].tag == w('p'):
                    tt = ptext(items[j]).strip()
                    if tt:
                        title = tt
                        break
                    j += 1
                while j < end and items[j].tag != w('tbl'):
                    j += 1
                tbl = copy.deepcopy(items[j])
                fix_table(tbl, max_w)
                notes = []
                k = j + 1
                while k < end and items[k].tag == w('p'):
                    tt = ptext(items[k]).strip()
                    if CAPTION_RE.match(tt) or FIG_RE.match(tt) or tt.upper() == 'APPENDIX':
                        break
                    if tt:
                        notes.append(normalize_p(copy.deepcopy(items[k]), spacing={'line': 240}, body_font_size=True))
                    k += 1
                blocks.append((num, title, tbl, notes))
                i = k
            else:
                i += 1
        return blocks

    def collect_figures(self, start, end):
        items = self.items
        figs = []
        i = start
        while i < end:
            el = items[i]
            if el.tag == w('p') and FIG_RE.match(ptext(el) or ''):
                num = FIG_RE.match(ptext(el)).group(1)
                title = ptext(items[i + 1]).strip()
                j = i + 2
                while j < end and not has_drawing(items[j]):
                    j += 1
                img = copy.deepcopy(items[j])
                notes = []
                k = j + 1
                while k < end and items[k].tag == w('p'):
                    tt = ptext(items[k]).strip()
                    if FIG_RE.match(tt) or tt.upper() == 'APPENDIX' or CAPTION_RE.match(tt):
                        break
                    if tt:
                        notes.append(normalize_p(copy.deepcopy(items[k]), spacing={'line': 240}))
                    k += 1
                figs.append((num, title, img, notes))
                i = k
            else:
                i += 1
        return figs

    def build_body(self):
        items = self.items
        n = len(items)
        out = []
        # indices of key landmarks
        chapters = [i for i, el in enumerate(items) if el.tag == w('p') and CHAPTER_RE.match(ptext(el) or '')]
        refs = next(i for i, el in enumerate(items) if el.tag == w('p') and self.style_name(el) == 'DISSREFERENCE')
        first_table = next(i for i, el in enumerate(items[refs:], start=refs)
                           if el.tag == w('p') and CAPTION_RE.match(ptext(el) or ''))
        first_fig = next(i for i, el in enumerate(items[first_table:], start=first_table)
                         if el.tag == w('p') and FIG_RE.match(ptext(el) or ''))
        appendix = next(i for i, el in enumerate(items[first_fig:], start=first_fig)
                        if el.tag == w('p') and ptext(el).strip().upper() == 'APPENDIX')
        first_atable = next(i for i, el in enumerate(items[appendix:], start=appendix)
                            if el.tag == w('p') and CAPTION_RE.match(ptext(el) or ''))
        chapter_bounds = chapters + [refs]
        results_idx = None
        for ci, c in enumerate(chapters):
            title = ptext(items[c + 1]).strip()
            if title.upper() == 'RESULTS':
                results_idx = ci
        assert results_idx is not None

        # tables & figures blocks (moved to end of Results chapter)
        tbl_blocks = self.collect_tables(first_table, first_fig, LANDSCAPE_TEXT_W)
        fig_blocks = self.collect_figures(first_fig, appendix)
        app_blocks = self.collect_tables(first_atable, n, LANDSCAPE_TEXT_W)

        def emit_chapter(ci, first_in_section):
            c = chapter_bounds[ci]
            end = chapter_bounds[ci + 1]
            num = CHAPTER_RE.match(ptext(items[c])).group(1)
            title = ptext(items[c + 1]).strip()
            out.extend(blank(2, pbb=not first_in_section))
            out.append(make_p('CHAPTER ' + num, jc='center', keep_next=True))
            out.append(make_p(title, jc='center', keep_next=True, outline=0))
            self.entries.append((0, num, title, title))
            for i in range(c + 2, end):
                el = items[i]
                if el.tag == w('tbl'):
                    t = copy.deepcopy(el)
                    fix_table(t, PORTRAIT_TEXT_W)
                    out.append(t)
                    continue
                if el.tag != w('p') or is_empty_p(el):
                    continue
                if has_drawing(el):
                    q = normalize_p(copy.deepcopy(el), jc='center')
                    fix_drawing(q, PORTRAIT_MAX_IMG_W_IN, 8.0)
                    out.append(q)
                    continue
                out.append(self.body_para(el))

        # ---- Chapters 1 .. Results (portrait, arabic from 1)
        for ci in range(0, results_idx + 1):
            emit_chapter(ci, first_in_section=(ci == 0))
        out.append(sect_pr(num_fmt='decimal', num_start=1, header_id='rIdHdrBlank', footer_id='rIdFtrNum'))
        # ---- Tables + figures (landscape)
        first = True
        for num, title, tbl, notes in tbl_blocks:
            key = 'Table ' + num
            out.extend(self.caption_pair(key, title, pbb=not first, key=key))
            out.append(tbl)
            out.extend(notes)
            self.tables.append((key, num, title))
            first = False
        for num, title, img, notes in fig_blocks:
            # APA 7: bold number, italic title, image, then Note (same order as tables)
            key = 'Figure ' + num
            out.extend(self.caption_pair(key, title, pbb=not first, key=key))
            img = normalize_p(img, jc='center', keep_next=True)
            fix_drawing(img, LANDSCAPE_MAX_IMG_W_IN, LANDSCAPE_MAX_IMG_H_IN)
            out.append(img)
            out.extend(notes)
            self.figures.append((key, num, title))
            first = False
        out.append(sect_pr(landscape=True, header_id='rIdHdrBlank', footer_id='rIdFtrNum'))
        # ---- remaining chapters (portrait)
        for ci in range(results_idx + 1, len(chapters)):
            emit_chapter(ci, first_in_section=(ci == results_idx + 1))
        # ---- REFERENCES
        out.extend(blank(2, pbb=True))
        out.append(make_p('REFERENCES', jc='center', outline=0))
        self.entries.append((0, '', 'REFERENCES', 'REFERENCES'))
        for i in range(refs + 1, first_table):
            el = items[i]
            if el.tag != w('p') or is_empty_p(el):
                continue
            out.append(normalize_p(copy.deepcopy(el), ind={'left': 720, 'hanging': 720}, strip_tab=True))
        # ---- APPENDIX prose
        out.extend(blank(2, pbb=True))
        out.append(make_p('APPENDIX', jc='center', outline=0))
        self.entries.append((0, '', 'APPENDIX', 'APPENDIX'))
        prose = [items[i] for i in range(appendix + 1, first_atable) if items[i].tag == w('p') and not is_empty_p(items[i])]
        merged = self.merge_hard_wraps(prose)
        for el in merged:
            out.append(self.body_para(el, in_appendix=True))
        out.append(sect_pr(header_id='rIdHdrBlank', footer_id='rIdFtrNum'))
        # ---- appendix tables: kept in source (numeric) order; consecutive tables of the same
        # orientation share a section (landscape for wide tables, portrait for narrow ones).
        # A section's orientation lives in the sectPr that closes it, so a break is emitted
        # whenever the orientation changes, and the last group's sectPr becomes the body sectPr.
        cur_landscape = None
        first = True
        for num, title, tbl, notes in app_blocks:
            landscape = sum(int(g.get(wa('w'))) for g in tbl.find(w('tblGrid'))) > PORTRAIT_TEXT_W
            if cur_landscape is None:
                cur_landscape = landscape
            elif landscape != cur_landscape:
                out.append(sect_pr(landscape=cur_landscape, header_id='rIdHdrBlank', footer_id='rIdFtrNum'))
                cur_landscape = landscape
                first = True
            key = 'Table ' + num
            out.extend(self.caption_pair(key, title, pbb=not first, key=key))
            out.append(tbl)
            out.extend(notes)
            self.tables.append((key, num, title))
            first = False
        final = sect_pr(landscape=bool(cur_landscape), header_id='rIdHdrBlank', footer_id='rIdFtrNum', as_body=True)
        return out, final

    def merge_hard_wraps(self, paras):
        """Join consecutive paragraphs that are hard-wrapped lines of a single paragraph
        (short lines without a first-line indent). A line that follows a completed sentence and
        opens a new MIDUS-wave description starts a new paragraph."""
        out = []
        cur = None
        for p in paras:
            t = ptext(p).strip()
            st = self.style_name(p)
            ppr = p.find(w('pPr'))
            ind = ppr.find(w('ind')) if ppr is not None else None
            indented = ind is not None and ind.get(wa('firstLine')) not in (None, '0')
            heading = st in ('DISS3SUBSECTION', 'DISS4APPENDIXTITLE')
            if cur is not None and not heading and not indented and not HYP_RE.match(t):
                prev_t = ptext(cur).strip()
                prev_ends = prev_t.endswith(('.', '?', '!', ':'))
                starts_new = prev_ends and re.match(r'^In MIDUS \d', t) is not None
                if not starts_new and (not prev_ends or len(prev_t) < 100):
                    cur.append(make_run(' '))
                    for r in [c for c in p if c.tag != w('pPr')]:
                        cur.append(copy.deepcopy(r))
                    self.notes.append('merged hard-wrapped line into previous paragraph: ' + t[:70])
                    continue
            cur = copy.deepcopy(p)
            out.append(cur)
        return out

    # ---- package parts
    def write_parts(self):
        wd = os.path.join(self.unp, 'word')
        # remove old headers/footers
        rels_path = os.path.join(wd, '_rels', 'document.xml.rels')
        rels = etree.parse(rels_path)
        rroot = rels.getroot()
        for rel in list(rroot):
            if rel.get('Type').endswith('/header') or rel.get('Type').endswith('/footer'):
                tgt = os.path.join(wd, rel.get('Target'))
                if os.path.exists(tgt):
                    os.remove(tgt)
                rroot.remove(rel)
        ct_path = os.path.join(self.unp, '[Content_Types].xml')
        ct = etree.parse(ct_path)
        croot = ct.getroot()
        for o in list(croot):
            pn = o.get('PartName') or ''
            if re.match(r'/word/(header|footer)\d*\.xml', pn):
                croot.remove(o)
        nsmap = dict(self.root.nsmap)

        def part(tag, content_children):
            el = etree.Element(w(tag), nsmap=nsmap)
            for c in content_children:
                el.append(c)
            return etree.tostring(el, xml_declaration=True, encoding='UTF-8', standalone=True)

        parts = {
            'headerBlank.xml': ('header', part('hdr', [make_p('', spacing={'line': 240})]), 'rIdHdrBlank'),
            'footerBlank.xml': ('footer', part('ftr', [make_p('', spacing={'line': 240})]), 'rIdFtrBlank'),
            'footerNum.xml': ('footer', part('ftr', [page_field_p()]), 'rIdFtrNum'),
        }
        for fname, (kind, data, rid) in parts.items():
            with open(os.path.join(wd, fname), 'wb') as f:
                f.write(data)
            rel = etree.SubElement(rroot, '{%s}Relationship' % PKG_REL_NS)
            rel.set('Id', rid)
            rel.set('Type', 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/' + kind)
            rel.set('Target', fname)
            o = etree.SubElement(croot, '{%s}Override' % CT_NS)
            o.set('PartName', '/word/' + fname)
            o.set('ContentType', 'application/vnd.openxmlformats-officedocument.wordprocessingml.%s+xml' % kind)
        rels.write(rels_path, xml_declaration=True, encoding='UTF-8', standalone=True)
        ct.write(ct_path, xml_declaration=True, encoding='UTF-8', standalone=True)

    def fix_styles(self):
        sp = os.path.join(self.unp, 'word', 'styles.xml')
        s = open(sp, encoding='utf-8').read()
        # document defaults: Times New Roman 12, double spaced, no paragraph spacing
        s = re.sub(r'<w:rPrDefault>.*?</w:rPrDefault>',
                   '<w:rPrDefault><w:rPr><w:rFonts w:ascii="%s" w:eastAsia="%s" w:hAnsi="%s" w:cs="%s"/>'
                   '<w:sz w:val="24"/><w:szCs w:val="24"/><w:lang w:val="en-US" w:eastAsia="en-US" w:bidi="ar-SA"/></w:rPr></w:rPrDefault>'
                   % (FONT, FONT, FONT, FONT), s, flags=re.S)
        s = re.sub(r'<w:pPrDefault>.*?</w:pPrDefault>',
                   '<w:pPrDefault><w:pPr><w:widowControl/><w:spacing w:before="0" w:after="0" w:line="480" w:lineRule="auto"/></w:pPr></w:pPrDefault>',
                   s, flags=re.S)
        # Normal: explicit
        s = re.sub(r'(<w:style w:type="paragraph" w:default="1" w:styleId="Normal">.*?)<w:rPr>.*?</w:rPr>(.*?</w:style>)',
                   r'\1<w:rPr><w:rFonts w:ascii="%s" w:eastAsia="%s" w:hAnsi="%s" w:cs="%s"/><w:sz w:val="24"/><w:szCs w:val="24"/></w:rPr>\2' % (FONT, FONT, FONT, FONT),
                   s, flags=re.S)
        # Neutralize style-level font/size/spacing overrides that would fight the body size
        s = re.sub(r'<w:sz w:val="(?!24")\d+"/>', '<w:sz w:val="24"/>', s)
        s = re.sub(r'<w:szCs w:val="(?!24")\d+"/>', '<w:szCs w:val="24"/>', s)
        s = re.sub(r'<w:rFonts [^/]*/>', '<w:rFonts w:ascii="%s" w:eastAsia="%s" w:hAnsi="%s" w:cs="%s"/>' % (FONT, FONT, FONT, FONT), s)
        # Header/Footer styles: single spaced, centered content handled per paragraph
        for sid in ('Header', 'Footer'):
            s = re.sub(r'(<w:style w:type="paragraph" w:styleId="%s">.*?)<w:pPr>.*?</w:pPr>' % sid,
                       r'\1<w:pPr><w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="auto"/></w:pPr>', s, flags=re.S)
        # Hyperlinks: keep underline, black text for print
        s = re.sub(r'(<w:style w:type="character" w:styleId="Hyperlink">.*?)<w:color [^/]*/>', r'\1<w:color w:val="000000"/>', s, flags=re.S)
        s = re.sub(r'(<w:style w:type="character" w:styleId="Hyperlink">.*?)<w:u w:val="single"/>', r'\1', s, flags=re.S)
        open(sp, 'w', encoding='utf-8').write(s)
        # footnotes: single spaced, 10 pt allowed; keep font
        fp = os.path.join(self.unp, 'word', 'footnotes.xml')
        if os.path.exists(fp):
            f = open(fp, encoding='utf-8').read()
            f = re.sub(r'w:ascii="[^"]*"', 'w:ascii="%s"' % FONT, f)
            f = re.sub(r'w:hAnsi="[^"]*"', 'w:hAnsi="%s"' % FONT, f)
            f = re.sub(r'w:cs="[^"]*"', 'w:cs="%s"' % FONT, f)
            open(fp, 'w', encoding='utf-8').write(f)

    def fix_core(self):
        cp = os.path.join(self.unp, 'docProps', 'core.xml')
        if not os.path.exists(cp):
            return
        c = open(cp, encoding='utf-8').read()
        c = re.sub(r'<dc:title>.*?</dc:title>', '<dc:title>%s</dc:title>' % self.title, c, flags=re.S)
        c = re.sub(r'<dc:creator>.*?</dc:creator>', '<dc:creator>%s</dc:creator>' % self.author, c, flags=re.S)
        open(cp, 'w', encoding='utf-8').write(c)

    def build(self, out_docx):
        self.unpack()
        self.extract_front()
        front = self.front_matter()
        body, final = self.build_body()
        toc = self.toc_block()
        front[self.toc_insert_at:self.toc_insert_at] = toc
        new_body = etree.Element(w('body'))
        for el in front + body:
            new_body.append(el)
        new_body.append(final)
        fix_fonts(new_body)
        self.root.remove(self.body)
        self.root.append(new_body)
        self.tree.write(self.doc_path, xml_declaration=True, encoding='UTF-8', standalone=True)
        self.write_parts()
        self.fix_styles()
        self.fix_core()
        if os.path.exists(out_docx):
            os.remove(out_docx)
        with zipfile.ZipFile(out_docx, 'w', zipfile.ZIP_DEFLATED) as z:
            # [Content_Types].xml first
            z.write(os.path.join(self.unp, '[Content_Types].xml'), '[Content_Types].xml')
            for folder, _, files in os.walk(self.unp):
                for fn in files:
                    full = os.path.join(folder, fn)
                    arc = os.path.relpath(full, self.unp)
                    if arc == '[Content_Types].xml':
                        continue
                    z.write(full, arc)
        return out_docx


# ----------------------------------------------------------------------------- PDF conversion & page lookup
def to_pdf(docx_path, outdir, fontconfig=None):
    env = dict(os.environ)
    if fontconfig:
        env['FONTCONFIG_FILE'] = fontconfig
    cmd = ['soffice', '--headless', '--convert-to', 'pdf', '--outdir', outdir, docx_path]
    subprocess.run(cmd, check=True, env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=600)
    pdf = os.path.join(outdir, os.path.splitext(os.path.basename(docx_path))[0] + '.pdf')
    assert os.path.exists(pdf), pdf
    return pdf


def page_lookup(pdf, keys):
    from pypdf import PdfReader
    reader = PdfReader(pdf)
    found = {}

    def walk(items):
        for it in items:
            if isinstance(it, list):
                walk(it)
            else:
                try:
                    pg = reader.get_destination_page_number(it)
                except Exception:
                    continue
                title = (it.title or '').strip()
                found.setdefault(title, pg)
    walk(reader.outline)
    texts = [pg.extract_text() or '' for pg in reader.pages]
    # title page index: page before the one containing "All Rights Reserved"
    cr = next(i for i, t in enumerate(texts) if 'All Rights Reserved' in t)
    title_idx = cr - 1
    body_start = None
    for k in keys:
        if k in found and k not in ('LIST OF TABLES', 'LIST OF FIGURES', 'ACKNOWLEDGEMENTS'):
            body_start = found[k]
            break
    page_map = {}
    for k in keys:
        if k not in found:
            page_map[k] = '?'
            continue
        p = found[k]
        if p < body_start:
            page_map[k] = roman(p - title_idx + 1)
        else:
            page_map[k] = str(p - body_start + 1)
    return page_map, found, texts, title_idx, body_start


def verify(pdf, page_map, found, texts, title_idx, body_start):
    """Check that the printed footer number on each referenced page equals the TOC value."""
    bad = []
    for k, label in page_map.items():
        if k not in found:
            bad.append((k, 'not found'))
            continue
        p = found[k]
        lines = [ln.strip() for ln in texts[p].splitlines()]
        if label not in lines:
            bad.append((k, 'expected %s on physical page %d; last lines: %s' % (label, p + 1, lines[-3:])))
    return bad


def main():
    src = os.path.abspath(sys.argv[1])
    outdir = os.path.abspath(sys.argv[2])
    os.makedirs(outdir, exist_ok=True)
    stem = os.path.splitext(os.path.basename(src))[0]
    work = os.path.join(outdir, '_build')
    os.makedirs(work, exist_ok=True)
    fontconfig = os.path.join(work, 'fonts.conf')
    with open(fontconfig, 'w') as f:
        f.write('''<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <dir>/System/Library/Fonts/Supplemental</dir>
  <dir>/System/Library/Fonts</dir>
  <dir>/Library/Fonts</dir>
  <dir>~/Library/Fonts</dir>
  <dir>/Applications/LibreOffice.app/Contents/Resources/fonts/truetype</dir>
  <cachedir>%s</cachedir>
</fontconfig>
''' % os.path.join(work, 'fccache'))
    page_map = {}
    final_pdf = None
    final_docx = None
    for attempt in range(1, 4):
        b = Builder(src, work, page_map)
        docx_out = os.path.join(work, '%s-UGA.docx' % stem)
        b.build(docx_out)
        keys = [k for _, _, _, k in b.entries] + [k for k, _, _ in b.tables] + [k for k, _, _ in b.figures] \
            + ['LIST OF TABLES', 'LIST OF FIGURES'] + [k for _, k in b.front_entries]
        keys = ['ACKNOWLEDGEMENTS'] * ('ACKNOWLEDGEMENTS' in keys) + ['LIST OF TABLES', 'LIST OF FIGURES'] + \
            [k for k in keys if k not in ('LIST OF TABLES', 'LIST OF FIGURES', 'ACKNOWLEDGEMENTS')]
        pdf = to_pdf(docx_out, work, fontconfig)
        new_map, found, texts, title_idx, body_start = page_lookup(pdf, keys)
        bad = verify(pdf, new_map, found, texts, title_idx, body_start)
        print('pass %d: %d keys, %d mismatches; pages=%d' % (attempt, len(new_map), len(bad), len(texts)))
        if attempt > 1 and new_map == page_map and not bad:
            final_pdf, final_docx = pdf, docx_out
            break
        page_map = new_map
        final_pdf, final_docx = pdf, docx_out
    if bad:
        print('WARNING: unresolved page-number mismatches:')
        for k, msg in bad:
            print('  ', k, msg)
    shutil.copy(final_pdf, os.path.join(outdir, '%s-UGA.pdf' % stem))
    shutil.copy(final_docx, os.path.join(outdir, '%s-UGA.docx' % stem))
    with open(os.path.join(work, 'page_map.json'), 'w') as f:
        json.dump(page_map, f, indent=1)
    for n in b.notes:
        print('note:', n)
    print('wrote', os.path.join(outdir, '%s-UGA.pdf' % stem))


if __name__ == '__main__':
    main()
