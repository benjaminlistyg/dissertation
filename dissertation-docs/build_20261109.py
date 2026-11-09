#!/usr/bin/env python3
"""Build dissertation-20261109.docx from dissertation-20261029.docx.

Changes applied (see plan of 2026-11-09):
  1. Figures 1-3 swapped for the regenerated per-item-mean-metric PNGs
     (v2_post_modifications pipeline); the old images were on the 4-20 sum metric
     while every table and all text use the 1-5 per-item mean metric.
  2. New analyses from the Aug-18 defense deck added: Big Five multiverse arms E/F
     (Table A9), EF x Big Five correlations (Table A10), MIDUS 2 -> 3 attrition on EF
     (Table A11, Figure 4), BTACT EF composite reliability (Measures text).
  3. Methods wording aligned with the scoring code (job demands = mean, spillover =
     item mean, range 1-5).
  4. Discussion / Abstract / Chapter 1 implications toned down.
  5. "Practical Implications" and "Conclusion" restyled as DISS_3_SUBSECTION so the
     UGA builder picks them up for the TOC.

Usage:  python3 build_20261109.py            (run from dissertation-docs/)
        python3 build_20261109.py --mark     writes dissertation-20261109-marked.docx with every
                                             inserted or rewritten passage in blue (Figures 1-3 are
                                             image swaps and cannot be coloured; heading restyles
                                             change no text and are left black)
Then:   python3 build_uga_pdf.py dissertation-20261109.docx .
"""
import copy
import csv
import re
import sys
from pathlib import Path

import docx
from docx.shared import Inches
from docx.oxml.ns import qn
from docx.text.paragraph import Paragraph

HERE = Path(__file__).resolve().parent
CODE = HERE.parent / 'dissertation-code'
SRC = HERE / 'dissertation-20261029.docx'
MARK = '--mark' in sys.argv[1:]
OUT = HERE / ('dissertation-20261109-marked.docx' if MARK else 'dissertation-20261109.docx')
BLUE = '0000FF'
# schema order of w:rPr children (subset); w:color must precede spacing/sz/u/lang etc.
RPR_AFTER_COLOR = ('spacing', 'w', 'kern', 'position', 'sz', 'szCs', 'highlight', 'u', 'effect', 'bdr', 'shd',
                   'fitText', 'vertAlign', 'rtl', 'cs', 'em', 'lang', 'eastAsianLayout', 'specVanish', 'oMath')

FIG_DIR = CODE / 'v2_post_modifications' / 'output' / 'figures' / 'interactions'
FIG1 = FIG_DIR / 'not-significant' / 'm5_gender_negfw_m3_simpleslopes.png'  # 03c files gender by factor name -> lands here
FIG2 = FIG_DIR / 'significant' / 'm12_super_poswf_m3_combined.png'
FIG3 = FIG_DIR / 'significant' / 'm14_cowork_negwf_m2_combined.png'
FIG4 = CODE / 'attrition-analysis' / 'attrition_ef_plot.png'
BIG5_CSV = CODE / 'output' / 'tables' / 'multiverse_big5' / 'multiverse_big5_replicability.csv'
CORR_CSV = CODE / 'ef-personality-correlations' / 'ef_big5_correlations.csv'
ATTR_CSV = CODE / 'attrition-analysis' / 'attrition_ef_stats.csv'

W = '{http://schemas.openxmlformats.org/wordprocessingml/2006/main}'


# ----------------------------------------------------------------------------- helpers
def body_items(doc):
    return list(doc.element.body.iterchildren())


def para(doc, el):
    return Paragraph(el, doc._body)


def find_para(doc, pred, start=0):
    """First body paragraph (index, Paragraph) for which pred(text) is true."""
    for i, el in enumerate(body_items(doc)):
        if i < start or el.tag != W + 'p':
            continue
        p = para(doc, el)
        if pred(p.text):
            return i, p
    raise LookupError('paragraph not found')


def starts(s):
    return lambda t: t.strip().startswith(s)


def contains(s):
    return lambda t: s in t


def mark_run(r_el):
    """Colour a w:r element blue (only when MARK). Idempotent, schema-ordered."""
    if not MARK:
        return r_el
    rpr = r_el.find(W + 'rPr')
    if rpr is None:
        rpr = r_el.makeelement(W + 'rPr', {})
        r_el.insert(0, rpr)
    col = rpr.find(W + 'color')
    if col is None:
        col = rpr.makeelement(W + 'color', {})
        idx = len(rpr)
        for i, c in enumerate(rpr):
            if c.tag.split('}')[1] in RPR_AFTER_COLOR:
                idx = i
                break
        rpr.insert(idx, col)
    col.attrib.clear()
    col.set(W + 'val', BLUE)
    return r_el


def _changed_span(old, new):
    """Word-boundary common prefix/suffix of old vs new; returns (head, middle, tail) of `new`
    where `middle` is the text that actually changed. Pure deletions mark all of `new`."""
    ow, nw = old.split(' '), new.split(' ')
    i = 0
    while i < min(len(ow), len(nw)) and ow[i] == nw[i]:
        i += 1
    j = 0
    while j < min(len(ow), len(nw)) - i and ow[-1 - j] == nw[-1 - j]:
        j += 1
    mid = nw[i:len(nw) - j]
    if not mid:
        return '', new, ''
    head = ' '.join(nw[:i]) + (' ' if i else '')
    tail = (' ' if j else '') + ' '.join(nw[len(nw) - j:])
    return head, ' '.join(mid), tail


def _clone_run(r_el, text):
    c = copy.deepcopy(r_el)
    for junk in c.findall(W + 'lastRenderedPageBreak') + c.findall(W + 'tab') + c.findall(W + 'br'):
        c.remove(junk)
    ts = c.findall(W + 't')
    for extra in ts[1:]:
        c.remove(extra)
    t = ts[0] if ts else c.makeelement(W + 't', {})
    if not ts:
        c.append(t)
    t.text = text
    t.set('{http://www.w3.org/XML/1998/namespace}space', 'preserve')
    return c


def replace_in_para(p, old, new, must=True):
    """Replace `old` with `new` inside paragraph p, run-aware (keeps formatting of the
    run where the match begins; text of later runs covered by the match is removed).
    With MARK, the changed words of `new` are split into their own blue run."""
    runs = p.runs
    full = ''.join(r.text for r in runs)
    pos = full.find(old)
    if pos < 0:
        if must:
            raise LookupError(f'text not found: {old[:60]!r}')
        return False
    # map char offsets to runs
    end = pos + len(old)
    acc = 0
    first = None
    for r in runs:
        a, b = acc, acc + len(r.text)
        acc = b
        if b <= pos or a >= end:
            continue
        if first is None:
            first = r
            head = r.text[:pos - a]
            tail = r.text[end - a:] if end <= b else ''
            if MARK:
                h2, mid, t2 = _changed_span(old, new)
                r.text = head + h2
                blue = mark_run(_clone_run(r._r, mid))
                r._r.addnext(blue)
                if t2 + tail:
                    blue.addnext(_clone_run(r._r, t2 + tail))
            else:
                r.text = head + new + tail
        else:
            r.text = r.text[end - a:] if end < b else ''
    return True


def set_para_text(p, text):
    """Keep the first run (its formatting) and replace the whole paragraph text."""
    runs = p.runs
    runs[0].text = text
    for r in runs[1:]:
        r._r.getparent().remove(r._r)


def new_para_after(anchor_el, runs, style=None):
    """Insert a new paragraph after anchor_el. runs: list of (text, {'bold','italic'})
    or a plain string. Returns the new w:p element."""
    p = anchor_el.makeelement(W + 'p', {})
    ppr = p.makeelement(W + 'pPr', {})
    if style is not None:
        ppr.append(ppr.makeelement(W + 'pStyle', {W + 'val': style}))
    else:
        # first-line indent marks a real paragraph start for build_uga_pdf.merge_hard_wraps
        # (the UGA builder re-normalises indents anyway)
        ppr.append(ppr.makeelement(W + 'ind', {W + 'firstLine': '720'}))
    p.append(ppr)
    if isinstance(runs, str):
        runs = [(runs, {})]
    for text, fmt in runs:
        r = p.makeelement(W + 'r', {})
        rpr = r.makeelement(W + 'rPr', {})
        fonts = rpr.makeelement(W + 'rFonts', {W + 'ascii': 'Times New Roman', W + 'hAnsi': 'Times New Roman',
                                                W + 'cs': 'Times New Roman', W + 'eastAsia': 'Times New Roman'})
        rpr.append(fonts)
        if fmt.get('bold'):
            rpr.append(rpr.makeelement(W + 'b', {}))
            rpr.append(rpr.makeelement(W + 'bCs', {}))
        if fmt.get('italic'):
            rpr.append(rpr.makeelement(W + 'i', {}))
            rpr.append(rpr.makeelement(W + 'iCs', {}))
        r.append(rpr)
        t = r.makeelement(W + 't', {'{http://www.w3.org/XML/1998/namespace}space': 'preserve'})
        t.text = text
        r.append(t)
        p.append(mark_run(r))
    anchor_el.addnext(p)
    return p


def rich(text):
    """Turn a string with *italic* markers into run tuples (used for p, r, t, d, M, SD...)."""
    parts = re.split(r'(\*[^*]+\*)', text)
    out = []
    for s in parts:
        if not s:
            continue
        if s.startswith('*') and s.endswith('*'):
            out.append((s[1:-1], {'italic': True}))
        else:
            out.append((s, {}))
    return out


def add_block_after(anchor_el, blocks):
    """blocks: list of (runs_or_text, style). Appends sequentially; returns last element."""
    cur = anchor_el
    for runs, style in blocks:
        cur = new_para_after(cur, runs, style)
    return cur


def fmt_p(p):
    p = float(p)
    if p < .001:
        return '< .001'
    return '= ' + f'{p:.3f}'[1:]


def fmt_p_cell(p):
    p = float(p)
    return '< .001' if p < .001 else f'{p:.3f}'[1:]


def stars(p):
    p = float(p)
    return '***' if p < .001 else '**' if p < .01 else '*' if p < .05 else ''


def fmt_r(x):
    v = round(float(x), 2) + 0.0  # normalise negative zero
    s = f'{v:.2f}'
    return s.replace('0.', '.').replace('-.', '−.') if abs(v) < 1 else s


def fmt_b(x):
    v = round(float(x), 2) + 0.0
    return f'{v:.2f}'.replace('-', '−')


def fmt_ci(lo, hi):
    return f'[{fmt_b(lo)}, {fmt_b(hi)}]'


# ----------------------------------------------------------------------------- table builder
class TableTemplate:
    """Clone cells/rows from Table A1 so new appendix tables share its APA formatting."""

    def __init__(self, tbl):
        trs = tbl.findall(W + 'tr')
        self.tbl = tbl
        self.header_cell = trs[0].findall(W + 'tc')[0]        # bordered top/bottom, centred
        body = trs[1].findall(W + 'tc')
        self.body_left = body[0]                                # left aligned, no borders
        self.body_center = body[1]                              # centred, no borders
        self.last_cell = trs[-1].findall(W + 'tc')[-1]          # bottom border
        self.tr = trs[1]

    def _cell(self, template, text, width, italic=False, bold=False, span=None, bottom=False, align=None):
        tc = copy.deepcopy(template)
        tcpr = tc.find(W + 'tcPr')
        tcpr.find(W + 'tcW').set(W + 'w', str(width))
        gs = tcpr.find(W + 'gridSpan')
        if gs is not None:
            tcpr.remove(gs)
        if span and span > 1:
            gs = tcpr.makeelement(W + 'gridSpan', {W + 'val': str(span)})
            tcpr.insert(1, gs)
        if bottom:
            b = tcpr.find(W + 'tcBorders').find(W + 'bottom')
            b.set(W + 'val', 'single'); b.set(W + 'sz', '6'); b.set(W + 'color', '000000')
        # single paragraph, single run
        ps = tc.findall(W + 'p')
        for extra in ps[1:]:
            tc.remove(extra)
        p = ps[0]
        if align is not None:
            ppr = p.find(W + 'pPr')
            jc = ppr.find(W + 'jc')
            if align == 'left' and jc is not None:
                ppr.remove(jc)
            elif align != 'left':
                if jc is None:
                    jc = ppr.makeelement(W + 'jc', {})
                    ppr.append(jc)
                jc.set(W + 'val', align)
        runs = p.findall(W + 'r')
        for extra in runs[1:]:
            p.remove(extra)
        for junk in p.findall(W + 'proofErr'):
            p.remove(junk)
        r = runs[0]
        rpr = r.find(W + 'rPr')
        for tag in ('i', 'iCs', 'b', 'bCs'):
            e = rpr.find(W + tag)
            if e is not None:
                rpr.remove(e)
        if italic:
            rpr.insert(1, rpr.makeelement(W + 'i', {})); rpr.insert(2, rpr.makeelement(W + 'iCs', {}))
        if bold:
            rpr.insert(1, rpr.makeelement(W + 'b', {})); rpr.insert(2, rpr.makeelement(W + 'bCs', {}))
        for junk in r.findall(W + 'lastRenderedPageBreak'):
            r.remove(junk)
        t = r.find(W + 't')
        t.text = text
        t.set('{http://www.w3.org/XML/1998/namespace}space', 'preserve')
        mark_run(r)
        return tc

    def build(self, widths, header, rows, *, left_cols=(0,), summary_rows=()):
        """rows: list of lists of str. summary_rows: list of (label, values_by_col_index{}, span)
        rendered as italic spanning rows at the bottom (like Table A1's Step 1-3 rows)."""
        tbl = copy.deepcopy(self.tbl)
        for tr in tbl.findall(W + 'tr'):
            tbl.remove(tr)
        tbl.find(W + 'tblPr').find(W + 'tblW').set(W + 'w', str(sum(widths)))
        grid = tbl.find(W + 'tblGrid')
        for g in list(grid):
            grid.remove(g)
        for wd in widths:
            grid.append(grid.makeelement(W + 'gridCol', {W + 'w': str(wd)}))

        def new_tr():
            tr = copy.deepcopy(self.tr)
            for tc in tr.findall(W + 'tc'):
                tr.remove(tc)
            return tr

        tr = new_tr()
        for j, h in enumerate(header):
            tr.append(self._cell(self.header_cell, h, widths[j]))
        tbl.append(tr)
        n_rows = len(rows) + len(summary_rows)
        for i, row in enumerate(rows):
            tr = new_tr()
            last = (i == n_rows - 1)
            for j, val in enumerate(row):
                tmpl = self.body_left if j in left_cols else self.body_center
                tr.append(self._cell(tmpl, val, widths[j], bottom=last))
            tbl.append(tr)
        for k, (label, vals, span) in enumerate(summary_rows):
            tr = new_tr()
            last = (k == len(summary_rows) - 1)
            tr.append(self._cell(self.body_left, label, sum(widths[:span]), italic=True, span=span, bottom=last))
            for j in range(span, len(widths)):
                tr.append(self._cell(self.body_center, vals.get(j, ''), widths[j], bottom=last))
            tbl.append(tr)
        return tbl


# ----------------------------------------------------------------------------- data
def load_big5():
    rows = list(csv.DictReader(open(BIG5_CSV)))
    assert len(rows) == 72, len(rows)
    return rows


def load_corr():
    return list(csv.DictReader(open(CORR_CSV)))


def load_attr():
    return list(csv.DictReader(open(ATTR_CSV)))


OUTCOME_ABBR = {'NEGWF': 'NWF', 'NEGFW': 'NFW', 'POSWF': 'PWF', 'POSFW': 'PFW'}
SET_LABEL = {'Main': 'main effects', 'Gender': 'EF × Gender moderation', 'Supervisor': 'EF × Supervisor support moderation',
             'Coworker': 'EF × Coworker support moderation', 'Spouse': 'EF × Spousal support moderation'}
ARM_SHORT = {'Arm E: Full controls + Big Five': 'Arm E (full covariates + Big Five)',
             'Arm F: EF + Big Five only': 'Arm F (EF + Big Five only)'}


def term(r):
    return r['term_label'].replace(' x ', ' × ')


def big5_table_rows(rows):
    out = []
    for r in rows:
        sig1 = r['sig_w1'] == 'TRUE'
        sig2 = r['sig_w2'] == 'TRUE'
        same = r['same_sign'] == 'TRUE'
        both = r['replicated_sig'] == 'TRUE'
        ci = r['ci_overlap'] == 'TRUE'
        if not sig1:
            m = ('—', '—', '—')
        else:
            m = ('✓' if same else '✗', '✓' if both else '✗', ('✓' if ci else '✗') if both else '—')
        spec = f"{ARM_SHORT[r['arm']]}: {SET_LABEL[r['model_set']]}"
        out.append([spec, OUTCOME_ABBR[r['outcome']], term(r),
                    f"{fmt_b(r['est_w1'])} {fmt_ci(r['ci_lo_w1'], r['ci_hi_w1'])}{stars(r['p_w1'])}",
                    f"{fmt_b(r['est_w2'])} {fmt_ci(r['ci_lo_w2'], r['ci_hi_w2'])}{stars(r['p_w2'])}",
                    *m])
    return out


def big5_summary(rows, arm):
    R = [r for r in rows if r['arm'] == arm]
    s1 = [r for r in R if r['sig_w1'] == 'TRUE']
    same = [r for r in s1 if r['same_sign'] == 'TRUE']
    both = [r for r in s1 if r['replicated_sig'] == 'TRUE']
    ci = [r for r in both if r['ci_overlap'] == 'TRUE']
    short = ARM_SHORT[arm].split(' (')[0]

    def pct(a, b):
        return f'{a}/{b} ({100 * a / b:.1f}%)' if b else f'{a}/{b} (—)'
    return [
        (f'{short} — Step 1: Same sign (of {len(s1)} sig in MIDUS 2)', {5: pct(len(same), len(s1))}, 5),
        (f'{short} — Step 2: Same sign + significant (of {len(s1)} sig in MIDUS 2)', {6: pct(len(both), len(s1))}, 5),
        (f'{short} — Step 3: Overlapping CIs (of {len(both)} sig in both)', {7: pct(len(ci), len(both))}, 5),
    ], dict(n=len(R), s1=len(s1), s2=sum(r['sig_w2'] == 'TRUE' for r in R), same=len(same), both=len(both), ci=len(ci))


# ----------------------------------------------------------------------------- main
def main():
    for f in (SRC, FIG1, FIG2, FIG3, FIG4, BIG5_CSV, CORR_CSV, ATTR_CSV):
        assert f.exists(), f
    doc = docx.Document(str(SRC))
    big5 = load_big5()
    corr = load_corr()
    attr = load_attr()
    A = {r['comparison']: r for r in attr}
    full = A['Retained at M3 vs Dropped out']
    left = A['Retained at M3 vs Left the study']
    filt = A['Retained at M3 vs In M3, filtered out']
    C = {(r['Wave'], r['Trait']): r for r in corr}
    sumE, nE = big5_summary(big5, 'Arm E: Full controls + Big Five')
    sumF, nF = big5_summary(big5, 'Arm F: EF + Big Five only')
    armF_main_negwf = next(r for r in big5 if r['arm'].startswith('Arm F') and r['model_set'] == 'Main' and r['outcome'] == 'NEGWF')
    armF_main_negfw = next(r for r in big5 if r['arm'].startswith('Arm F') and r['model_set'] == 'Main' and r['outcome'] == 'NEGFW')
    print(f'Arm E: {nE}\nArm F: {nF}')

    # ---------------------------------------------------------------- 1. swap Figures 1-3
    for label, png, width in (('Figure 1', FIG1, 5.0), ('Figure 2', FIG2, 9.0), ('Figure 3', FIG3, 9.0)):
        i, cap = find_para(doc, lambda t, L=label: t.strip() == L)
        items = body_items(doc)
        j = i + 1
        while not items[j].findall('.//' + W + 'drawing'):
            j += 1
        p = para(doc, items[j])
        for r in list(p.runs):
            r._r.getparent().remove(r._r)
        p.add_run().add_picture(str(png), width=Inches(width))
        print(f'{label}: image replaced from {png.name}')

    # ---------------------------------------------------------------- 2. Methods: measures
    _, p = find_para(doc, starts('In the present study, executive functioning was operationalized'))
    replace_in_para(p, 'indicating strong reliability and temporal stability of the measure.',
                    'indicating strong reliability and temporal stability of the measure. In the present analytic samples, '
                    'the internal consistency of the five-indicator executive functioning composite, estimated from a '
                    'one-factor model, was α = .74 (ω = .77) in MIDUS 2 and α = .75 (ω = .76) in MIDUS 3.')

    _, p = find_para(doc, starts('Spillover. Positive and negative family-to-work'))
    replace_in_para(p, 'Reliability values for the four scales in the analytic samples ranged from 0.61 to 0.84.',
                    'Items were scored so that higher values reflect more frequent spillover, and each subscale score was '
                    'computed as the mean of its four items (possible range 1–5). Reliability values for the four scales in '
                    'the analytic samples ranged from 0.61 to 0.84.')

    _, p = find_para(doc, starts('Job Demands. Job demands were included'))
    replace_in_para(p, 'The composite score was constructed by calculating the sum of the reverse-coded values of the items in the scale.',
                    'The composite score was constructed by calculating the mean of the reverse-coded values of the items in '
                    'the scale, so that higher scores reflect greater job demands.')

    # ---------------------------------------------------------------- 2b. Methods: supplemental analyses
    _, p = find_para(doc, starts('Multiverse analysis. In addition to the main analyses'))
    replace_in_para(p, 'Four arms were examined in the multiverse analysis:', 'Six arms were examined in the multiverse analysis:')
    replace_in_para(p, 're-running all 20 regression models per wave as a robustness check; and (d)',
                    're-running all 20 regression models per wave as a robustness check; (d)')
    replace_in_para(p, 'complete data on every variable used across all model sets. Multiverse results are summarized',
                    'complete data on every variable used across all model sets; (e) personality-adjusted specifications, in '
                    'which the Big Five personality traits (neuroticism, extraversion, openness to experience, '
                    'conscientiousness, and agreeableness) were added as covariates to each of the 20 primary models; and '
                    '(f) personality-only specifications, in which each spillover outcome was regressed on executive '
                    'functioning and the Big Five traits alone (retaining the focal moderator and its interaction with '
                    'executive functioning in the moderation models), to examine whether an executive functioning effect '
                    'emerges when stable dispositional differences, rather than situational demands and demographic '
                    'characteristics, are held constant. The Big Five traits were measured with the Midlife Development '
                    'Inventory (MIDI) personality adjective scales (Lachman & Weaver, 1997), in which respondents rate how '
                    'well each of a set of trait adjectives describes them on a scale from 1 (a lot) to 4 (not at all); the '
                    'MIDUS pre-scored trait means, coded so that higher scores reflect more of each trait, were used. '
                    'Multiverse results are summarized')

    i, p = find_para(doc, starts('Cross-wave replicability. Following the criteria'))
    add_block_after(p._p, [
        ([('Executive functioning–personality correlations. ', {'bold': True})] + rich(
            'To characterize the overlap between executive functioning and dispositional traits, zero-order Pearson '
            'correlations between the BTACT executive functioning composite and each Big Five trait were computed within '
            'each wave’s regression sample (see Appendix, Table A10).'), None),
        ([('Attrition analysis. ', {'bold': True})] + rich('Because MIDUS 3 retains only a subset of MIDUS 2 respondents, a supplementary analysis '
              'examined whether attrition between waves was selective with respect to executive functioning. MIDUS 2 '
              'executive functioning scores were compared, using Welch’s *t* tests and Cohen’s *d*, between respondents '
              'who did and did not provide a MIDUS 3 executive functioning score, both in the full MIDUS 2 BTACT sample and '
              'in the MIDUS 2 analytic sample. Within the analytic sample, non-retained respondents were further divided '
              'into those with no MIDUS 3 survey record and those who completed MIDUS 3 but did not meet the MIDUS 3 '
              'analytic inclusion criteria (see Appendix, Table A11 and Figure 4).'), None),
    ])

    # ---------------------------------------------------------------- 3. Results additions
    _, p = find_para(doc, contains('To formally assess replicability, the Open Science Collaboration (2015) criteria were applied'))
    results_blocks = [
        (rich(
            f'Two additional multiverse arms incorporated the Big Five personality traits (Table A9). When the Big Five '
            f'were added as covariates to the full primary models (Arm E), executive functioning–related terms reached '
            f'significance for {nE["s1"]} of {nE["n"]} terms in MIDUS 2 and {nE["s2"]} of {nE["n"]} in MIDUS 3, and no term was '
            f'significant in both waves; the conclusions of the primary models were unchanged. When executive functioning '
            f'and the Big Five were the only predictors (Arm F), executive functioning was positively related to negative '
            f'work-to-family spillover in both waves (main-effects model: MIDUS 2 *B* = {fmt_b(armF_main_negwf["est_w1"])}, '
            f'*p* {fmt_p(armF_main_negwf["p_w1"])}; MIDUS 3 *B* = {fmt_b(armF_main_negwf["est_w2"])}, *p* {fmt_p(armF_main_negwf["p_w2"])}), '
            f'and this effect met all three replicability criteria in the main-effects, supervisor support, coworker support, '
            f'and spousal support model sets ({nF["both"]} of {nF["n"]} terms). '
            f'Executive functioning was also positively related to negative family-to-work spillover in MIDUS 2 '
            f'(*B* = {fmt_b(armF_main_negfw["est_w1"])}, *p* {fmt_p(armF_main_negfw["p_w1"])}) but not in MIDUS 3 '
            f'(*B* = {fmt_b(armF_main_negfw["est_w2"])}, *p* {fmt_p(armF_main_negfw["p_w2"])}). As in the no-covariate '
            f'specifications of Arm A, these positive associations were attenuated to nonsignificance once job demands, family '
            f'demands, and the demographic covariates were included, indicating that adjusting for personality alone does '
            f'not alter the primary conclusions.'), None),
        (rich(
            f'Zero-order correlations between executive functioning and the Big Five traits were small in both waves '
            f'(Table A10; |*r*| ≤ {fmt_r(max(abs(float(r["r"])) for r in corr))}). Executive functioning was positively related to '
            f'openness to experience in MIDUS 2 (*r* = {fmt_r(C[("MIDUS 2", "Openness")]["r"])}, *p* {fmt_p(C[("MIDUS 2", "Openness")]["p"])}) '
            f'and MIDUS 3 (*r* = {fmt_r(C[("MIDUS 3", "Openness")]["r"])}, *p* {fmt_p(C[("MIDUS 3", "Openness")]["p"])}), and weakly '
            f'negatively related to extraversion (*r* = {fmt_r(C[("MIDUS 2", "Extraversion")]["r"])}, *p* {fmt_p(C[("MIDUS 2", "Extraversion")]["p"])}) '
            f'and agreeableness (*r* = {fmt_r(C[("MIDUS 2", "Agreeableness")]["r"])}, *p* {fmt_p(C[("MIDUS 2", "Agreeableness")]["p"])}) in '
            f'MIDUS 2 only. Correlations with neuroticism and conscientiousness did not reach significance in either wave.'), None),
        (rich(
            f'Attrition between MIDUS 2 and MIDUS 3 was selective with respect to executive functioning (Table A11; Figure 4). '
            f'In the full MIDUS 2 BTACT sample, respondents who provided a MIDUS 3 executive functioning score had higher '
            f'MIDUS 2 executive functioning (*M* = {float(full["m_retained"]):.2f}, *SD* = {float(full["sd_retained"]):.2f}; '
            f'*n* = {int(full["n_retained"]):,}) than those who did not (*M* = {fmt_b(full["m_group"])}, *SD* = {float(full["sd_group"]):.2f}; '
            f'*n* = {int(full["n_group"]):,}), *t*({float(full["df"]):.1f}) = {float(full["t"]):.2f}, *p* {fmt_p(full["p"])}, '
            f'*d* = {float(full["d"]):.2f}. Within the MIDUS 2 analytic sample, respondents retained in the MIDUS 3 analytic '
            f'sample (*M* = {float(left["m_retained"]):.2f}, *SD* = {float(left["sd_retained"]):.2f}; *n* = {int(left["n_retained"])}) '
            f'scored higher than both those who left the study (*M* = {float(left["m_group"]):.2f}, *SD* = {float(left["sd_group"]):.2f}; '
            f'*n* = {int(left["n_group"])}), *t*({float(left["df"]):.1f}) = {float(left["t"]):.2f}, *p* {fmt_p(left["p"])}, '
            f'*d* = {float(left["d"]):.2f}, and those who completed MIDUS 3 but did not meet the MIDUS 3 inclusion criteria '
            f'(*M* = {float(filt["m_group"]):.2f}, *SD* = {float(filt["sd_group"]):.2f}; *n* = {int(filt["n_group"])}), '
            f'*t*({float(filt["df"]):.1f}) = {float(filt["t"]):.2f}, *p* {fmt_p(filt["p"])}, *d* = {float(filt["d"]):.2f}. '
            f'Lower-functioning respondents were thus less likely to remain in the study and less likely to satisfy the '
            f'employment and complete-data criteria at the later wave, further restricting the range of executive '
            f'functioning in the MIDUS 3 analytic sample.'), None),
    ]
    add_block_after(p._p, results_blocks)

    # ---------------------------------------------------------------- 4. Figure 4 (after Figure 3's note, before APPENDIX)
    i, cap3 = find_para(doc, lambda t: t.strip() == 'Figure 3')
    items = body_items(doc)
    j = i + 1
    while not (items[j].tag == W + 'p' and para(doc, items[j]).text.strip().startswith('Note.')):
        j += 1
    note3 = items[j]
    f4_cap = new_para_after(note3, [('Figure 4', {'bold': True})])
    f4_title = new_para_after(f4_cap, [('Distribution of MIDUS 2 Executive Functioning by MIDUS 3 Retention Status', {'italic': True})])
    f4_img = new_para_after(f4_title, '')
    para(doc, f4_img).add_run().add_picture(str(FIG4), width=Inches(9.0))
    new_para_after(f4_img, [('Note. ', {'italic': True})] + rich(
        f'Kernel density estimates of MIDUS 2 executive functioning (BTACT composite *z*-score) by MIDUS 3 retention status. '
        f'Panel A: full MIDUS 2 BTACT sample (retained, *n* = {int(full["n_retained"]):,}; dropped out, *n* = {int(full["n_group"]):,}). '
        f'Panel B: MIDUS 2 analytic sample (retained in the MIDUS 3 analytic sample, *n* = {int(left["n_retained"])}; completed '
        f'MIDUS 3 but excluded by the MIDUS 3 inclusion criteria, *n* = {int(filt["n_group"])}; no MIDUS 3 record, '
        f'*n* = {int(left["n_group"])}). Vertical lines mark group means; brackets give Cohen’s *d* and the Welch *t*-test '
        f'*p* value for each comparison with the retained group (Table A11).'))

    # ---------------------------------------------------------------- 5. Appendix prose
    # 5a. Multiverse: Arms E/F paragraph after the cross-wave replicability paragraph
    _, p = find_para(doc, starts('Cross-wave replicability of the multiverse results (Table A1)'))
    armE = [r for r in big5 if r['arm'].startswith('Arm E')]
    e_sig1 = [r for r in armE if r['sig_w1'] == 'TRUE']
    e_sig2 = [r for r in armE if r['sig_w2'] == 'TRUE']
    armF = [r for r in big5 if r['arm'].startswith('Arm F')]
    f_negwf = [r for r in armF if r['outcome'] == 'NEGWF' and r['term_label'] == 'EF' and r['replicated_sig'] == 'TRUE']
    f_negfw = [r for r in armF if r['outcome'] == 'NEGFW' and r['term_label'] == 'EF' and r['sig_w1'] == 'TRUE']
    f_int3 = [r for r in armF if r['term_label'] != 'EF' and r['sig_w2'] == 'TRUE' and r['sig_w1'] != 'TRUE']

    def desc(r, wave):
        return f'*B* = {fmt_b(r["est_w" + wave])}, *p* {fmt_p(r["p_w" + wave])}'

    def rng(vals, fmt='{:.2f}'):
        lo, hi = min(vals), max(vals)
        a, b = fmt.format(lo), fmt.format(hi)
        return a if a == b else f'{a}–{b}'

    OUT_LONG = {'NEGWF': 'negative work-to-family spillover', 'NEGFW': 'negative family-to-work spillover',
                'POSWF': 'positive work-to-family spillover', 'POSFW': 'positive family-to-work spillover'}
    e1_txt = '; '.join(f'{term(r)} for {OUT_LONG[r["outcome"]]} in the {SET_LABEL[r["model_set"]]} model, {desc(r, "1")}' for r in e_sig1)
    e2_txt = '; '.join(f'{term(r)} for {OUT_LONG[r["outcome"]]} in the {SET_LABEL[r["model_set"]]} model, {desc(r, "2")}' for r in e_sig2)
    int3_txt = '; '.join(f'{term(r)} for {OUT_LONG[r["outcome"]]}' for r in f_int3)
    f_pmax1 = max(float(r['p_w1']) for r in f_negwf)
    f_pmax2 = max(float(r['p_w2']) for r in f_negwf)
    negfw_p2min = min(float(r['p_w2']) for r in f_negfw)
    negfw_p1max = max(float(r['p_w1']) for r in f_negfw)
    add_block_after(p._p, [
        (rich(
            f'Two further arms examined the role of Big Five personality (Table A9). In Arm E, the five MIDI trait scores '
            f'were added as covariates to each of the 20 primary models. Executive functioning–related terms were significant '
            f'for {len(e_sig1)} of {len(armE)} terms in MIDUS 2 ({e1_txt}) and {len(e_sig2)} of {len(armE)} in MIDUS 3 ({e2_txt}), '
            f'mirroring the isolated, non-replicating effects of the primary models; no term was significant in both waves. '
            f'In Arm F, each outcome was regressed on executive functioning and the Big Five alone (plus the focal moderator '
            f'and its interaction in the moderation models). Executive functioning was positively related to negative '
            f'work-to-family spillover in both waves across the main-effects, supervisor, coworker, and spousal model sets '
            f'(MIDUS 2 *B* = {rng([float(r["est_w1"]) for r in f_negwf])}, *p* ≤ {f"{f_pmax1:.3f}"[1:]}; '
            f'MIDUS 3 *B* = {rng([float(r["est_w2"]) for r in f_negwf])}, *p* ≤ {f"{f_pmax2:.3f}"[1:]}), and all '
            f'{len(f_negwf)} of these terms met the three OSC criteria. Executive functioning was also positively related to '
            f'negative family-to-work spillover in MIDUS 2 (*B* = {rng([float(r["est_w1"]) for r in f_negfw])}, '
            f'*p* ≤ {f"{negfw_p1max:.3f}"[1:]}) but not in MIDUS 3 (*B* = {rng([float(r["est_w2"]) for r in f_negfw])}, '
            f'*p* ≥ {f"{negfw_p2min:.3f}"[1:]}). {len(f_int3)} interaction terms reached significance in MIDUS 3 only '
            f'({int3_txt}), none of which was significant in MIDUS 2. Of the {nF["s1"]} Arm F terms significant in MIDUS 2, '
            f'{nF["same"]} had the same sign in MIDUS 3 ({100 * nF["same"] / nF["s1"]:.1f}%), {nF["both"]} were also significant in '
            f'MIDUS 3 ({100 * nF["both"] / nF["s1"]:.1f}%), and {nF["ci"]} of those {nF["both"]} had overlapping confidence '
            f'intervals ({100 * nF["ci"] / nF["both"]:.1f}%). The Arm F pattern parallels the no-covariate specifications of '
            f'Arm A: the positive zero-order association between executive functioning and negative spillover persists when '
            f'only personality is controlled, but is attenuated to nonsignificance once job demands, family demands, and the '
            f'demographic covariates are included (Arm E and the primary models).'), None),
    ])

    # 5b. new appendix sections before Table A1 caption (after the last CFA/invariance paragraph)
    i_a1, _ = find_para(doc, lambda t: t.strip() == 'Table A1')
    items = body_items(doc)
    k = i_a1 - 1
    while not (items[k].tag == W + 'p' and para(doc, items[k]).text.strip()):
        k -= 1
    last_prose = items[k]
    o2, o3 = C[('MIDUS 2', 'Openness')], C[('MIDUS 3', 'Openness')]
    ex2, ex3 = C[('MIDUS 2', 'Extraversion')], C[('MIDUS 3', 'Extraversion')]
    ag2, ag3 = C[('MIDUS 2', 'Agreeableness')], C[('MIDUS 3', 'Agreeableness')]
    ne2, ne3 = C[('MIDUS 2', 'Neuroticism')], C[('MIDUS 3', 'Neuroticism')]
    co2, co3 = C[('MIDUS 2', 'Conscientiousness')], C[('MIDUS 3', 'Conscientiousness')]
    add_block_after(last_prose, [
        ([('Executive Functioning and Big Five Personality', {})], 'DISS4APPENDIXTITLE'),
        (rich(
            f'Zero-order Pearson correlations between the BTACT executive functioning composite and the five MIDI personality '
            f'trait scores were computed in each wave’s regression sample (Table A10). All correlations were small '
            f'(|*r*| ≤ {fmt_r(max(abs(float(r["r"])) for r in corr))}). Openness to experience was the only trait related to '
            f'executive functioning in both waves (MIDUS 2: *r* = {fmt_r(o2["r"])}, 95% CI [{fmt_r(o2["ci_lower"])}, {fmt_r(o2["ci_upper"])}], '
            f'*p* {fmt_p(o2["p"])}; MIDUS 3: *r* = {fmt_r(o3["r"])}, 95% CI [{fmt_r(o3["ci_lower"])}, {fmt_r(o3["ci_upper"])}], '
            f'*p* {fmt_p(o3["p"])}). In MIDUS 2, executive functioning was also weakly and negatively related to extraversion '
            f'(*r* = {fmt_r(ex2["r"])}, *p* {fmt_p(ex2["p"])}) and agreeableness (*r* = {fmt_r(ag2["r"])}, *p* {fmt_p(ag2["p"])}); '
            f'these associations did not reach significance in MIDUS 3 (*r* = {fmt_r(ex3["r"])}, *p* {fmt_p(ex3["p"])}, and '
            f'*r* = {fmt_r(ag3["r"])}, *p* {fmt_p(ag3["p"])}, respectively). Correlations with neuroticism '
            f'(*r* = {fmt_r(ne2["r"])} and {fmt_r(ne3["r"])}) and conscientiousness (*r* = {fmt_r(co2["r"])} and {fmt_r(co3["r"])}) '
            f'were nonsignificant in both waves. The small overlap between executive functioning and personality suggests '
            f'that the Big Five covariates in Arms E and F removed little executive functioning variance, so the attenuation '
            f'of the Arm F effects in Arm E is more plausibly attributable to the demand and demographic covariates than to '
            f'personality.'), None),
        ([('Attrition Analysis', {})], 'DISS4APPENDIXTITLE'),
        (rich(
            f'To examine whether attrition between MIDUS 2 and MIDUS 3 was selective with respect to executive functioning, '
            f'MIDUS 2 BTACT executive functioning composite scores were compared between respondents who did and did not '
            f'provide a MIDUS 3 executive functioning score (Table A11; Figure 4). Two samples were examined. The first was '
            f'the full MIDUS 2 BTACT sample (*n* = {int(full["n_retained"]) + int(full["n_group"]):,} respondents with an executive '
            f'functioning score), for which non-retention reflects attrition from the cognitive battery itself. The second was '
            f'the MIDUS 2 analytic sample (*N* = 1,289) used in the primary analyses; within this sample, non-retained '
            f'respondents were separated into those with no MIDUS 3 survey record (“left the study”) and those who completed '
            f'MIDUS 3 but did not meet the MIDUS 3 analytic inclusion criteria of current employment and complete data '
            f'(“in MIDUS 3, filtered out”). Group differences were tested with Welch’s *t* tests and summarized with Cohen’s *d*.'), None),
        (rich(
            f'In the full BTACT sample, respondents retained at MIDUS 3 (*n* = {int(full["n_retained"]):,}) had higher MIDUS 2 '
            f'executive functioning (*M* = {float(full["m_retained"]):.2f}, *SD* = {float(full["sd_retained"]):.2f}) than those who '
            f'dropped out (*n* = {int(full["n_group"]):,}; *M* = {fmt_b(full["m_group"])}, *SD* = {float(full["sd_group"]):.2f}), '
            f'*t*({float(full["df"]):.1f}) = {float(full["t"]):.2f}, *p* {fmt_p(full["p"])}, *d* = {float(full["d"]):.2f}. In the '
            f'analytic sample, the {int(left["n_retained"])} respondents retained in the MIDUS 3 analytic sample '
            f'(*M* = {float(left["m_retained"]):.2f}, *SD* = {float(left["sd_retained"]):.2f}) scored higher than the '
            f'{int(left["n_group"])} who left the study (*M* = {float(left["m_group"]):.2f}, *SD* = {float(left["sd_group"]):.2f}), '
            f'*t*({float(left["df"]):.1f}) = {float(left["t"]):.2f}, *p* {fmt_p(left["p"])}, *d* = {float(left["d"]):.2f}, and than '
            f'the {int(filt["n_group"])} who completed MIDUS 3 but were excluded by the MIDUS 3 inclusion criteria '
            f'(*M* = {float(filt["m_group"]):.2f}, *SD* = {float(filt["sd_group"]):.2f}), *t*({float(filt["df"]):.1f}) = '
            f'{float(filt["t"]):.2f}, *p* {fmt_p(filt["p"])}, *d* = {float(filt["d"]):.2f}. Attrition and the analytic inclusion '
            f'criteria therefore both selected on executive functioning, with medium-sized differences between retained and '
            f'non-retained respondents, which is consistent with the range restriction discussed in the Limitations.'), None),
    ])

    # ---------------------------------------------------------------- 6. Appendix tables A9-A11 (after Table A8's note)
    i_a8, _ = find_para(doc, lambda t: t.strip() == 'Table A8')
    items = body_items(doc)
    j = i_a8 + 1
    while items[j].tag != W + 'tbl':
        j += 1
    j += 1
    while items[j].tag == W + 'p' and para(doc, items[j]).text.strip():
        j += 1
    anchor = items[j - 1]
    i_a1, _ = find_para(doc, lambda t: t.strip() == 'Table A1')
    items = body_items(doc)
    t_a1 = next(items[k] for k in range(i_a1, len(items)) if items[k].tag == W + 'tbl')
    tmpl = TableTemplate(t_a1)

    # A9
    a9_rows = big5_table_rows(big5)
    a9 = tmpl.build([2600, 900, 1900, 2100, 2100, 1060, 900, 1400],
                    ['Specification', 'Outcome', 'Term', 'MIDUS 2 B [95% CI]', 'MIDUS 3 B [95% CI]', 'Sign', 'Sig', 'CI'],
                    a9_rows, left_cols=(0,), summary_rows=sumE + sumF)
    cap = new_para_after(anchor, [('Table A9', {'bold': True})])
    ttl = new_para_after(cap, [('Multiverse Arms E and F: Executive Functioning Effects With Big Five Personality Controls', {'italic': True})])
    ttl.addnext(a9)
    n1 = new_para_after(a9, [('Note. ', {'italic': True})] + rich(
        'Arm E adds the five MIDI Big Five trait scores as covariates to the full primary models; Arm F regresses each '
        'outcome on executive functioning and the Big Five only (retaining the focal moderator and its interaction in the '
        'moderation models). Outcome acronyms: NWF = negative work-to-family spillover; NFW = negative family-to-work '
        'spillover; PWF = positive work-to-family spillover; PFW = positive family-to-work spillover. EF = executive '
        'functioning. Replicability assessed using the Open Science Collaboration (2015) criteria. Step 1: percentage of '
        'MIDUS 2-significant effects with the same sign in MIDUS 3. Step 2: percentage of MIDUS 2-significant effects that '
        'are also significant and same sign in MIDUS 3. Step 3: percentage of effects significant in both waves with '
        'overlapping 95% CIs. ✓ = criterion met. ✗ = criterion not met. — = not applicable. MIDUS 2 *N* = 1,277; MIDUS 3 '
        '*N* = 727 (complete cases on the Big Five).'))
    n2 = new_para_after(n1, [('*', {}), ('p', {'italic': True}), (' < .05. **', {}), ('p', {'italic': True}),
                             (' < .01. ***', {}), ('p', {'italic': True}), (' < .001.', {})])

    # A10
    traits = ['Neuroticism', 'Extraversion', 'Openness to experience', 'Conscientiousness', 'Agreeableness']
    key = {'Openness to experience': 'Openness'}
    a10_rows = []
    for tr in traits:
        r2, r3 = C[('MIDUS 2', key.get(tr, tr))], C[('MIDUS 3', key.get(tr, tr))]
        a10_rows.append([tr, r2['n'], f"{fmt_r(r2['r'])} [{fmt_r(r2['ci_lower'])}, {fmt_r(r2['ci_upper'])}]", fmt_p_cell(r2['p']),
                         r3['n'], f"{fmt_r(r3['r'])} [{fmt_r(r3['ci_lower'])}, {fmt_r(r3['ci_upper'])}]", fmt_p_cell(r3['p'])])
    a10 = tmpl.build([2600, 900, 2300, 1000, 900, 2300, 1000],
                     ['Trait', 'MIDUS 2 n', 'MIDUS 2 r [95% CI]', 'p', 'MIDUS 3 n', 'MIDUS 3 r [95% CI]', 'p'],
                     a10_rows, left_cols=(0,))
    cap = new_para_after(n2, [('Table A10', {'bold': True})])
    ttl = new_para_after(cap, [('Zero-Order Correlations Between Executive Functioning and Big Five Personality Traits', {'italic': True})])
    ttl.addnext(a10)
    n3 = new_para_after(a10, [('Note. ', {'italic': True})] + rich(
        'Pearson correlations between the BTACT executive functioning composite and the MIDUS pre-scored MIDI trait means '
        '(Lachman & Weaver, 1997), computed within each wave’s regression sample on pairwise-complete cases. 95% CI = '
        'Fisher *z* confidence interval.'))

    # A11
    def ms(m, sd):
        return f'{fmt_b(m)} ({float(sd):.2f})'
    a11_rows = [
        ['Full MIDUS 2 BTACT sample', 'Dropped out before MIDUS 3', f'{int(full["n_retained"]):,}', f'{int(full["n_group"]):,}',
         ms(full['m_retained'], full['sd_retained']), ms(full['m_group'], full['sd_group']),
         f'{float(full["t"]):.2f}', f'{float(full["df"]):.1f}', fmt_p_cell(full['p']), f'{float(full["d"]):.2f}'],
        ['MIDUS 2 analytic sample', 'Left the study (no MIDUS 3 record)', f'{int(left["n_retained"])}', f'{int(left["n_group"])}',
         ms(left['m_retained'], left['sd_retained']), ms(left['m_group'], left['sd_group']),
         f'{float(left["t"]):.2f}', f'{float(left["df"]):.1f}', fmt_p_cell(left['p']), f'{float(left["d"]):.2f}'],
        ['MIDUS 2 analytic sample', 'In MIDUS 3, excluded by inclusion criteria', f'{int(filt["n_retained"])}', f'{int(filt["n_group"])}',
         ms(filt['m_retained'], filt['sd_retained']), ms(filt['m_group'], filt['sd_group']),
         f'{float(filt["t"]):.2f}', f'{float(filt["df"]):.1f}', fmt_p_cell(filt['p']), f'{float(filt["d"]):.2f}'],
    ]
    a11 = tmpl.build([2000, 2500, 950, 950, 1350, 1350, 800, 900, 800, 700],
                     ['Sample', 'Comparison group', 'n retained', 'n group', 'Retained M (SD)', 'Group M (SD)', 't', 'df', 'p', 'd'],
                     a11_rows, left_cols=(0, 1))
    cap = new_para_after(n3, [('Table A11', {'bold': True})])
    ttl = new_para_after(cap, [('MIDUS 2 Executive Functioning by MIDUS 3 Retention Status', {'italic': True})])
    ttl.addnext(a11)
    new_para_after(a11, [('Note. ', {'italic': True})] + rich(
        'Executive functioning = MIDUS 2 BTACT composite *z*-score. Retained = provided a MIDUS 3 executive functioning score '
        '(full sample) or met the MIDUS 3 analytic inclusion criteria (analytic sample). *t* and *df* from Welch’s unequal-variance '
        '*t* test; *d* = Cohen’s *d* (retained minus comparison group).'))

    # ---------------------------------------------------------------- 7. Discussion tone-down
    T = [
        # intro
        ('neurocognitive deficits represent the most common type of disability',
         'the results overwhelmingly did not support', 'the results did not support'),
        # summary of findings
        ('Hypotheses 1 through 4 predicted that executive functioning would be negatively related',
         'suggesting that any true main effect of executive functioning on spillover is likely trivially small.',
         'suggesting that any true main effect of executive functioning on spillover in these samples is small at most.'),
        ('Cross-wave replicability, assessed using the Open Science Collaboration (2015) criteria, was poor.',
         'The multiverse analyses confirmed this pattern:', 'The multiverse analyses were consistent with this pattern:'),
        ('Cross-wave replicability, assessed using the Open Science Collaboration (2015) criteria, was poor.',
         'the few significant effects observed are likely Type I errors',
         'the few significant effects observed may reflect Type I error'),
        # theoretical
        ('The first concerns the relative strength of situational and contextual factors',
         'were far stronger predictors of spillover than individual differences in executive functioning across all models.',
         'were considerably stronger predictors of spillover than individual differences in executive functioning across all models.'),
        ('The first concerns the relative strength of situational and contextual factors',
         'The implication of this finding is that executive functioning is irrelevant to spillover but that its influence may be conditional',
         'The implication of this finding is not that executive functioning is irrelevant to spillover, but that its influence may be conditional'),
        ('The first concerns the relative strength of situational and contextual factors',
         'may therefore need to incorporate situation strength as a moderating boundary condition',
         'may therefore benefit from incorporating situation strength as a moderating boundary condition'),
        ('The second concerns the role of gender in the EF–spillover relationship.',
         'providing no reliable evidence that gender amplifies', 'providing no consistent evidence that gender amplifies'),
        # practical
        ('The consistent, large effects of job demands on negative work-to-family spillover',
         'The consistent, large effects of job demands on negative work-to-family spillover (β = .39–.54) suggest that organizations seeking to reduce spillover should focus on workload management.',
         'The consistent associations between job demands and negative work-to-family spillover (β = .39–.54) suggest that organizations seeking to reduce spillover may wish to prioritize workload management.'),
        ('suggest that organizations seeking to reduce spillover may wish to prioritize workload management.',
         'may be among the most effective interventions.', 'may be promising interventions.'),
        ('suggest that organizations seeking to reduce spillover may wish to prioritize workload management.',
         'the strong effects of family demands on negative family-to-work spillover (β = .35–.43) underscore the value of organizational policies',
         'the associations between family demands and negative family-to-work spillover (β = .35–.43) are consistent with the value of organizational policies'),
        ('At the organizational level,',
         'the findings underscore the importance of social support in work-family dynamics',
         'the findings are consistent with the importance of social support in work-family dynamics'),
        ('At the organizational level,',
         'suggest that social support operates as a direct resource for managing work-family demands',
         'are compatible with the view that social support may operate as a resource for managing work-family demands, although the cross-sectional design precludes causal inference'),
        ('At the organizational level,',
         'should consider investing in supervisor training', 'could consider investing in supervisor training'),
        ('For organizations concerned about supporting employees with executive functioning difficulties',
         'offer a more promising path than cognitive intervention',
         'may offer a more practical starting point than cognitive intervention'),
        ('For organizations concerned about supporting employees with executive functioning difficulties',
         'The key insight from the present study is that these environmental and structural supports are likely effective not because they compensate for cognitive deficits that drive spillover, but because they directly address the situational demands and support deficits that actually do.',
         'One interpretation of the present findings is that such environmental and structural supports may help less by compensating for cognitive deficits than by addressing the situational demands and support deficits that were more consistently associated with spillover in these data; this interpretation remains to be tested directly.'),
        # limitations
        ('Second, the cross-sectional, between-person design of this study',
         'as a stable individual difference, which the present study suggests it does not,',
         'as a stable individual difference, for which the present study found little evidence,'),
        ('The examination of boundary conditions in this study was intended',
         'provide clear evidence that these environmental factors matter',
         'suggest that these environmental factors matter'),
        ('Fourth, the composition of the analytic sample likely constrains the range',
         'The very individuals for whom theories of cognitive resource depletion and boundary management failure would predict the largest EF–spillover effects are therefore systematically less likely to appear in the analytic sample.',
         'The very individuals for whom theories of cognitive resource depletion and boundary management failure would predict the largest EF–spillover effects are therefore systematically less likely to appear in the analytic sample. The supplementary attrition analysis provides direct evidence of this selection: respondents who dropped out between MIDUS 2 and MIDUS 3, or who no longer met the employment and complete-data criteria at MIDUS 3, had substantially lower MIDUS 2 executive functioning than those retained (Cohen’s d = 0.36–0.57; Table A11, Figure 4).'),
        # conclusion
        ('This study represents the first direct empirical examination of the relationship between executive functioning and work-family spillover.',
         'indicate that the general executive functioning composite measured by the BTACT does not meaningfully predict negative or positive spillover between work and family domains.',
         'suggest that the general executive functioning composite measured by the BTACT was not a meaningful predictor of negative or positive spillover between work and family domains in these samples.'),
        ('This study represents the first direct empirical examination of the relationship between executive functioning and work-family spillover.',
         'The findings underscore the primacy of environmental factors, particularly job demands, family demands, and social support, in shaping work-family spillover experiences. By understanding these factors, organizations can develop support systems to help employees maintain a healthy work-life balance, reduce turnover and absenteeism, and enhance overall employee engagement and job satisfaction, ultimately contributing to a healthier and more productive workforce.',
         'The findings instead point to the comparatively greater role of environmental factors, particularly job demands, family demands, and social support, in work-family spillover experiences, and suggest that organizational efforts to reduce spillover may be most productively directed at these factors.'),
        ('This study represents the first direct empirical examination of the relationship between executive functioning and work-family spillover.',
         'they provide an important corrective to the assumption that broad cognitive differences are a primary driver of work-family outcomes.',
         'they caution against assuming that broad cognitive differences are a primary driver of work-family outcomes.'),
        # abstract + chapter 1
        ('Theories of work-family spillover implicate cognitive functioning as a crucial resource',
         'The findings suggest that interventions should target situational demands and social support resources to reduce work-family spillover.',
         'The findings suggest that interventions targeting situational demands and social support resources may be more fruitful than those targeting broad cognitive capacity for reducing work-family spillover.'),
        ('These aims collectively contribute to both theory and practice.',
         'Practically, the findings will provide organizations and policymakers with evidence-based guidance for supporting employees with executive dysfunction, potentially improving both employee well-being and organizational outcomes.',
         'Practically, the findings may inform how organizations and policymakers support employees with executive dysfunction.'),
    ]
    for anchor_txt, old, new in T:
        _, p = find_para(doc, contains(anchor_txt))
        replace_in_para(p, old, new)
        print('toned:', old[:50], '…')

    # add Big Five / attrition sentence to Summary of Findings (end of the replicability/multiverse paragraph)
    _, p = find_para(doc, contains('Cross-wave replicability, assessed using the Open Science Collaboration (2015) criteria, was poor.'))
    extra = p.add_run(' Two supplementary analyses reinforce this reading: adjusting for the Big Five '
                      'personality traits did not uncover an executive functioning effect in the full models, and the positive '
                      'zero-order association between executive functioning and negative spillover that appeared when only '
                      'personality was controlled did not survive the inclusion of job and family demands and demographic covariates. '
                      'At the same time, attrition between waves was selective on executive functioning, so the later-wave '
                      'analytic sample under-represents respondents with lower executive functioning.')
    mark_run(extra._r)

    # ---------------------------------------------------------------- 8. restyle Discussion sub-headings
    for heading in ('Practical Implications', 'Conclusion'):
        i, p = find_para(doc, lambda t, h=heading: t.strip() == h)
        p.style = doc.styles['DISS_3_SUBSECTION']
        for r in p.runs:
            r.bold = None
        print('restyled heading:', heading)

    doc.save(str(OUT))
    print('wrote', OUT)


if __name__ == '__main__':
    main()
