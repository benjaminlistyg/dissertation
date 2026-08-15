#!/usr/bin/env python3
"""Generate results_tables document in APA format.

APA 7th edition table rules:
- Three horizontal rules only: top, below header, bottom
- NO vertical rules
- NO internal horizontal rules between data rows
- Landscape Letter, 1-inch margins, Times New Roman
- Title: "Table N" bold 12pt; subtitle italic 12pt
- Cell text: 8pt (7pt for very wide tables)
- Note: italic 9pt
- Header rows that use colspan are merged in Word
"""

from docx import Document
from docx.shared import Pt, Inches
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.section import WD_ORIENT
from docx.oxml.ns import qn, nsdecls
from docx.oxml import parse_xml
import re
import os
import datetime

BASE = '/Users/benjamin.listyg/Documents/research/dissertation/dissertation-code/dissertation_code/output/tables'


# ---------------------------------------------------------------------------
# HTML parsing with colspan support
# ---------------------------------------------------------------------------

def parse_html_table_with_spans(html_content):
    """Parse HTML table into rows, returning (text, colspan, bold, underline) tuples per cell."""
    rows = []
    tr_pattern = re.compile(r'<tr[^>]*>(.*?)</tr>', re.DOTALL)
    cell_pattern = re.compile(r'<(t[dh])([^>]*)>(.*?)</\1>', re.DOTALL)

    for tr_match in tr_pattern.finditer(html_content):
        tr_content = tr_match.group(1)
        cells = []
        for cell_match in cell_pattern.finditer(tr_content):
            attrs = cell_match.group(2)
            raw_text = cell_match.group(3)
            text = re.sub(r'<[^>]+>', '', raw_text).strip()
            text = text.replace('&lt;', '<').replace('&gt;', '>')
            text = text.replace('&amp;', '&').replace('&nbsp;', ' ')
            colspan_m = re.search(r'colspan\s*=\s*"(\d+)"', attrs)
            colspan = int(colspan_m.group(1)) if colspan_m else 1
            # Extract inline style formatting
            style_m = re.search(r'style\s*=\s*"([^"]*)"', attrs)
            style = style_m.group(1) if style_m else ''
            is_bold = 'font-weight: bold' in style or 'font-weight:bold' in style
            is_underline = 'text-decoration: underline' in style or 'text-decoration:underline' in style
            cells.append((text, colspan, is_bold, is_underline))
        if cells:
            rows.append(cells)
    return rows


def expand_rows(span_rows):
    """Expand (text, colspan, bold, underline) rows into flat rows + maps.

    Returns:
        flat_rows: list of lists of strings (one per column)
        merge_map: dict of (row_idx, col_idx) -> span_count for cells needing merge
        fmt_map: dict of (row_idx, col_idx) -> (bold, underline) for cells with formatting
    """
    # First determine total columns from the widest expanded row
    total_cols = 0
    for row in span_rows:
        cols = sum(c[1] for c in row)
        total_cols = max(total_cols, cols)

    flat_rows = []
    merge_map = {}
    fmt_map = {}
    for i, row in enumerate(span_rows):
        flat = []
        col_idx = 0
        for text, colspan, is_bold, is_underline in row:
            flat.append(text)
            if is_bold or is_underline:
                fmt_map[(i, col_idx)] = (is_bold, is_underline)
            if colspan > 1:
                merge_map[(i, col_idx)] = colspan
                for extra in range(colspan - 1):
                    flat.append('')  # placeholder for spanned columns
                    if is_bold or is_underline:
                        fmt_map[(i, col_idx + 1 + extra)] = (is_bold, is_underline)
            col_idx += colspan
        # Pad to total_cols
        while len(flat) < total_cols:
            flat.append('')
        flat_rows.append(flat)

    return flat_rows, merge_map, fmt_map


def split_note(rows):
    """Separate note row from data rows."""
    note = ""
    data = []
    for row in rows:
        if row and row[0].startswith('Note.'):
            note = row[0]
        else:
            data.append(row)
    return data, note


def read_html(subdir, fname):
    fpath = os.path.join(BASE, subdir, fname)
    if not os.path.isfile(fpath):
        return [], {}, {}, ""
    with open(fpath, 'r') as f:
        content = f.read()
    span_rows = parse_html_table_with_spans(content)
    # Convert to flat text rows for split_note
    temp_flat = []
    for row in span_rows:
        temp_flat.append([c[0] for c in row])
    data_temp, note = split_note(temp_flat)
    # Now re-expand only data rows (excluding note)
    data_span = [r for r in span_rows if not (r and r[0][0].startswith('Note.'))]
    flat_rows, merge_map, fmt_map = expand_rows(data_span)
    return flat_rows, merge_map, fmt_map, note


# ---------------------------------------------------------------------------
# Document building
# ---------------------------------------------------------------------------

doc = Document()


def set_landscape(section):
    section.orientation = WD_ORIENT.LANDSCAPE
    section.page_width = Inches(11)
    section.page_height = Inches(8.5)
    section.top_margin = Inches(1)
    section.bottom_margin = Inches(1)
    section.left_margin = Inches(1)
    section.right_margin = Inches(1)


def make_run(paragraph, text, bold=False, italic=False, size_pt=12):
    run = paragraph.add_run(text)
    run.bold = bold
    run.italic = italic
    run.font.size = Pt(size_pt)
    run.font.name = 'Times New Roman'
    return run


def set_cell_border(tc, top_val="none", top_sz="0", bottom_val="none", bottom_sz="0"):
    """Set APA borders on a single table cell: only horizontal, no vertical."""
    tcPr = tc.find(qn('w:tcPr'))
    if tcPr is None:
        tcPr = parse_xml(f'<w:tcPr {nsdecls("w")}/>')
        tc.insert(0, tcPr)
    borders = parse_xml(
        f'<w:tcBorders {nsdecls("w")}>'
        f'<w:top w:val="{top_val}" w:sz="{top_sz}" w:space="0" w:color="auto"/>'
        f'<w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:bottom w:val="{bottom_val}" w:sz="{bottom_sz}" w:space="0" w:color="auto"/>'
        f'<w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'</w:tcBorders>'
    )
    existing = tcPr.find(qn('w:tcBorders'))
    if existing is not None:
        tcPr.remove(existing)
    tcPr.append(borders)


def set_col_width(tc, width_inches):
    """Set a fixed column width on a cell."""
    tcPr = tc.find(qn('w:tcPr'))
    if tcPr is None:
        tcPr = parse_xml(f'<w:tcPr {nsdecls("w")}/>')
        tc.insert(0, tcPr)
    width_twips = int(width_inches * 1440)
    w_el = tcPr.find(qn('w:tcW'))
    if w_el is not None:
        tcPr.remove(w_el)
    tcPr.append(parse_xml(
        f'<w:tcW {nsdecls("w")} w:w="{width_twips}" w:type="dxa"/>'
    ))


def fill_cell(table, i, j, text, cell_size_pt, is_header,
              cell_bold=False, cell_underline=False):
    """Clear and fill a single cell with optional bold/underline from HTML styles."""
    cell = table.cell(i, j)
    tc = cell._tc
    for existing_p in tc.findall(qn('w:p')):
        tc.remove(existing_p)
    new_p = parse_xml(
        f'<w:p {nsdecls("w")}>'
        f'<w:pPr><w:spacing w:after="0" w:before="0"/></w:pPr>'
        f'</w:p>'
    )
    tc.append(new_p)
    p = cell.paragraphs[0]
    if text:
        run = p.add_run(text)
        run.font.size = Pt(cell_size_pt)
        run.font.name = 'Times New Roman'
        # Apply bold for headers OR significant data rows (from HTML style)
        if is_header or cell_bold:
            run.bold = True
        # Apply underline for EF variable rows (from HTML style)
        if cell_underline:
            run.underline = True
    p.alignment = WD_ALIGN_PARAGRAPH.LEFT if j == 0 else WD_ALIGN_PARAGRAPH.CENTER


def compute_col_widths(max_cols, col0_width=None):
    """Compute column widths in inches for a table.

    For other tables: col0_width for first column, rest auto (None).
    """
    AVAIL = 9.0  # landscape letter minus 1" margins each side

    if col0_width:
        remaining = AVAIL - col0_width
        per_col = remaining / (max_cols - 1) if max_cols > 1 else remaining
        return [col0_width] + [per_col] * (max_cols - 1)
    else:
        return None  # autofit


# Column widths for 6-column regression panels (Variable + B + β + SE + p + CI)
PANEL_COL_WIDTHS = [2.5, 1.1, 0.8, 1.0, 0.8, 2.2]  # total = 8.4"


def add_table_page(doc, table_label, title, data_rows, merge_map, fmt_map,
                   note_text, cell_size_pt=8, is_first=False,
                   col0_width=None):
    """Add a complete APA table page."""
    if not is_first:
        doc.add_section()

    section = doc.sections[-1]
    set_landscape(section)

    # Title
    p_title = doc.add_paragraph()
    p_title.paragraph_format.space_after = Pt(0)
    p_title.paragraph_format.space_before = Pt(0)
    make_run(p_title, table_label, bold=True, size_pt=12)

    # Subtitle
    p_sub = doc.add_paragraph()
    p_sub.paragraph_format.space_after = Pt(4)
    p_sub.paragraph_format.space_before = Pt(0)
    make_run(p_sub, title, italic=True, size_pt=12)

    if not data_rows:
        doc.add_paragraph()
        return

    max_cols = max(len(r) for r in data_rows)
    table = doc.add_table(rows=len(data_rows), cols=max_cols)
    table.style = None

    # Disable all table-level borders
    tbl = table._tbl
    tblPr = tbl.tblPr if tbl.tblPr is not None else parse_xml(f'<w:tblPr {nsdecls("w")}/>')
    for tag in ['tblBorders', 'tblLayout']:
        existing = tblPr.find(qn(f'w:{tag}'))
        if existing is not None:
            tblPr.remove(existing)
    tblPr.append(parse_xml(
        f'<w:tblBorders {nsdecls("w")}>'
        f'<w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:insideH w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:insideV w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'</w:tblBorders>'
    ))
    # Use fixed layout so column widths are respected
    tblPr.append(parse_xml(f'<w:tblLayout {nsdecls("w")} w:type="fixed"/>'))

    # Determine header row count
    n_header_rows = 1
    if len(data_rows) > 1:
        row1 = data_rows[1]
        sub_hdr_tokens = {'B', '\u03B2', 'SE', 'p', '95% CI', 'n', 'M', 'SD',
                          'W1 B [95% CI]', 'W2 B [95% CI]', 'Sign', 'Sig', 'CI'}
        if any(c.strip() in sub_hdr_tokens for c in row1):
            n_header_rows = 2

    last_row = len(data_rows) - 1

    # Compute column widths
    col_widths = compute_col_widths(max_cols, col0_width)

    # Fill all cells
    for i, row_data in enumerate(data_rows):
        is_header = (i < n_header_rows)
        # Set row height for breathing room (prevents "bunched up" rows)
        tr = table.rows[i]._tr
        trPr = tr.find(qn('w:trPr'))
        if trPr is None:
            trPr = parse_xml(f'<w:trPr {nsdecls("w")}/>')
            tr.insert(0, trPr)
        # Row height: 320 twips (~0.22") gives comfortable spacing
        trPr.append(parse_xml(
            f'<w:trHeight {nsdecls("w")} w:val="320" w:hRule="atLeast"/>'
        ))

        for j in range(max_cols):
            text = row_data[j] if j < len(row_data) else ''
            # Look up per-cell formatting from HTML styles
            cell_bold, cell_underline = fmt_map.get((i, j), (False, False))
            fill_cell(table, i, j, text, cell_size_pt, is_header,
                      cell_bold=cell_bold, cell_underline=cell_underline)

            # Set column widths
            if col_widths and j < len(col_widths):
                set_col_width(table.cell(i, j)._tc, col_widths[j])

            # APA borders
            top_v, top_s = "none", "0"
            bot_v, bot_s = "none", "0"
            if i == 0:
                top_v, top_s = "single", "8"
            if i == n_header_rows - 1:
                bot_v, bot_s = "single", "8"
            if i == last_row:
                bot_v, bot_s = "single", "8"
            set_cell_border(table.cell(i, j)._tc, top_v, top_s, bot_v, bot_s)

    # Merge cells for colspan headers
    for (row_idx, col_idx), span in sorted(merge_map.items()):
        try:
            a = table.cell(row_idx, col_idx)
            b = table.cell(row_idx, col_idx + span - 1)
            a.merge(b)
            # Re-apply formatting on merged cell
            cb, cu = fmt_map.get((row_idx, col_idx), (False, False))
            fill_cell(table, row_idx, col_idx,
                      data_rows[row_idx][col_idx] if col_idx < len(data_rows[row_idx]) else '',
                      cell_size_pt, row_idx < n_header_rows,
                      cell_bold=cb, cell_underline=cu)
            # Re-apply borders on the merged cell
            top_v, top_s = "none", "0"
            bot_v, bot_s = "none", "0"
            if row_idx == 0:
                top_v, top_s = "single", "8"
            if row_idx == n_header_rows - 1:
                bot_v, bot_s = "single", "8"
            if row_idx == last_row:
                bot_v, bot_s = "single", "8"
            set_cell_border(table.cell(row_idx, col_idx)._tc, top_v, top_s, bot_v, bot_s)
        except Exception as e:
            print(f"  Warning: merge ({row_idx},{col_idx}) span={span}: {e}")

    # Note
    p_note = doc.add_paragraph()
    p_note.paragraph_format.space_before = Pt(2)
    p_note.paragraph_format.space_after = Pt(0)
    if note_text:
        run_note = p_note.add_run(note_text)
        run_note.font.size = Pt(9)
        run_note.font.name = 'Times New Roman'
        run_note.italic = True

    doc.add_paragraph()


def _make_apa_subtable(doc, panel_rows, panel_fmt, col_widths, cell_size_pt,
                       row_height_twips=320):
    """Create a single APA-bordered Word table from panel rows.

    panel_rows: list of lists of strings (first row is header).
    panel_fmt: dict of (row, col) -> (bold, underline).
    """
    n_rows = len(panel_rows)
    n_cols = len(panel_rows[0]) if panel_rows else 0
    if n_rows == 0 or n_cols == 0:
        return

    table = doc.add_table(rows=n_rows, cols=n_cols)
    table.style = None

    # Table-level: no borders, fixed layout
    tbl = table._tbl
    tblPr = tbl.tblPr if tbl.tblPr is not None else parse_xml(f'<w:tblPr {nsdecls("w")}/>')
    for tag in ['tblBorders', 'tblLayout']:
        existing = tblPr.find(qn(f'w:{tag}'))
        if existing is not None:
            tblPr.remove(existing)
    tblPr.append(parse_xml(
        f'<w:tblBorders {nsdecls("w")}>'
        f'<w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:insideH w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:insideV w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'</w:tblBorders>'
    ))
    tblPr.append(parse_xml(f'<w:tblLayout {nsdecls("w")} w:type="fixed"/>'))

    last_row = n_rows - 1

    for i, row_data in enumerate(panel_rows):
        is_header = (i == 0)
        # Row height
        tr = table.rows[i]._tr
        trPr = tr.find(qn('w:trPr'))
        if trPr is None:
            trPr = parse_xml(f'<w:trPr {nsdecls("w")}/>')
            tr.insert(0, trPr)
        trPr.append(parse_xml(
            f'<w:trHeight {nsdecls("w")} w:val="{row_height_twips}" w:hRule="atLeast"/>'
        ))

        for j in range(n_cols):
            text = row_data[j] if j < len(row_data) else ''
            cb, cu = panel_fmt.get((i, j), (False, False))
            fill_cell(table, i, j, text, cell_size_pt, is_header,
                      cell_bold=cb, cell_underline=cu)

            if col_widths and j < len(col_widths):
                set_col_width(table.cell(i, j)._tc, col_widths[j])

            # APA borders: top on row 0, bottom on row 0 (header sep), bottom on last row
            top_v, top_s = "none", "0"
            bot_v, bot_s = "none", "0"
            if i == 0:
                top_v, top_s = "single", "8"
                bot_v, bot_s = "single", "8"
            if i == last_row:
                bot_v, bot_s = "single", "8"
            set_cell_border(table.cell(i, j)._tc, top_v, top_s, bot_v, bot_s)


def add_panel_table(doc, table_label, title, data_rows, merge_map, fmt_map,
                    note_text, is_first=False):
    """Add a 21-column regression/multiverse table as 4 outcome panels.

    Each panel has 6 columns: Variable, B, β, SE, p, 95% CI.
    This gives each column generous width and uses 10pt font for readability.
    """
    if not is_first:
        doc.add_section()

    section = doc.sections[-1]
    set_landscape(section)

    # Title
    p_title = doc.add_paragraph()
    p_title.paragraph_format.space_after = Pt(0)
    p_title.paragraph_format.space_before = Pt(0)
    make_run(p_title, table_label, bold=True, size_pt=12)

    # Subtitle
    p_sub = doc.add_paragraph()
    p_sub.paragraph_format.space_after = Pt(6)
    p_sub.paragraph_format.space_before = Pt(0)
    make_run(p_sub, title, italic=True, size_pt=12)

    if not data_rows or len(data_rows) < 3:
        doc.add_paragraph()
        return

    # Detect structure: row 0 has outcome names (colspan=5), row 1 has B/β/SE/p/CI
    # Extract outcome names from row 0 using merge_map
    outcome_names = []
    outcome_cols = []
    for (ri, ci), span in sorted(merge_map.items()):
        if ri == 0 and ci > 0 and span == 5:
            name = data_rows[0][ci] if ci < len(data_rows[0]) else ''
            outcome_names.append(name)
            outcome_cols.append(ci)

    # Fallback: if merge_map didn't give us 4 outcomes, try to extract from row 0
    if len(outcome_names) != 4:
        # Try reading non-empty cells from row 0 after column 0
        for ci, text in enumerate(data_rows[0]):
            if ci > 0 and text.strip():
                outcome_names.append(text.strip())
        outcome_cols = [1, 6, 11, 16]

    if len(outcome_names) < 4:
        outcome_names = ['Outcome 1', 'Outcome 2', 'Outcome 3', 'Outcome 4']
        outcome_cols = [1, 6, 11, 16]

    # Expand abbreviated outcome names to full APA-style names
    name_map = {
        'Neg W→F': 'Negative Work-to-Family Spillover',
        'Neg F→W': 'Negative Family-to-Work Spillover',
        'Pos W→F': 'Positive Work-to-Family Spillover',
        'Pos F→W': 'Positive Family-to-Work Spillover',
        'NEGWF': 'Negative Work-to-Family Spillover',
        'NEGFW': 'Negative Family-to-Work Spillover',
        'POSWF': 'Positive Work-to-Family Spillover',
        'POSFW': 'Positive Family-to-Work Spillover',
    }
    outcome_names = [name_map.get(n, n) for n in outcome_names]

    n_header_rows = 2  # row 0 = outcome names, row 1 = B/β/SE/p/CI
    data_start = n_header_rows
    panel_labels = ['A', 'B', 'C', 'D']

    for pidx in range(4):
        col_start = 1 + pidx * 5  # first data column for this outcome

        # Panel heading
        p_panel = doc.add_paragraph()
        p_panel.paragraph_format.space_before = Pt(12 if pidx > 0 else 4)
        p_panel.paragraph_format.space_after = Pt(2)
        run_lbl = p_panel.add_run(f"Panel {panel_labels[pidx]}. ")
        run_lbl.bold = True
        run_lbl.font.size = Pt(10)
        run_lbl.font.name = 'Times New Roman'
        run_name = p_panel.add_run(outcome_names[pidx])
        run_name.italic = True
        run_name.font.size = Pt(10)
        run_name.font.name = 'Times New Roman'

        # Build panel rows: [header] + [data rows]
        panel_header = ['Variable', 'B', '\u03B2', 'SE', 'p', '95% CI']
        panel_rows = [panel_header]
        panel_fmt = {}

        for orig_i in range(data_start, len(data_rows)):
            row = data_rows[orig_i]
            var_name = row[0] if len(row) > 0 else ''
            panel_row = [var_name]
            for k in range(5):
                c = col_start + k
                panel_row.append(row[c] if c < len(row) else '')
            panel_rows.append(panel_row)

            # Map formatting from original to panel coordinates
            out_i = len(panel_rows) - 1
            b0, u0 = fmt_map.get((orig_i, 0), (False, False))
            if b0 or u0:
                panel_fmt[(out_i, 0)] = (b0, u0)
            for k in range(5):
                bf, uf = fmt_map.get((orig_i, col_start + k), (False, False))
                if bf or uf:
                    panel_fmt[(out_i, 1 + k)] = (bf, uf)

        _make_apa_subtable(doc, panel_rows, panel_fmt, PANEL_COL_WIDTHS,
                           cell_size_pt=10, row_height_twips=320)

    # Note (after last panel)
    if note_text:
        p_note = doc.add_paragraph()
        p_note.paragraph_format.space_before = Pt(4)
        p_note.paragraph_format.space_after = Pt(0)
        run_note = p_note.add_run(note_text)
        run_note.font.size = Pt(9)
        run_note.font.name = 'Times New Roman'
        run_note.italic = True


# ---------------------------------------------------------------------------
# Build all tables
# ---------------------------------------------------------------------------

# Table 1-2: Descriptive Statistics
data, mm, fm, note = read_html('descriptives', 'descriptives_m2.html')
add_table_page(doc, "Table 1", "Descriptive Statistics (MIDUS 2, N = 1,289)",
               data, mm, fm, note, cell_size_pt=8, is_first=True)

data, mm, fm, note = read_html('descriptives', 'descriptives_m3.html')
add_table_page(doc, "Table 2", "Descriptive Statistics (MIDUS 3, N = 729)",
               data, mm, fm, note, cell_size_pt=8)

# Table 3-4: Correlations (wider col0 for variable names)
data, mm, fm, note = read_html('descriptives', 'correlations_m2.html')
add_table_page(doc, "Table 3", "Bivariate Correlations (MIDUS 2, N = 1,289)",
               data, mm, fm, note, cell_size_pt=7, col0_width=2.5)

data, mm, fm, note = read_html('descriptives', 'correlations_m3.html')
add_table_page(doc, "Table 4", "Bivariate Correlations (MIDUS 3, N = 729)",
               data, mm, fm, note, cell_size_pt=7, col0_width=2.5)

# Tables 5-14: Regression tables
reg_tables = [
    ('table01_main_effects_m2.html', 5,
     'Main Effects of Executive Functioning on Work-Family Spillover (MIDUS 2)'),
    ('table02_main_effects_m3.html', 6,
     'Main Effects of Executive Functioning on Work-Family Spillover (MIDUS 3)'),
    ('table03_gender_moderation_m2.html', 7,
     'Gender \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table04_gender_moderation_m3.html', 8,
     'Gender \u00D7 Executive Functioning Interactions (MIDUS 3)'),
    ('table05_supervisor_moderation_m2.html', 9,
     'Supervisor Support \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table06_supervisor_moderation_m3.html', 10,
     'Supervisor Support \u00D7 Executive Functioning Interactions (MIDUS 3)'),
    ('table07_coworker_moderation_m2.html', 11,
     'Coworker Support \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table08_coworker_moderation_m3.html', 12,
     'Coworker Support \u00D7 Executive Functioning Interactions (MIDUS 3)'),
    ('table09_spousal_moderation_m2.html', 13,
     'Spousal Support \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table10_spousal_moderation_m3.html', 14,
     'Spousal Support \u00D7 Executive Functioning Interactions (MIDUS 3)'),
]

for fname, tnum, title in reg_tables:
    data, mm, fm, note = read_html('regressions', fname)
    add_panel_table(doc, f"Table {tnum}", title, data, mm, fm, note)

# Table 15: Cross-Wave Replicability
data, mm, fm, note = read_html('replicability', 'replicability_regressions.html')
add_table_page(doc, "Table 15",
               "Cross-Wave Replicability: Primary Regressions (OSC, 2015)",
               data, mm, fm, note, cell_size_pt=7, col0_width=2.2)

# --- APPENDIX ---

# A1-A2: Multiverse Wide (also 21 columns → panels)
data, mm, fm, note = read_html('multiverse', 'multiverse_wide_m2.html')
add_panel_table(doc, "Table A1", "Multiverse Summary: EF Main Effects (MIDUS 2)",
                data, mm, fm, note)

data, mm, fm, note = read_html('multiverse', 'multiverse_wide_m3.html')
add_panel_table(doc, "Table A2", "Multiverse Summary: EF Main Effects (MIDUS 3)",
                data, mm, fm, note)

# A3-A4: Multiverse Long
data, mm, fm, note = read_html('multiverse', 'multiverse_long_m2.html')
add_table_page(doc, "Table A3",
               "Multiverse Analysis: EF Effects Across Specifications (MIDUS 2)",
               data, mm, fm, note, cell_size_pt=7, col0_width=1.8)

data, mm, fm, note = read_html('multiverse', 'multiverse_long_m3.html')
add_table_page(doc, "Table A4",
               "Multiverse Analysis: EF Effects Across Specifications (MIDUS 3)",
               data, mm, fm, note, cell_size_pt=7, col0_width=1.8)

# A5: Multiverse Replicability
data, mm, fm, note = read_html('replicability', 'replicability_multiverse.html')
add_table_page(doc, "Table A5",
               "Cross-Wave Replicability: Multiverse Specifications (OSC, 2015)",
               data, mm, fm, note, cell_size_pt=7, col0_width=2.2)

# A6-A12: CFA/MI tables
cfa_tables = [
    ('cfa_fit_indices.html', 'A6', 'Confirmatory Factor Analysis Model Fit Indices'),
    ('cfa_loadings_m2.html', 'A7', 'Standardized Factor Loadings (MIDUS 2)'),
    ('cfa_factor_correlations_m2.html', 'A8', 'Latent Factor Correlations (MIDUS 2)'),
    ('cfa_loadings_m3.html', 'A9', 'Standardized Factor Loadings (MIDUS 3)'),
    ('cfa_factor_correlations_m3.html', 'A10', 'Latent Factor Correlations (MIDUS 3)'),
    ('invariance_gender_m2.html', 'A11', 'Measurement Invariance Across Gender (MIDUS 2)'),
    ('invariance_gender_m3.html', 'A12', 'Measurement Invariance Across Gender (MIDUS 3)'),
]
for fname, label, title in cfa_tables:
    data, mm, fm, note = read_html('cfa', fname)
    if data:
        add_table_page(doc, f"Table {label}", title, data, mm, fm, note, cell_size_pt=8)

# A13-A20: Power tables
power_tables = [
    ('power_mde.html', 'A13', 'Minimum Detectable Effect Sizes (\u03B1 = .05)'),
    ('power_fixed_effects.html', 'A14',
     'Statistical Power at Fixed Effect Sizes (\u03B1 = .05)'),
    ('power_beta_midusii_k8.html', 'A15',
     'Power by Standardized Effect Size: MIDUS 2 (k = 8)'),
    ('power_beta_midusii_k10.html', 'A16',
     'Power by Standardized Effect Size: MIDUS 2 (k = 10)'),
    ('power_beta_midusii_k15.html', 'A17',
     'Power by Standardized Effect Size: MIDUS 2 (k = 15)'),
    ('power_beta_midusiii_k8.html', 'A18',
     'Power by Standardized Effect Size: MIDUS 3 (k = 8)'),
    ('power_beta_midusiii_k10.html', 'A19',
     'Power by Standardized Effect Size: MIDUS 3 (k = 10)'),
    ('power_beta_midusiii_k15.html', 'A20',
     'Power by Standardized Effect Size: MIDUS 3 (k = 15)'),
]
for fname, label, title in power_tables:
    data, mm, fm, note = read_html('power', fname)
    if data:
        add_table_page(doc, f"Table {label}", title, data, mm, fm, note, cell_size_pt=8)

# Save
ts = datetime.datetime.now().strftime('%Y-%m-%d')
outpath = f'/Users/benjamin.listyg/Documents/research/dissertation/dissertation-docs/results_tables_{ts}.docx'
doc.save(outpath)
print(f"Saved: {outpath}")
