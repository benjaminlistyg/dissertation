#!/usr/bin/env python3
"""Generate combined APA tables for Power Analysis and CFA/MI sections."""

from docx import Document
from docx.shared import Pt, Inches
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.section import WD_ORIENT
from docx.oxml.ns import qn, nsdecls
from docx.oxml import parse_xml
import datetime

doc = Document()

# Set default font
style = doc.styles['Normal']
style.font.name = 'Times New Roman'
style.font.size = Pt(12)

# ---------------------------------------------------------------------------
# Helpers (same APA style as gen_results_tables_v2.py)
# ---------------------------------------------------------------------------

def set_landscape(section):
    section.orientation = WD_ORIENT.LANDSCAPE
    section.page_width = Inches(11)
    section.page_height = Inches(8.5)
    for m in ('top_margin', 'bottom_margin', 'left_margin', 'right_margin'):
        setattr(section, m, Inches(1))


def set_cell_border(tc, top_val="none", top_sz="0", bot_val="none", bot_sz="0"):
    tcPr = tc.find(qn('w:tcPr'))
    if tcPr is None:
        tcPr = parse_xml(f'<w:tcPr {nsdecls("w")}/>')
        tc.insert(0, tcPr)
    borders = parse_xml(
        f'<w:tcBorders {nsdecls("w")}>'
        f'<w:top w:val="{top_val}" w:sz="{top_sz}" w:space="0" w:color="auto"/>'
        f'<w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'<w:bottom w:val="{bot_val}" w:sz="{bot_sz}" w:space="0" w:color="auto"/>'
        f'<w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
        f'</w:tcBorders>'
    )
    existing = tcPr.find(qn('w:tcBorders'))
    if existing is not None:
        tcPr.remove(existing)
    tcPr.append(borders)


def set_col_width(tc, width_inches):
    tcPr = tc.find(qn('w:tcPr'))
    if tcPr is None:
        tcPr = parse_xml(f'<w:tcPr {nsdecls("w")}/>')
        tc.insert(0, tcPr)
    w_twips = int(width_inches * 1440)
    w_el = tcPr.find(qn('w:tcW'))
    if w_el is not None:
        tcPr.remove(w_el)
    tcPr.append(parse_xml(
        f'<w:tcW {nsdecls("w")} w:w="{w_twips}" w:type="dxa"/>'
    ))


def fill_cell(table, i, j, text, size_pt, bold=False, italic=False, align=None):
    cell = table.cell(i, j)
    tc = cell._tc
    for p in tc.findall(qn('w:p')):
        tc.remove(p)
    tc.append(parse_xml(
        f'<w:p {nsdecls("w")}>'
        f'<w:pPr><w:spacing w:after="0" w:before="0"/></w:pPr>'
        f'</w:p>'
    ))
    p = cell.paragraphs[0]
    if text:
        run = p.add_run(text)
        run.font.size = Pt(size_pt)
        run.font.name = 'Times New Roman'
        if bold:
            run.bold = True
        if italic:
            run.italic = True
    if align is None:
        p.alignment = WD_ALIGN_PARAGRAPH.LEFT if j == 0 else WD_ALIGN_PARAGRAPH.CENTER
    else:
        p.alignment = align


def make_apa_table(doc, rows, col_widths, font_pt=10, row_ht=320,
                   n_header_rows=1, bold_col0=False):
    """Create an APA-bordered table and return the table object."""
    n_rows = len(rows)
    n_cols = len(rows[0]) if rows else 0
    table = doc.add_table(rows=n_rows, cols=n_cols)
    table.style = None

    tbl = table._tbl
    tblPr = tbl.tblPr if tbl.tblPr is not None else parse_xml(f'<w:tblPr {nsdecls("w")}/>')
    for tag in ('tblBorders', 'tblLayout'):
        ex = tblPr.find(qn(f'w:{tag}'))
        if ex is not None:
            tblPr.remove(ex)
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
    for i, row_data in enumerate(rows):
        is_hdr = (i < n_header_rows)
        tr = table.rows[i]._tr
        trPr = tr.find(qn('w:trPr'))
        if trPr is None:
            trPr = parse_xml(f'<w:trPr {nsdecls("w")}/>')
            tr.insert(0, trPr)
        trPr.append(parse_xml(
            f'<w:trHeight {nsdecls("w")} w:val="{row_ht}" w:hRule="atLeast"/>'
        ))

        for j in range(n_cols):
            text = row_data[j] if j < len(row_data) else ''
            b = is_hdr or (bold_col0 and j == 0)
            fill_cell(table, i, j, text, font_pt, bold=b)
            if col_widths and j < len(col_widths):
                set_col_width(table.cell(i, j)._tc, col_widths[j])

            top_v, top_s = "none", "0"
            bot_v, bot_s = "none", "0"
            if i == 0:
                top_v, top_s = "single", "8"
            if i == n_header_rows - 1:
                bot_v, bot_s = "single", "8"
            if i == last_row:
                bot_v, bot_s = "single", "8"
            set_cell_border(table.cell(i, j)._tc, top_v, top_s, bot_v, bot_s)

    return table


def add_title(doc, label, subtitle):
    p1 = doc.add_paragraph()
    p1.paragraph_format.space_after = Pt(0)
    p1.paragraph_format.space_before = Pt(0)
    run = p1.add_run(label)
    run.bold = True
    run.font.size = Pt(12)
    run.font.name = 'Times New Roman'

    p2 = doc.add_paragraph()
    p2.paragraph_format.space_after = Pt(6)
    p2.paragraph_format.space_before = Pt(0)
    run = p2.add_run(subtitle)
    run.italic = True
    run.font.size = Pt(12)
    run.font.name = 'Times New Roman'


def add_panel_header(doc, text, space_before=12):
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(space_before)
    p.paragraph_format.space_after = Pt(2)
    parts = text.split('. ', 1)
    run1 = p.add_run(parts[0] + '. ')
    run1.bold = True
    run1.font.size = Pt(10)
    run1.font.name = 'Times New Roman'
    if len(parts) > 1:
        run2 = p.add_run(parts[1])
        run2.italic = True
        run2.font.size = Pt(10)
        run2.font.name = 'Times New Roman'


def add_note(doc, text):
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(4)
    p.paragraph_format.space_after = Pt(0)
    run = p.add_run(text)
    run.font.size = Pt(9)
    run.font.name = 'Times New Roman'
    run.italic = True


# ===================================================================
# TABLE 1: COMBINED POWER ANALYSIS
# ===================================================================

set_landscape(doc.sections[0])

add_title(doc, "Table 1",
          "Sensitivity Power Analysis for Primary Regression Models")

# --- Panel A: Minimum Detectable Effect Sizes ---
add_panel_header(doc, "Panel A. Minimum Detectable Effect Sizes (\u03B1 = .05)",
                 space_before=4)

mde_header = ['Sample', 'Model', 'k',
              'f\u00B2 (80%)', '\u0394R\u00B2 (80%)',
              'f\u00B2 (90%)', '\u0394R\u00B2 (90%)']
mde_rows = [
    mde_header,
    ['MIDUS II (n = 1,289)', 'Main effect',       '9',  '.011', '.011', '.014', '.014'],
    ['',                      '2-way interaction', '11', '.012', '.012', '.016', '.015'],
    ['',                      '3-way interaction', '16', '.014', '.014', '.018', '.018'],
    ['MIDUS III (n = 729)',   'Main effect',       '9',  '.020', '.020', '.025', '.025'],
    ['',                      '2-way interaction', '11', '.022', '.021', '.028', '.027'],
    ['',                      '3-way interaction', '16', '.026', '.025', '.032', '.031'],
]
mde_widths = [2.0, 1.5, 0.5, 1.0, 1.0, 1.0, 1.0]  # 8.0"
make_apa_table(doc, mde_rows, mde_widths, font_pt=10)

# --- Panel B: Power at Conventional Effect Sizes ---
add_panel_header(doc, "Panel B. Statistical Power at Conventional Effect Sizes (\u03B1 = .05)")

pwr_header = ['Sample', 'f\u00B2', '\u0394R\u00B2',
              'Main effect (k = 8)', '2-way (k = 10)', '3-way (k = 15)']
pwr_rows = [
    pwr_header,
    ['MIDUS II (n = 1,289)', '.01', '.010', '74.4%',  '69.6%',  '60.2%'],
    ['',                      '.02', '.020', '97.8%',  '96.6%',  '93.4%'],
    ['',                      '.05', '.048', '100.0%', '100.0%', '100.0%'],
    ['',                      '.10', '.091', '100.0%', '100.0%', '100.0%'],
    ['',                      '.15', '.130', '100.0%', '100.0%', '100.0%'],
    ['MIDUS III (n = 729)',   '.01', '.010', '45.4%',  '40.9%',  '33.3%'],
    ['',                      '.02', '.020', '80.3%',  '75.9%',  '66.7%'],
    ['',                      '.05', '.048', '99.8%',  '99.6%',  '99.0%'],
    ['',                      '.10', '.091', '100.0%', '100.0%', '100.0%'],
    ['',                      '.15', '.130', '100.0%', '100.0%', '100.0%'],
]
pwr_widths = [2.0, 0.7, 0.7, 1.8, 1.4, 1.4]  # 8.0"
make_apa_table(doc, pwr_rows, pwr_widths, font_pt=10)

# --- Panel C: Power for Standardized Beta Coefficients ---
add_panel_header(doc,
    "Panel C. Power for Standardized Regression Coefficients (R\u00B2\u2093 = .30)")

beta_header = ['\u03B2', 'MIDUS II (k = 8)', 'MIDUS II (k = 10)', 'MIDUS II (k = 15)',
               'MIDUS III (k = 8)', 'MIDUS III (k = 10)', 'MIDUS III (k = 15)']
beta_rows = [
    beta_header,
    ['.05',  '32.1%', '32.1%', '32.0%', '20.2%', '20.1%', '20.0%'],
    ['.08',  '66.7%', '66.7%', '66.5%', '43.4%', '43.3%', '43.1%'],
    ['.10',  '84.9%', '84.8%', '84.7%', '61.1%', '61.0%', '60.7%'],
    ['.12',  '94.8%', '94.8%', '94.7%', '76.7%', '76.6%', '76.3%'],
    ['.15',  '99.4%', '99.4%', '99.4%', '92.0%', '91.9%', '91.7%'],
    ['.20', '100.0%','100.0%','100.0%', '99.4%', '99.4%', '99.4%'],
]
beta_widths = [0.6, 1.4, 1.4, 1.4, 1.4, 1.4, 1.4]  # 9.0"
make_apa_table(doc, beta_rows, beta_widths, font_pt=10)

add_note(doc,
    "Note. k = total number of predictors in model. f\u00B2 = Cohen\u2019s (1988) "
    "effect size for multiple regression; small = .02, medium = .15, large = .35. "
    "\u0394R\u00B2 = incremental variance explained by the focal predictor. "
    "Panel C assumes multicollinearity R\u00B2\u2093 = .30 for the focal predictor "
    "with other predictors. All tests are two-tailed.")


# ===================================================================
# TABLE 2: CFA FIT AND MEASUREMENT INVARIANCE
# ===================================================================

doc.add_section()
set_landscape(doc.sections[-1])

add_title(doc, "Table 2",
    "Confirmatory Factor Analysis Model Fit and Measurement Invariance Across Gender")

# --- Panel A: Model Fit Indices ---
add_panel_header(doc, "Panel A. Four-Factor Model Fit Indices", space_before=4)

fit_header = ['Model', '\u03C7\u00B2', 'df', 'p', 'CFI', 'TLI',
              'RMSEA', 'RMSEA 90% CI', 'SRMR']
fit_rows = [
    fit_header,
    ['MIDUS 2 (N = 1,289)', '1769.25', '98', '< .001', '.839', '.803',
     '.089', '[.086, .093]', '.094'],
    ['MIDUS 3 (N = 729)',   '1061.28', '98', '< .001', '.865', '.835',
     '.085', '[.081, .090]', '.091'],
]
fit_widths = [2.0, 1.0, 0.5, 0.7, 0.7, 0.7, 0.8, 1.4, 0.7]  # 8.5"
make_apa_table(doc, fit_rows, fit_widths, font_pt=10)

# --- Panel B: MI - MIDUS 2 ---
add_panel_header(doc, "Panel B. Measurement Invariance Across Gender (MIDUS 2)")

mi_header = ['Level', '\u03C7\u00B2', 'df', 'CFI', 'RMSEA', 'SRMR',
             '\u0394CFI', '\u0394RMSEA', 'Decision']
mi_m2_rows = [
    mi_header,
    ['Configural', '1935.41', '196', '.834', '.091', '.091', '\u2014', '\u2014', '\u2014'],
    ['Metric',     '1964.99', '208', '.833', '.089', '.092', '\u2212.002', '\u2212.002', 'Supported'],
    ['Scalar',     '2068.65', '220', '.824', '.089', '.093', '\u2212.009', '< .001',     'Supported'],
    ['Strict',     '2138.55', '236', '.819', '.087', '.094', '\u2212.005', '\u2212.002', 'Supported'],
]
mi_widths = [1.2, 1.0, 0.5, 0.7, 0.8, 0.7, 0.8, 0.9, 1.0]  # 7.6"
make_apa_table(doc, mi_m2_rows, mi_widths, font_pt=10)

# --- Panel C: MI - MIDUS 3 ---
add_panel_header(doc, "Panel C. Measurement Invariance Across Gender (MIDUS 3)")

mi_m3_rows = [
    mi_header,
    ['Configural', '1186.07', '196', '.863', '.087', '.089', '\u2014', '\u2014', '\u2014'],
    ['Metric',     '1208.89', '208', '.862', '.085', '.090', '\u2212.002', '\u2212.002', 'Supported'],
    ['Scalar',     '1269.08', '220', '.855', '.084', '.091', '\u2212.007', '< .001',     'Supported'],
    ['Strict',     '1306.02', '236', '.852', '.082', '.092', '\u2212.003', '\u2212.002', 'Supported'],
]
make_apa_table(doc, mi_m3_rows, mi_widths, font_pt=10)

add_note(doc,
    "Note. Good fit criteria: CFI > .95, TLI > .95, RMSEA < .06, SRMR < .08 "
    "(Hu & Bentler, 1999). \u0394CFI < .010 supports invariance at each level "
    "(Chen, 2007). Configural = same factor structure; Metric = equal loadings; "
    "Scalar = equal intercepts; Strict = equal residual variances.")


# ===================================================================
# TABLE 3: FACTOR LOADINGS (M2 & M3 SIDE BY SIDE)
# ===================================================================

doc.add_section()
set_landscape(doc.sections[-1])

add_title(doc, "Table 3",
    "Standardized Factor Loadings Across Waves")

load_header = ['Factor', 'Item (M2)', '\u03BB', 'SE',
               'Item (M3)', '\u03BB', 'SE']
load_rows = [
    load_header,
    ['Neg Work-to-Family', 'B1SF27A', '.603', '.016', 'C1SF40A', '.668', '.018'],
    ['',                   'B1SF27B', '.807', '.012', 'C1SF40B', '.818', '.012'],
    ['',                   'B1SF27C', '.683', '.014', 'C1SF40C', '.742', '.015'],
    ['',                   'B1SF27D', '.732', '.013', 'C1SF40D', '.810', '.013'],
    ['Pos Work-to-Family', 'B1SF27E', '.710', '.017', 'C1SF40E', '.646', '.022'],
    ['',                   'B1SF27F', '.712', '.017', 'C1SF40F', '.728', '.020'],
    ['',                   'B1SF27G', '.404', '.022', 'C1SF40G', '.532', '.024'],
    ['',                   'B1SF27H', '.567', '.019', 'C1SF40H', '.606', '.023'],
    ['Neg Family-to-Work', 'B1SF27I', '.593', '.017', 'C1SF40I', '.629', '.021'],
    ['',                   'B1SF27J', '.752', '.013', 'C1SF40J', '.730', '.018'],
    ['',                   'B1SF27K', '.627', '.016', 'C1SF40K', '.631', '.020'],
    ['',                   'B1SF27L', '.761', '.013', 'C1SF40L', '.668', '.019'],
    ['Pos Family-to-Work', 'B1SF27M', '.463', '.020', 'C1SF40M', '.453', '.025'],
    ['',                   'B1SF27N', '.306', '.022', 'C1SF40N', '.237', '.029'],
    ['',                   'B1SF27O', '.905', '.016', 'C1SF40O', '.863', '.018'],
    ['',                   'B1SF27P', '.670', '.017', 'C1SF40P', '.735', '.019'],
]
load_widths = [1.8, 1.0, 0.7, 0.6, 1.0, 0.7, 0.6]  # 6.4"
make_apa_table(doc, load_rows, load_widths, font_pt=10)

add_note(doc,
    "Note. \u03BB = standardized factor loading. All loadings significant "
    "at p < .001. M2 = MIDUS 2 (N = 1,289); M3 = MIDUS 3 (N = 729).")


# ===================================================================
# TABLE 4: LATENT FACTOR CORRELATIONS
# ===================================================================

doc.add_section()
set_landscape(doc.sections[-1])

add_title(doc, "Table 4",
    "Latent Factor Correlations Across Waves")

cor_header = ['Factor 1', 'Factor 2', 'r (MIDUS 2)', 'r (MIDUS 3)']
cor_rows = [
    cor_header,
    ['Neg Work-to-Family', 'Pos Work-to-Family',  '\u2212.053',    '.009'],
    ['Neg Work-to-Family', 'Neg Family-to-Work',  '.639***',       '.670***'],
    ['Neg Work-to-Family', 'Pos Family-to-Work',  '\u2212.124***', '\u2212.157***'],
    ['Pos Work-to-Family', 'Neg Family-to-Work',  '.088**',        '.070'],
    ['Pos Work-to-Family', 'Pos Family-to-Work',  '.387***',       '.495***'],
    ['Neg Family-to-Work', 'Pos Family-to-Work',  '\u2212.165***', '\u2212.292***'],
]
cor_widths = [2.2, 2.2, 1.5, 1.5]  # 7.4"
make_apa_table(doc, cor_rows, cor_widths, font_pt=10)

add_note(doc,
    "Note. *p < .05. **p < .01. ***p < .001. "
    "MIDUS 2 N = 1,289; MIDUS 3 N = 729.")


# ===================================================================
# Save
# ===================================================================

ts = datetime.datetime.now().strftime('%Y-%m-%d')
outpath = (f'/Users/benjamin.listyg/Documents/research/dissertation/'
           f'dissertation-docs/combined_power_cfa_tables_{ts}.docx')
doc.save(outpath)
print(f"Saved: {outpath}")
