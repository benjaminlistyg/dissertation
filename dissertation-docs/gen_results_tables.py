#!/usr/bin/env python3
"""Generate updated results_tables document with current pipeline results.

Reads the HTML output tables and converts them to Word format.
"""

from docx import Document
from docx.shared import Pt, Inches, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.oxml.ns import qn
import re
import os
import glob
import datetime

BASE = '/Users/benjamin.listyg/Documents/research/dissertation/dissertation-code/dissertation_code/output/tables'

doc = Document()

# Set default font
style = doc.styles['Normal']
font = style.font
font.name = 'Times New Roman'
font.size = Pt(10)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

def parse_html_table(html_content):
    """Parse HTML table into list of rows, each row is list of cell text."""
    rows = []
    tr_pattern = re.compile(r'<tr[^>]*>(.*?)</tr>', re.DOTALL)
    td_pattern = re.compile(r'<t[dh][^>]*>(.*?)</t[dh]>', re.DOTALL)
    for tr_match in tr_pattern.finditer(html_content):
        tr_content = tr_match.group(1)
        cells = []
        for td_match in td_pattern.finditer(tr_content):
            cell_text = re.sub(r'<[^>]+>', '', td_match.group(1)).strip()
            cell_text = cell_text.replace('&lt;', '<').replace('&gt;', '>')
            cell_text = cell_text.replace('&amp;', '&')
            cells.append(cell_text)
        if cells:
            rows.append(cells)
    return rows


def extract_caption(html_content):
    """Extract caption text from HTML."""
    match = re.search(r'<caption[^>]*>(.*?)</caption>', html_content, re.DOTALL)
    if match:
        text = re.sub(r'<[^>]+>', '', match.group(1)).strip()
        return text
    return ""


def extract_note(rows):
    """Find and separate the Note row from the data rows."""
    note_text = ""
    data_rows = []
    for row in rows:
        if row and row[0].startswith('Note.'):
            note_text = row[0]
        else:
            data_rows.append(row)
    return data_rows, note_text


def add_table_title(doc, number, title):
    """Add APA-formatted table title."""
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(2)
    p.paragraph_format.space_before = Pt(12)
    run = p.add_run(f"Table {number}")
    run.bold = True
    run.font.size = Pt(10)
    run.font.name = 'Times New Roman'

    p2 = doc.add_paragraph()
    p2.paragraph_format.space_after = Pt(4)
    run2 = p2.add_run(title)
    run2.italic = True
    run2.font.size = Pt(10)
    run2.font.name = 'Times New Roman'


def add_note(doc, note_text):
    """Add table note in APA format."""
    if not note_text:
        return
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(2)
    run = p.add_run(note_text)
    run.font.size = Pt(8)
    run.font.name = 'Times New Roman'
    run.italic = True


def add_word_table(doc, rows, note_text=""):
    """Add a Word table from parsed HTML rows."""
    if not rows:
        return

    # Determine max columns
    max_cols = max(len(r) for r in rows)
    if max_cols == 0:
        return

    table = doc.add_table(rows=len(rows), cols=max_cols)
    table.alignment = WD_TABLE_ALIGNMENT.CENTER
    table.style = 'Table Grid'

    for i, row in enumerate(rows):
        for j, cell_text in enumerate(row):
            if j < max_cols:
                cell = table.cell(i, j)
                cell.text = cell_text
                for paragraph in cell.paragraphs:
                    paragraph.alignment = WD_ALIGN_PARAGRAPH.CENTER
                    for run in paragraph.runs:
                        run.font.size = Pt(8)
                        run.font.name = 'Times New Roman'
                # Left-align first column (variable names)
                if j == 0:
                    for paragraph in cell.paragraphs:
                        paragraph.alignment = WD_ALIGN_PARAGRAPH.LEFT
                # Bold header row
                if i == 0:
                    for paragraph in cell.paragraphs:
                        for run in paragraph.runs:
                            run.bold = True

    add_note(doc, note_text)


def process_html_file(fpath, doc, table_num, title):
    """Read an HTML table file and add it to the Word document."""
    with open(fpath, 'r') as f:
        content = f.read()
    rows = parse_html_table(content)
    if not rows:
        return
    data_rows, note_text = extract_note(rows)

    add_table_title(doc, table_num, title)
    add_word_table(doc, data_rows, note_text)
    doc.add_paragraph()  # spacing


# ---------------------------------------------------------------------------
# Build the document
# ---------------------------------------------------------------------------

# Title page
p = doc.add_paragraph()
p.alignment = WD_ALIGN_PARAGRAPH.CENTER
run = p.add_run("Results Tables")
run.bold = True
run.font.size = Pt(14)
run.font.name = 'Times New Roman'

p2 = doc.add_paragraph()
p2.alignment = WD_ALIGN_PARAGRAPH.CENTER
ts = datetime.datetime.now().strftime('%Y-%m-%d')
run2 = p2.add_run(f"Generated: {ts}")
run2.font.size = Pt(10)
run2.font.name = 'Times New Roman'

doc.add_page_break()

# --- Regression Tables (Tables 1-10) ---
reg_tables = [
    ('table01_main_effects_m2.html', 1, 'Main Effects of Executive Functioning on Work-Family Spillover (MIDUS 2)'),
    ('table02_main_effects_m3.html', 2, 'Main Effects of Executive Functioning on Work-Family Spillover (MIDUS 3)'),
    ('table03_gender_moderation_m2.html', 3, 'Gender \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table04_gender_moderation_m3.html', 4, 'Gender \u00D7 Executive Functioning Interactions (MIDUS 3)'),
    ('table05_supervisor_moderation_m2.html', 5, 'Supervisor Support \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table06_supervisor_moderation_m3.html', 6, 'Supervisor Support \u00D7 Executive Functioning Interactions (MIDUS 3)'),
    ('table07_coworker_moderation_m2.html', 7, 'Coworker Support \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table08_coworker_moderation_m3.html', 8, 'Coworker Support \u00D7 Executive Functioning Interactions (MIDUS 3)'),
    ('table09_spousal_moderation_m2.html', 9, 'Spousal Support \u00D7 Executive Functioning Interactions (MIDUS 2)'),
    ('table10_spousal_moderation_m3.html', 10, 'Spousal Support \u00D7 Executive Functioning Interactions (MIDUS 3)'),
]

for fname, tnum, title in reg_tables:
    fpath = os.path.join(BASE, 'regressions', fname)
    if os.path.isfile(fpath):
        process_html_file(fpath, doc, tnum, title)
        doc.add_page_break()

# --- Multiverse Tables (Tables 11-14) ---
mv_tables = [
    ('multiverse_long_m2.html', 11, 'Multiverse Analysis: EF Effects Across Specifications (MIDUS 2)'),
    ('multiverse_wide_m2.html', 12, 'Multiverse Summary: EF Main Effects (MIDUS 2)'),
    ('multiverse_long_m3.html', 13, 'Multiverse Analysis: EF Effects Across Specifications (MIDUS 3)'),
    ('multiverse_wide_m3.html', 14, 'Multiverse Summary: EF Main Effects (MIDUS 3)'),
]

for fname, tnum, title in mv_tables:
    fpath = os.path.join(BASE, 'multiverse', fname)
    if os.path.isfile(fpath):
        process_html_file(fpath, doc, tnum, title)
        doc.add_page_break()

# --- Descriptives Tables (Tables 30-33) ---
desc_tables = [
    ('descriptives_m2.html', 30, 'Descriptive Statistics (MIDUS 2)'),
    ('descriptives_m3.html', 31, 'Descriptive Statistics (MIDUS 3)'),
    ('correlations_m2.html', 32, 'Bivariate Correlations (MIDUS 2)'),
    ('correlations_m3.html', 33, 'Bivariate Correlations (MIDUS 3)'),
]

for fname, tnum, title in desc_tables:
    fpath = os.path.join(BASE, 'descriptives', fname)
    if os.path.isfile(fpath):
        process_html_file(fpath, doc, tnum, title)
        doc.add_page_break()

# --- Replicability Tables (Tables 34-35) ---
rep_tables = [
    ('replicability_regressions.html', 34, 'Cross-Wave Replicability of Primary Regressions (OSC, 2015)'),
    ('replicability_multiverse.html', 35, 'Cross-Wave Replicability of Multiverse Specifications (OSC, 2015)'),
]

for fname, tnum, title in rep_tables:
    fpath = os.path.join(BASE, 'replicability', fname)
    if os.path.isfile(fpath):
        process_html_file(fpath, doc, tnum, title)
        doc.add_page_break()

# --- CFA Tables (Tables 15-21) ---
cfa_tables = [
    ('cfa_fit_indices.html', 15, 'Confirmatory Factor Analysis Model Fit Indices'),
    ('cfa_loadings_m2.html', 16, 'Standardized Factor Loadings (MIDUS 2)'),
    ('cfa_factor_correlations_m2.html', 17, 'Latent Factor Correlations (MIDUS 2)'),
    ('cfa_loadings_m3.html', 18, 'Standardized Factor Loadings (MIDUS 3)'),
    ('cfa_factor_correlations_m3.html', 19, 'Latent Factor Correlations (MIDUS 3)'),
    ('invariance_gender_m2.html', 20, 'Measurement Invariance Across Gender (MIDUS 2)'),
    ('invariance_gender_m3.html', 21, 'Measurement Invariance Across Gender (MIDUS 3)'),
]

for fname, tnum, title in cfa_tables:
    fpath = os.path.join(BASE, 'cfa', fname)
    if os.path.isfile(fpath):
        process_html_file(fpath, doc, tnum, title)
        doc.add_page_break()

# --- Power Tables (Tables 22-29) ---
power_tables = [
    ('power_mde.html', 22, 'Minimum Detectable Effect Sizes (\u03B1 = .05)'),
    ('power_fixed_effects.html', 23, 'Statistical Power at Fixed Effect Sizes (\u03B1 = .05)'),
    ('power_beta_midusii_k8.html', 24, 'Power by Standardized Effect Size: MIDUS 2 (k = 8)'),
    ('power_beta_midusii_k10.html', 25, 'Power by Standardized Effect Size: MIDUS 2 (k = 10)'),
    ('power_beta_midusii_k15.html', 26, 'Power by Standardized Effect Size: MIDUS 2 (k = 15)'),
    ('power_beta_midusiii_k8.html', 27, 'Power by Standardized Effect Size: MIDUS 3 (k = 8)'),
    ('power_beta_midusiii_k10.html', 28, 'Power by Standardized Effect Size: MIDUS 3 (k = 10)'),
    ('power_beta_midusiii_k15.html', 29, 'Power by Standardized Effect Size: MIDUS 3 (k = 15)'),
]

for fname, tnum, title in power_tables:
    fpath = os.path.join(BASE, 'power', fname)
    if os.path.isfile(fpath):
        process_html_file(fpath, doc, tnum, title)
        doc.add_page_break()

# Save
outpath = f'/Users/benjamin.listyg/Documents/research/dissertation/dissertation-docs/results_tables_{ts}.docx'
doc.save(outpath)
print(f"Saved: {outpath}")
print(f"Total tables: {len(reg_tables) + len(mv_tables) + len(desc_tables) + len(rep_tables) + len(cfa_tables) + len(power_tables)}")
