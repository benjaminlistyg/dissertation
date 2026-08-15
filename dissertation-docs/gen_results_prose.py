#!/usr/bin/env python3
"""Generate updated results_prose document with current pipeline results.

Table reference mapping (results_tables_2026-02-16.docx):
  Table 1-2:    Descriptive Statistics (M2, M3)
  Table 3-4:    Bivariate Correlations (M2, M3)
  Table 5-6:    Main Effects (M2, M3) — Panels A-D per outcome
  Table 7-8:    Gender × EF Interactions (M2, M3) — Panels A-D
  Table 9-10:   Supervisor Support × EF Interactions (M2, M3) — Panels A-D
  Table 11-12:  Coworker Support × EF Interactions (M2, M3) — Panels A-D
  Table 13-14:  Spousal Support × EF Interactions (M2, M3) — Panels A-D
  Table 15:     Cross-Wave Replicability: Primary Regressions
  Table A1-A2:  Multiverse Summary (wide; M2, M3) — Panels A-D
  Table A3-A4:  Multiverse Long (M2, M3)
  Table A5:     Cross-Wave Replicability: Multiverse
  Table A6-A12: CFA / Measurement Invariance
  Table A13-A20: Power Analyses
"""

from docx import Document
from docx.shared import Pt, Inches
from docx.enum.text import WD_ALIGN_PARAGRAPH
import datetime

doc = Document()

# Set default font
style = doc.styles['Normal']
font = style.font
font.name = 'Times New Roman'
font.size = Pt(12)

# Paragraph format
for s in doc.styles:
    try:
        s.paragraph_format.line_spacing = 2.0
    except:
        pass

def add_heading_text(text, level=1):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = p.add_run(text)
    run.bold = True
    run.font.size = Pt(12)
    run.font.name = 'Times New Roman'

def add_body(text):
    p = doc.add_paragraph()
    p.paragraph_format.first_line_indent = Inches(0.5)
    p.paragraph_format.space_after = Pt(0)
    run = p.add_run(text)
    run.font.size = Pt(12)
    run.font.name = 'Times New Roman'
    return p

# Title
add_heading_text("CHAPTER 5")
add_heading_text("Results")
doc.add_paragraph()  # blank line

# --- Descriptive Statistics ---
add_body(
    "Descriptive statistics and bivariate correlations for all study variables "
    "are presented in Tables 1 through 4. In MIDUS 2 (N = 1,289; Table 1), "
    "participants reported moderate levels of negative work-to-family spillover "
    "(M = 10.42, SD = 2.67) and negative family-to-work spillover "
    "(M = 8.31, SD = 2.34), with positive spillover scores somewhat higher for "
    "both family-to-work (M = 13.91, SD = 2.62) and work-to-family "
    "(M = 11.70, SD = 2.63) directions. The mean BTACT composite score was "
    "0.33 (SD = 0.88). The sample was restricted to respondents who reported "
    "being currently employed. Descriptive statistics for MIDUS 3 "
    "(N = 729; Table 2) were similar, with slightly lower mean executive "
    "functioning (M = 0.18, SD = 0.61), consistent with the longitudinal aging "
    "of the MIDUS cohort."
)

add_body(
    "Bivariate correlations (Tables 3 and 4) indicated that executive "
    "functioning was weakly correlated with the spillover outcomes. In MIDUS 2, "
    "executive functioning was significantly correlated with negative "
    "work-to-family spillover (r = .055, p < .05), negative family-to-work "
    "spillover (r = .081, p < .01), and positive work-to-family spillover "
    "(r = .058, p < .05), though all associations were small in magnitude. In "
    "MIDUS 3, the only significant zero-order correlation between executive "
    "functioning and a spillover variable was with negative work-to-family "
    "spillover (r = .094, p < .05). Across both waves, job demands and family "
    "demands showed the strongest associations with spillover, and the three "
    "social support variables were intercorrelated and associated with lower "
    "negative and higher positive spillover."
)

# --- Analytic Approach ---
add_body(
    "A series of multiple linear regression models were estimated to test all "
    "hypotheses and research questions. Two waves of the MIDUS study were "
    "analyzed separately to assess replicability: MIDUS 2 (N = 1,289) and "
    "MIDUS 3 (N = 729). All models controlled for job demands, family demands, "
    "household income, household size, number of other household members, age, "
    "gender (where not serving as a moderator), and education. Both "
    "unstandardized (B) and standardized (\u03B2) coefficients are reported, along "
    "with 95% confidence intervals and exact p-values. Results for each "
    "regression model are organized into four panels corresponding to the "
    "four spillover outcomes. Results are interpreted "
    "with attention to effect sizes and consistency across waves, not solely "
    "p-values."
)

# --- H1-H4: Main Effects ---
add_body(
    "Hypotheses 1 through 4 predicted that executive functioning would be "
    "negatively related to negative spillover (H1: negative work-to-family; "
    "H2: negative family-to-work) and positively related to positive spillover "
    "(H3: positive work-to-family; H4: positive family-to-work). Results for "
    "these main effect models are presented in Tables 5 and 6."
)

add_body(
    "In MIDUS 2 (Table 5), the main effect models explained substantial "
    "variance in negative spillover (R\u00B2 = .320 for negative work-to-family, "
    "Panel A; R\u00B2 = .283 for negative family-to-work, Panel B) but "
    "considerably less in positive spillover (R\u00B2 = .040 for positive "
    "work-to-family, Panel C; R\u00B2 = .056 for positive family-to-work, "
    "Panel D). Executive functioning was not significantly related to "
    "negative work-to-family spillover (B = \u22120.010, \u03B2 = \u22120.003, SE = 0.079, "
    "p = .901, 95% CI [\u22120.164, 0.145]), failing to support H1. Executive "
    "functioning was also not significantly related to negative family-to-work "
    "spillover (B = 0.063, \u03B2 = 0.024, SE = 0.071, p = .374, "
    "95% CI [\u22120.076, 0.203]), failing to support H2. Executive functioning "
    "was not significantly related to positive work-to-family spillover "
    "(B = 0.078, \u03B2 = 0.026, SE = 0.092, p = .399, "
    "95% CI [\u22120.103, 0.259]), failing to support H3. Executive functioning "
    "was also not significantly related to positive family-to-work spillover "
    "(B = \u22120.157, \u03B2 = \u22120.053, SE = 0.091, p = .085, "
    "95% CI [\u22120.336, 0.021]), failing to support H4. Among the control "
    "variables, job demands was the strongest predictor of negative "
    "work-to-family spillover (\u03B2 = .422, p < .001) and family demands was "
    "the strongest predictor of negative family-to-work spillover "
    "(\u03B2 = .385, p < .001). Education was positively associated with both "
    "forms of positive spillover."
)

add_body(
    "In MIDUS 3 (Table 6), the pattern was similar. Models explained more "
    "variance in negative spillover (R\u00B2 = .409 for negative work-to-family, "
    "Panel A; R\u00B2 = .343 for negative family-to-work, Panel B) than positive "
    "spillover (R\u00B2 = .040 for positive work-to-family, Panel C; R\u00B2 = .086 "
    "for positive family-to-work, Panel D). Executive functioning was not "
    "significantly related to any of the four spillover outcomes (all "
    "ps > .05), providing no support for H1 through H4. The strongest "
    "predictors of spillover remained job demands and family demands, "
    "consistent with MIDUS 2."
)

add_body(
    "In summary, H1 through H4 were not supported. Executive functioning did "
    "not demonstrate the expected main effects on work-family spillover in "
    "either wave."
)

# --- H5-H6, RQ1-RQ2: Gender Moderation ---
add_body(
    "Hypotheses 5 and 6 predicted that gender would moderate the relationship "
    "between executive functioning and negative spillover, such that the "
    "negative relationship would be stronger for women. Research questions 1 "
    "and 2 explored whether gender moderated the relationship between "
    "executive functioning and positive spillover. Results are presented in "
    "Tables 7 and 8."
)

add_body(
    "In MIDUS 2 (Table 7), the EF \u00D7 Gender interaction was not significant "
    "for any of the four spillover outcomes: negative family-to-work "
    "(B = 0.082, \u03B2 = 0.046, p = .523; Panel A), negative work-to-family "
    "(B = 0.273, \u03B2 = 0.136, p = .054; Panel B), positive family-to-work "
    "(B = \u22120.114, \u03B2 = \u22120.058, p = .489; Panel C), or positive work-to-family "
    "(B = 0.056, \u03B2 = 0.029, p = .735; Panel D). H5 and H6 were not "
    "supported, and RQ1 and RQ2 revealed no significant gender moderation of "
    "positive spillover."
)

add_body(
    "In MIDUS 3 (Table 8), the EF \u00D7 Gender interaction was significant for "
    "negative family-to-work spillover (B = \u22120.556, \u03B2 = \u22120.213, SE = 0.237, "
    "p = .019, 95% CI [\u22121.020, \u22120.091]; Panel A), indicating that the "
    "association between executive functioning and negative family-to-work "
    "spillover differed by gender. The conditional main effect of executive "
    "functioning on positive family-to-work spillover also reached "
    "significance in this model (B = \u22120.921, \u03B2 = \u22120.214, SE = 0.467, "
    "p = .049; Panel C). However, no EF \u00D7 Gender interactions were "
    "significant in MIDUS 2, and these MIDUS 3 findings should be interpreted "
    "cautiously given the lack of cross-wave replication. In summary, gender "
    "did not consistently moderate the relationship between executive "
    "functioning and spillover across waves."
)

# --- H7-H8, RQ3-RQ4: Supervisor Moderation ---
add_body(
    "Hypotheses 7 and 8 predicted that supervisor support would buffer the "
    "negative relationship between executive functioning and negative "
    "spillover. Research questions 3 and 4 explored whether supervisor support "
    "moderated the relationship between executive functioning and positive "
    "spillover. Results are presented in Tables 9 and 10."
)

add_body(
    "In MIDUS 2 (Table 9), the EF \u00D7 Supervisor Support interaction was not "
    "significant for any of the four spillover outcomes: negative family-to-work "
    "(B = 0.030, \u03B2 = 0.124, p = .204; Panel A), negative work-to-family "
    "(B = 0.003, \u03B2 = 0.011, p = .907; Panel B), positive family-to-work "
    "(B = \u22120.052, \u03B2 = \u22120.196, p = .079; Panel C), or positive work-to-family "
    "(B = \u22120.039, \u03B2 = \u22120.145, p = .193; Panel D), failing to support H7 "
    "and H8. RQ3 and RQ4 also revealed no significant supervisor support "
    "moderation of positive spillover in MIDUS 2."
)

add_body(
    "In MIDUS 3 (Table 10), the EF \u00D7 Supervisor Support interaction was not "
    "significant for negative family-to-work (B = 0.064, \u03B2 = 0.198, "
    "p = .136; Panel A) or negative work-to-family spillover (B = 0.016, "
    "\u03B2 = 0.039, p = .752; Panel B), again failing to support H7 and H8. "
    "The interaction was not significant for positive family-to-work spillover "
    "(B = 0.012, \u03B2 = 0.033, p = .832; Panel C). However, the "
    "EF \u00D7 Supervisor Support interaction was significant for positive "
    "work-to-family spillover (B = 0.163, \u03B2 = 0.427, SE = 0.060, "
    "p = .007, 95% CI [0.045, 0.282]; Panel D; RQ4), indicating that "
    "supervisor support strengthened the positive association between "
    "executive functioning and positive work-to-family spillover. The "
    "conditional main effect of executive functioning was also significant "
    "in this model (B = \u22121.960, \u03B2 = \u22120.446, SE = 0.692, p = .005, "
    "95% CI [\u22123.317, \u22120.603]), representing the effect of executive "
    "functioning when supervisor support is at its mean."
)

add_body(
    "In summary, H7 and H8 were not supported in either wave. There was some "
    "evidence that supervisor support moderated the relationship between "
    "executive functioning and positive work-to-family spillover (RQ4), "
    "though this effect was significant only in MIDUS 3 and not in MIDUS 2."
)

# --- H9-H10, RQ5-RQ6: Coworker Moderation ---
add_body(
    "Hypotheses 9 and 10 predicted that coworker support would buffer the "
    "negative relationship between executive functioning and negative "
    "spillover. Research questions 5 and 6 explored whether coworker support "
    "moderated the relationship between executive functioning and positive "
    "spillover. Results are presented in Tables 11 and 12."
)

add_body(
    "In MIDUS 2 (Table 11), the EF \u00D7 Coworker Support interaction was "
    "significant for negative work-to-family spillover (B = 0.122, "
    "\u03B2 = 0.300, SE = 0.049, p = .014, 95% CI [0.025, 0.218]; Panel B), "
    "suggesting that the association between executive functioning and "
    "negative work-to-family spillover varied as a function of coworker "
    "support. The conditional main effect of executive functioning was also "
    "significant in this model (B = \u22120.914, \u03B2 = \u22120.300, SE = 0.370, "
    "p = .014, 95% CI [\u22121.639, \u22120.189]). The interaction was not significant "
    "for negative family-to-work (B = 0.026, p = .558; Panel A), positive "
    "family-to-work (B = \u22120.071, p = .213; Panel C), or positive "
    "work-to-family spillover (B = \u22120.050, p = .382; Panel D)."
)

add_body(
    "In MIDUS 3 (Table 12), the EF \u00D7 Coworker Support interaction was not "
    "significant for any spillover outcome (all ps > .05). The MIDUS 2 "
    "finding for negative work-to-family spillover did not replicate. "
    "Coworker support maintained significant main effects on negative "
    "spillover in expected directions. In summary, H9 and H10 were not "
    "consistently supported, and RQ5 and RQ6 revealed no replicable coworker "
    "support moderation effects across waves."
)

# --- H11-H12, RQ7-RQ8: Spousal Moderation ---
add_body(
    "Hypotheses 11 and 12 predicted that spousal support would buffer the "
    "negative relationship between executive functioning and negative "
    "spillover. Research questions 7 and 8 explored whether spousal support "
    "moderated the relationship between executive functioning and positive "
    "spillover. Results are presented in Tables 13 and 14."
)

add_body(
    "In MIDUS 2 (Table 13), the EF \u00D7 Spousal Support interaction was not "
    "significant for any of the four spillover outcomes (all ps > .05). "
    "Spousal support demonstrated significant main effects, most notably a "
    "strong positive association with positive family-to-work spillover "
    "(R\u00B2 = .195 for positive family-to-work in this model, Panel C). In "
    "MIDUS 3 (Table 14), the EF \u00D7 Spousal Support interaction was again "
    "not significant for any spillover outcome (all ps > .05). Spousal "
    "support maintained its strong associations with spillover outcomes. In "
    "summary, H11 and H12 were not supported, and RQ7 and RQ8 revealed no "
    "significant spousal support moderation effects in either wave."
)

# --- Replicability ---
add_body(
    "To formally assess replicability, the Open Science Collaboration (2015) "
    "criteria were applied to evaluate whether effects that reached "
    "significance in MIDUS 2 replicated in MIDUS 3 (Table 15). Of the two "
    "executive functioning\u2013related terms that reached significance in MIDUS 2 "
    "(the EF conditional main effect and the EF \u00D7 Coworker Support "
    "interaction, both for negative work-to-family spillover), neither showed "
    "the same sign in MIDUS 3 (0/2, 0.0%), and neither reached significance "
    "in MIDUS 3 with the same sign (0/2, 0.0%). These results indicate that "
    "the significant effects observed in the primary analyses did not "
    "replicate across waves."
)

add_body(
    "Across all models, the most consistent predictors of work-family "
    "spillover were job demands (positively related to negative spillover) "
    "and family demands (positively related to negative family-to-work "
    "spillover and negatively related to positive family-to-work spillover). "
    "Social support variables\u2014supervisor, coworker, and spousal\u2014demonstrated "
    "significant main effects in expected directions across most models, even "
    "though they did not consistently moderate the executive functioning\u2013"
    "spillover relationship. The primary finding is that executive "
    "functioning, as measured by the BTACT composite, did not demonstrate "
    "robust main or interactive effects on work-family spillover. None of the "
    "12 directional hypotheses received consistent support across waves. "
    "Several isolated significant effects emerged in one wave but did not "
    "replicate in the other, limiting confidence in their robustness."
)

# --- Appendix: Multiverse Analyses ---
doc.add_paragraph()
add_heading_text("APPENDIX")
add_heading_text("Multiverse Analyses")
doc.add_paragraph()

add_body(
    "To assess the robustness of the primary findings, a multiverse analysis "
    "was conducted in which the executive functioning\u2013spillover relationship "
    "was estimated across several alternative analytic specifications. These "
    "specifications were organized into four arms: (A) covariate variations "
    "(no covariates, demographics only, education as SES indicator); "
    "(B) covariate \u00D7 EF interactions (age, education, income, job demands, "
    "and family demands each interacted with EF, for all four spillover "
    "outcomes); (C) dual-earner couples (restricting the sample to households "
    "where both the respondent and spouse were employed, running all 20 "
    "regression models as a robustness check); and (D) model-set specific "
    "complete-case samples. Summary results are presented in Tables A1 and "
    "A2 (panels by outcome), with full specification-level results in "
    "Tables A3 and A4, and cross-wave replicability assessed in Table A5."
)

add_body(
    "In MIDUS 2 (Tables A1 and A3), when no covariates were included, "
    "executive functioning was positively related to both negative "
    "work-to-family spillover (B = 0.250, \u03B2 = 0.084, p < .001) and negative "
    "family-to-work spillover (B = 0.246, \u03B2 = 0.094, p < .001). However, "
    "these effects disappeared once any control variables were introduced. "
    "In the covariate interaction arm, the Age \u00D7 EF interaction was "
    "significant for positive work-to-family spillover (B = \u22120.018, p < .05), "
    "and the Family Demands \u00D7 EF interaction was significant for negative "
    "family-to-work spillover (B = 0.007, p < .05). In the dual-earner arm, "
    "EF was significantly related to positive family-to-work spillover "
    "(B = \u22120.222, p < .05), and the EF \u00D7 Supervisor Support interaction was "
    "significant for positive family-to-work spillover (B = \u22120.085, p < .05). "
    "The EF \u00D7 Coworker Support interaction was significant for negative "
    "work-to-family spillover in the dual-earner subsample (B = 0.122, "
    "p < .05), consistent with the primary analysis finding."
)

add_body(
    "In MIDUS 3 (Tables A2 and A4), the no-covariate models again showed "
    "significant positive associations between executive functioning and "
    "negative work-to-family spillover (B = 0.538, \u03B2 = 0.119, p < .001) and "
    "negative family-to-work spillover (B = 0.253, \u03B2 = 0.069, p < .01). "
    "Several covariate \u00D7 EF interactions reached significance, including "
    "Job Demands \u00D7 EF for negative work-to-family spillover and negative "
    "family-to-work spillover. In the dual-earner arm, the EF \u00D7 Supervisor "
    "Support interaction was significant for positive work-to-family spillover "
    "(B = 0.235, p < .001), though the direction differed from MIDUS 2. "
    "The EF \u00D7 Gender interaction was significant for positive family-to-work "
    "spillover in the dual-earner subsample (B = \u22121.132, p < .05)."
)

add_body(
    "Cross-wave replicability of the multiverse results (Table A5) was "
    "evaluated using the same OSC (2015) criteria applied to the primary "
    "analyses. Of the nine specifications that produced significant effects "
    "in MIDUS 2, four showed the same sign in MIDUS 3 (44.4%), three reached "
    "significance with the same sign in the second wave (33.3%), and of the "
    "four effects that were significant in both waves, three had overlapping "
    "confidence intervals (75.0%). Notably, the no-covariate models for "
    "negative work-to-family and negative family-to-work spillover met all "
    "three replicability criteria, as did the Job Demands \u00D7 EF interaction "
    "for negative family-to-work spillover. These results suggest that the "
    "positive zero-order association between executive functioning and "
    "negative spillover is replicable but is fully attenuated by the "
    "inclusion of covariates, reinforcing the primary finding that executive "
    "functioning does not have a robust or unique relationship with "
    "work-family spillover."
)

# Save
ts = datetime.datetime.now().strftime('%Y-%m-%d')
outpath = f'/Users/benjamin.listyg/Documents/research/dissertation/dissertation-docs/results_prose_{ts}.docx'
doc.save(outpath)
print(f"Saved: {outpath}")
