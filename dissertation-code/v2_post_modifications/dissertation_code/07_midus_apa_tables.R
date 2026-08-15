# =============================================================================
# MIDUS DISSERTATION - COMPREHENSIVE APA FORMAT TABLES
# Covers: Regressions, Multiverse, CFA/MI, and Power Analyses
# =============================================================================

library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)
library(lavaan)
library(semTools)

# =============================================================================
# OUTPUT DIRECTORY STRUCTURE
# =============================================================================
#
#   output/
#   └── tables/
#       ├── regressions/    Tables 1-10: main effects & moderation regressions
#       ├── multiverse/     Specification-curve / robustness tables
#       ├── cfa/            CFA fit, loadings, correlations, invariance
#       ├── power/          MDE, fixed-effect power, beta power tables
#       ├── descriptives/   Descriptive statistics & correlation matrices
#       └── replicability/  Cross-wave OSC (2015) replicability tables
#
# =============================================================================

OUTPUT_DIR <- file.path("..", "output", "tables")

DIRS <- list(
  regressions             = file.path(OUTPUT_DIR, "regressions"),
  regressions_uncentered  = file.path(OUTPUT_DIR, "regressions_uncentered"),
  multiverse              = file.path(OUTPUT_DIR, "multiverse"),
  cfa                     = file.path(OUTPUT_DIR, "cfa"),
  power                   = file.path(OUTPUT_DIR, "power"),
  descriptives            = file.path(OUTPUT_DIR, "descriptives"),
  replicability           = file.path(OUTPUT_DIR, "replicability")
)

# Create directories if they don't exist
invisible(lapply(DIRS, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE)))

# =============================================================================
# CORE HELPER FUNCTIONS
# =============================================================================

# --- Save a kable table to HTML (no Viewer rendering) ---
apa_save_html <- function(tbl, filename, dir = OUTPUT_DIR) {
  path <- file.path(dir, filename)
  save_kable(tbl, file = path)
  message("Table saved to: ", path)
  # Intentionally do NOT print(tbl) to the RStudio Viewer; tables are
  # written to disk only.
  invisible(tbl)
}

format_p_stars <- function(p) {
  case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  )
}

format_p_apa <- function(p) {
  case_when(
    p < .001 ~ "< .001",
    TRUE     ~ sub("^0", "", sprintf("%.3f", p))
  )
}

# APA 7th edition caption: bold table number + italic title.
# Currently disabled — every call site passes its (number, title) but we
# return NULL so kable emits no in-table caption. The table number and
# italic title belong with the figure/table number in the dissertation
# document, not inside the HTML output.
# To restore in-table captions, change the body back to:
#   paste0("<b>Table ", number, "</b><br><i>", title, "</i>")
apa_caption <- function(number, title) {
  NULL
}

# Variable name labels (MIDUS 2 and 3)
var_labels <- c(
  "(Intercept)" = "Intercept",
  # MIDUS 2
  "B3TEFZ1" = "Executive Functioning", "B1SJCDS_mean" = "Job Demands",
  "FAM_DEMANDS_M2" = "Family Demands", "B1STINC1" = "Household Income",
  "B1PHHSIZE_oth" = "Household Size",
  "B1PAGE_M2" = "Age", "B1PRSEX" = "Gender", "B1PB1" = "Education",
  "B1SJCSS_mean" = "Supervisor Support", "B1SJCCS_mean" = "Coworker Support",
  "B1SSPEMP_mean" = "Spousal Support",
  # MIDUS 3
  "C3TEF" = "Executive Functioning", "C1SJCDS_mean" = "Job Demands",
  "FAM_DEMANDS_M3" = "Family Demands", "C1STINC" = "Household Income",
  "C1PHHSIZE_oth" = "Household Size",
  "C1PRAGE" = "Age", "C1PRSEX" = "Gender", "C1PB1" = "Education",
  "C1SJCSS_mean" = "Supervisor Support", "C1SJCCS_mean" = "Coworker Support",
  "C1SSPEMP_mean" = "Spousal Support",
  # Interactions
  "B3TEFZ1:B1PRSEX" = "EF x Gender",
  "B3TEFZ1:B1SJCSS_mean" = "EF x Supervisor Support",
  "B3TEFZ1:B1SJCCS_mean" = "EF x Coworker Support",
  "B3TEFZ1:B1SSPEMP_mean" = "EF x Spousal Support",
  "C3TEF:C1PRSEX" = "EF x Gender",
  "C3TEF:C1SJCSS_mean" = "EF x Supervisor Support",
  "C3TEF:C1SJCCS_mean" = "EF x Coworker Support",
  "C3TEF:C1SSPEMP_mean" = "EF x Spousal Support",
  # Multiverse interaction labels
  "B3TEFZ1:B1PAGE_M2" = "EF x Age",
  "B3TEFZ1:B1PB1" = "EF x Education",
  "B3TEFZ1:B1STINC1" = "EF x Income",
  "B3TEFZ1:B1SJCDS_mean" = "EF x Job Demands",
  "B3TEFZ1:FAM_DEMANDS_M2" = "EF x Family Demands",
  "C3TEF:C1PRAGE" = "EF x Age",
  "C3TEF:C1PB1" = "EF x Education",
  "C3TEF:C1STINC" = "EF x Income",
  "C3TEF:C1SJCDS_mean" = "EF x Job Demands",
  "C3TEF:FAM_DEMANDS_M3" = "EF x Family Demands"
,
  # ---- Centered-variable aliases (v2 centering: EF + 3 supports + 2 demands) ----
  "B3TEFZ1_c" = "Executive Functioning", "C3TEF_c" = "Executive Functioning",
  "B1SJCSS_c" = "Supervisor Support", "C1SJCSS_c" = "Supervisor Support",
  "B1SJCCS_c" = "Coworker Support",   "C1SJCCS_c" = "Coworker Support",
  "B1SSPEMP_c" = "Spousal Support",    "C1SSPEMP_c" = "Spousal Support",
  "B1SJCDS_c" = "Job Demands",         "C1SJCDS_c" = "Job Demands",
  "FAM_DEMANDS_M2_c" = "Family Demands","FAM_DEMANDS_M3_c" = "Family Demands",
  # ---- Interaction terms with centered names (both orderings) ----
  "B3TEFZ1_c:B1PRSEX" = "EF x Gender", "B1PRSEX:B3TEFZ1_c" = "EF x Gender",
  "C3TEF_c:C1PRSEX"   = "EF x Gender", "C1PRSEX:C3TEF_c"   = "EF x Gender",
  "B3TEFZ1_c:B1SJCSS_c" = "EF x Supervisor Support",
  "B1SJCSS_c:B3TEFZ1_c" = "EF x Supervisor Support",
  "C3TEF_c:C1SJCSS_c"   = "EF x Supervisor Support",
  "C1SJCSS_c:C3TEF_c"   = "EF x Supervisor Support",
  "B3TEFZ1_c:B1SJCCS_c" = "EF x Coworker Support",
  "B1SJCCS_c:B3TEFZ1_c" = "EF x Coworker Support",
  "C3TEF_c:C1SJCCS_c"   = "EF x Coworker Support",
  "C1SJCCS_c:C3TEF_c"   = "EF x Coworker Support",
  "B3TEFZ1_c:B1SSPEMP_c" = "EF x Spousal Support",
  "B1SSPEMP_c:B3TEFZ1_c" = "EF x Spousal Support",
  "C3TEF_c:C1SSPEMP_c"   = "EF x Spousal Support",
  "C1SSPEMP_c:C3TEF_c"   = "EF x Spousal Support",
  "B3TEFZ1_c:B1SJCDS_c" = "EF x Job Demands",
  "B1SJCDS_c:B3TEFZ1_c" = "EF x Job Demands",
  "C3TEF_c:C1SJCDS_c"   = "EF x Job Demands",
  "C1SJCDS_c:C3TEF_c"   = "EF x Job Demands",
  "B3TEFZ1_c:FAM_DEMANDS_M2_c" = "EF x Family Demands",
  "FAM_DEMANDS_M2_c:B3TEFZ1_c" = "EF x Family Demands",
  "C3TEF_c:FAM_DEMANDS_M3_c"   = "EF x Family Demands",
  "FAM_DEMANDS_M3_c:C3TEF_c"   = "EF x Family Demands",
  # Centered EF × uncentered demographics (Age, Education, Income)
  "B3TEFZ1_c:B1PAGE_M2" = "EF x Age", "B1PAGE_M2:B3TEFZ1_c" = "EF x Age",
  "B3TEFZ1_c:B1PB1"     = "EF x Education", "B1PB1:B3TEFZ1_c" = "EF x Education",
  "B3TEFZ1_c:B1STINC1"  = "EF x Income", "B1STINC1:B3TEFZ1_c" = "EF x Income",
  "C3TEF_c:C1PRAGE"     = "EF x Age", "C1PRAGE:C3TEF_c" = "EF x Age",
  "C3TEF_c:C1PB1"       = "EF x Education", "C1PB1:C3TEF_c" = "EF x Education",
  "C3TEF_c:C1STINC"     = "EF x Income", "C1STINC:C3TEF_c" = "EF x Income",
  # Factor-coded gender (Male = ref) — coefficient name is *PRSEXFemale
  "B1PRSEXFemale" = "Gender", "C1PRSEXFemale" = "Gender",
  "B3TEFZ1:B1PRSEXFemale" = "EF x Gender", "B1PRSEXFemale:B3TEFZ1" = "EF x Gender",
  "C3TEF:C1PRSEXFemale"   = "EF x Gender", "C1PRSEXFemale:C3TEF"   = "EF x Gender",
  "B3TEFZ1_c:B1PRSEXFemale" = "EF x Gender", "B1PRSEXFemale:B3TEFZ1_c" = "EF x Gender",
  "C3TEF_c:C1PRSEXFemale"   = "EF x Gender", "C1PRSEXFemale:C3TEF_c"   = "EF x Gender")

label_terms <- function(term) {
  ifelse(term %in% names(var_labels), var_labels[term], term)
}

# =============================================================================
# APA 7 STYLING HELPERS
# =============================================================================
# Used by every apa_table_*() function. APA 7 tables have only three or four
# horizontal rules (top, header-bottom, optional midrule, bottom), no body row
# borders, and no vertical rules. The Note spans the full table width.

# CSS pieces; font_size in pt.
# `head` carries BOTH the 2px top rule (top of table) and the 1px bottom
# rule under the column headers. Word pastes ignore table-level borders
# but honor cell-level borders, so duplicating the top rule on the cells
# of the topmost row is required for the paste to look correct.
apa7_css <- function(font_size = 11) {
  list(
    table = paste0(
      "font-family: 'Times New Roman', Times, serif; ",
      "font-size: ", font_size, "pt; border-collapse: collapse; ",
      "margin: 0 auto; ",
      "border-top: 2px solid black; border-bottom: 2px solid black;"
    ),
    body = "border: none; padding: 2px 8px; white-space: nowrap;",
    body_wrap = "border: none; padding: 2px 8px;",  # for label columns
    head = paste0("border: none; padding: 3px 8px; ",
                  "border-top: 2px solid black; ",
                  "border-bottom: 1px solid black; white-space: nowrap;"),
    mid  = paste0("border: none; padding: 2px 8px; ",
                  "border-top: 1px solid black; white-space: nowrap;"),
    # Used to add the table-bottom rule on the cells of the last body row
    # so Word paste shows the closing rule even though it ignores the
    # <table> element's border-bottom.
    last_body = paste0("border: none; padding: 2px 8px; ",
                       "border-bottom: 2px solid black; ",
                       "white-space: nowrap;")
  )
}

# Replace kableExtra::footnote() with a <tfoot><td colspan="N"></tfoot> so
# the Note spans the full table width on browser display and Word paste.
# Modifies the kable string in place to preserve html_dependencies.
apa7_tfoot <- function(k, total_cols, full_note, font_size = 11) {
  note_html  <- gsub("\n", "<br>", full_note, fixed = TRUE)
  tfoot_html <- paste0(
    "<tfoot>",
    "<tr><td colspan=\"", total_cols, "\" ",
    "style=\"border-top: 1px solid black; padding: 8px 8px 0 8px; ",
    "font-family: 'Times New Roman', Times, serif; font-size: ",
    font_size, "pt; text-align: left; white-space: normal;\">",
    "<em>Note.</em> ", note_html,
    "</td></tr>",
    "</tfoot>"
  )
  k[1] <- sub("</table>", paste0(tfoot_html, "</table>"), k[1], fixed = TRUE)
  k
}

# Escape "<" in note text / cell content (needed because kable is rendered
# with escape = FALSE so <em>Note.</em> survives).
esc_lt <- function(s) gsub("<", "&lt;", s, fixed = TRUE)

# Non-breaking minus / non-breaking space inside CI brackets so numerics
# don't break across lines when pasted into Word.
nb_minus <- function(s) gsub("-", "‑", s, fixed = TRUE)
nb_comma <- function(s) gsub(", ", ", ", s, fixed = TRUE)

# =============================================================================
# 1. REGRESSION TABLES
# =============================================================================

# --- Standardized beta from an lm model ---
std_betas <- function(model) {
  b <- summary(model)$coef[-1, 1]
  sx <- apply(model.matrix(model)[, -1, drop = FALSE], 2, sd)
  sy <- sd(model$model[[1]])
  beta <- b * sx / sy
  return(beta)
}

# --- Single model table ---
# If `outcome_label` is supplied (e.g. "Negative Work-to-Family Spillover")
# it's injected as a centered italic-bold colspan'd row at the top of the
# table, so a standalone file remains self-explanatory without needing an
# external Table N caption.
apa_table_single <- function(model, title = "Regression Results", note = NULL,
                             outcome_label = NULL) {
  mod_summary <- summary(model)
  betas <- std_betas(model)

  tbl <- tidy(model) %>%
    mutate(
      beta = betas[term],
      term = label_terms(term),
      b_star   = paste0(nb_minus(sprintf("%.3f", estimate)),
                        format_p_stars(p.value)),
      beta_fmt = ifelse(is.na(beta), "\u2014",
                        nb_minus(sprintf("%.3f", beta))),
      se = nb_minus(sprintf("%.3f", std.error)),
      t  = nb_minus(sprintf("%.2f", statistic)),
      p_fmt = esc_lt(nb_minus(format_p_apa(p.value))),
      ci = nb_comma(nb_minus(paste0(
        "[", sprintf("%.3f", estimate - 1.96 * std.error), ", ",
        sprintf("%.3f", estimate + 1.96 * std.error), "]"))),
      sig = p.value < .05
    ) %>%
    select(term, b_star, beta_fmt, se, t, p_fmt, ci, sig)

  r2     <- sprintf("%.3f", mod_summary$r.squared)
  adj_r2 <- sprintf("%.3f", mod_summary$adj.r.squared)
  f_stat <- mod_summary$fstatistic
  fit_note <- paste0("<i>R</i>\u00B2 = ", r2,
                     ", Adjusted <i>R</i>\u00B2 = ", adj_r2,
                     ", <i>F</i>(", f_stat[2], ", ", f_stat[3], ") = ",
                     sprintf("%.2f", f_stat[1]),
                     ", <i>N</i> = ", nobs(model))
  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    fit_note,
    "\n\u03B2 = standardized coefficient. ",
    "Bold rows are significant (<i>p</i> &lt; .05).",
    "\n*<i>p</i> &lt; .05. **<i>p</i> &lt; .01. ***<i>p</i> &lt; .001."
  )

  sig_rows <- which(tbl$sig)
  n_total  <- nrow(tbl)
  total_cols <- 7L
  css <- apa7_css(11)

  k <- kable(tbl %>% select(-sig),
             col.names = c("Predictor", "<i>B</i>", "\u03B2", "<i>SE</i>",
                           "<i>t</i>", "<i>p</i>", "95% CI"),
             caption = NULL,
             align = c("l", "r", "r", "r", "r", "r", "c"),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(n_total), extra_css = css$body) %>%
    row_spec(n_total, extra_css = css$last_body)   # 2px bottom rule

  if (length(sig_rows)) k <- k %>% row_spec(sig_rows, bold = TRUE)

  # Optional outcome-label row (centered italic-bold, spans all 7 cols).
  # Injected after the opening <thead> so it sits above the column headers.
  if (!is.null(outcome_label) && nzchar(outcome_label)) {
    outcome_row <- paste0(
      "<tr><td colspan=\"", total_cols, "\" ",
      "style=\"border: none; padding: 6px 6px 4px; ",
      "text-align: center; font-style: italic; font-weight: bold; ",
      "font-size: 11pt;\">",
      outcome_label,
      "</td></tr>"
    )
    k[1] <- sub("(<thead[^>]*>)", paste0("\\1", outcome_row), k[1])
  }

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# --- Multiple models stacked vertically (long APA 7 layout) ---
# Six columns total: Predictor, B, \u03B2, SE, p, 95% CI. Each model becomes a
# section: a colspan'd outcome-label row followed by that model's predictor
# rows and three fit-statistic rows (R\u00B2, Adjusted R\u00B2, N). The table fits a
# portrait page at 12pt Times.
# --- Four outcomes in a 2x2 grid (APA 7) -------------------------------------
# Each model becomes its own 6-column mini table (Predictor + B + beta + SE +
# p + 95% CI). The outcome name (e.g., "Negative Work-to-Family Spillover")
# sits centered as an italic-bold header at the top of each quadrant. The
# 2x2 layout is a CSS grid; a single full-width Note sits below it. Font is
# 12pt Times throughout.
# --- Four outcomes laid out 2x2 inside a SINGLE <table> (APA 7) --------------
# Pasted into Word, this comes through as ONE table. The 2x2 visual is
# achieved by stacking two outcome bands top-to-bottom; each band carries
# two outcomes side-by-side, separated by a narrow spacer column.
# Layout: 13 columns total = (Pred + B + beta + SE + p + CI) + spacer +
# (Pred + B + beta + SE + p + CI). One full-width tfoot Note at the bottom.
apa_table_multi <- function(models, model_names, title = "Regression Results",
                            note = NULL) {

  stopifnot(length(models) == 4, length(model_names) == 4)

  nb_num <- function(s) gsub("-", "‑", s, fixed = TRUE)
  nb_ci  <- function(s) gsub(", ", ", ", nb_num(s), fixed = TRUE)
  esc_p  <- function(s) sub("<", "&lt;", s, fixed = TRUE)

  outcome_full <- c(
    "Neg W→F" = "Negative Work-to-Family Spillover",
    "Neg F→W" = "Negative Family-to-Work Spillover",
    "Pos W→F" = "Positive Work-to-Family Spillover",
    "Pos F→W" = "Positive Family-to-Work Spillover"
  )
  expand_name <- function(n) {
    if (n %in% names(outcome_full)) unname(outcome_full[n]) else n
  }

  # Column widths (px @ 96 dpi). Two outcome-blocks of 410 px each + 16 px
  # spacer = 836 px ≈ 8.7 in. Fits Letter landscape at 0.5" margins.
  col_w <- c(Pred = 130L, B = 54L, beta = 36L, SE = 42L, p = 42L, CI = 102L)
  spacer_w <- 16L
  total_w  <- 2L * sum(col_w) + spacer_w
  total_cols <- 13L

  # --- Styles ---
  apa_css <- paste0(
    "font-family: 'Times New Roman', Times, serif; ",
    "font-size: 12pt; border-collapse: collapse; margin: 0 auto; ",
    "table-layout: fixed; mso-table-layout-alt: fixed; ",
    "width: ", total_w, "px; ",
    "border-top: 2px solid black; border-bottom: 2px solid black;"
  )
  s_body   <- "border: none; padding: 2px 5px; white-space: nowrap; vertical-align: top;"
  s_head   <- paste0("border: none; padding: 4px 5px 3px; ",
                     "border-bottom: 1px solid black; ",
                     "white-space: nowrap; font-weight: bold;")
  # First-row cells need the 2px top rule (Word ignores table-level borders
  # on paste; cell borders survive).
  s_title  <- paste0("border: none; padding: 6px 5px 4px; ",
                     "border-top: 2px solid black; ",
                     "text-align: center; font-style: italic; ",
                     "font-weight: bold;")
  s_fit    <- paste0("border: none; padding: 2px 5px; ",
                     "border-top: 1px solid black; white-space: nowrap; ",
                     "vertical-align: top;")
  # Used on the last-body-row cells so the bottom rule survives Word paste.
  s_last   <- paste0("border: none; padding: 2px 5px; ",
                     "border-bottom: 2px solid black; ",
                     "white-space: nowrap; vertical-align: top;")
  s_spacer <- "border: none; padding: 0; width: 16px;"
  s_break  <- paste0("border: none; padding: 0; height: 16px; ",
                     "background-color: transparent;")

  # --- Per-model formatting ---
  fmt_model <- function(mod) {
    betas <- std_betas(mod)
    coefs <- tidy(mod) %>%
      mutate(
        beta_val = betas[term],
        Predictor = label_terms(term),
        B = paste0(nb_num(sprintf("%.2f", estimate)),
                   format_p_stars(p.value)),
        beta = ifelse(is.na(beta_val), "—",
                      nb_num(sub("^0", "",
                                 sub("^-0", "-",
                                     sprintf("%.2f", beta_val))))),
        SE = nb_num(sprintf("%.2f", std.error)),
        p_fmt = esc_p(nb_num(format_p_apa(p.value))),
        CI = nb_ci(paste0("[",
                          sprintf("%.2f", estimate - 1.96 * std.error),
                          ", ",
                          sprintf("%.2f", estimate + 1.96 * std.error),
                          "]")),
        is_sig = p.value < .05
      ) %>%
      select(Predictor, B, beta, SE, p_fmt, CI, is_sig)
    s <- summary(mod)
    fit <- tibble(
      Predictor = c("<i>R</i>²", "Adjusted <i>R</i>²", "<i>N</i>"),
      B = c(nb_num(sub("^0", "", sprintf("%.3f", s$r.squared))),
            nb_num(sub("^0", "", sprintf("%.3f", s$adj.r.squared))),
            as.character(nobs(mod))),
      beta = "", SE = "", p_fmt = "", CI = "",
      is_sig = FALSE
    )
    list(coefs = coefs, fit = fit)
  }

  d <- lapply(models, fmt_model)

  # --- HTML helpers ---
  td <- function(content, style, align = "right") {
    paste0("<td style=\"", style, " text-align: ", align, ";\">",
           content, "</td>")
  }
  th <- function(content, align = "right") {
    paste0("<th style=\"", s_head, " text-align: ", align, ";\">",
           content, "</th>")
  }
  spacer_td <- paste0("<td style=\"", s_spacer, "\"></td>")
  spacer_th <- paste0("<th style=\"", s_spacer, "\"></th>")

  # Row of header labels (Predictor + 5) — repeated for both columns in a band
  one_hdr_block <- paste0(
    th("Predictor", "left"),
    th("<i>B</i>", "right"),
    th("β",        "right"),
    th("<i>SE</i>","right"),
    th("<i>p</i>", "right"),
    th("95% CI",   "center")
  )
  header_row <- paste0("<tr>", one_hdr_block, spacer_th, one_hdr_block, "</tr>")

  # Outcome-title row given two outcome names. `with_top_rule = TRUE` adds
  # a 1px black rule above the title cells — used to separate panels per
  # APA 7 paneled-table convention.
  title_row <- function(left, right, with_top_rule = FALSE) {
    extra <- if (with_top_rule) " border-top: 1px solid black;" else ""
    paste0(
      "<tr>",
      "<td colspan=\"6\" style=\"", s_title, extra, "\">",
      expand_name(left), "</td>",
      "<td style=\"", s_spacer, extra, "\"></td>",
      "<td colspan=\"6\" style=\"", s_title, extra, "\">",
      expand_name(right), "</td>",
      "</tr>"
    )
  }

  # A single data row given matched rows from left/right model frames
  data_row <- function(rL, rR, cell_style = s_body) {
    boldL <- if (isTRUE(rL$is_sig)) " font-weight: bold;" else ""
    boldR <- if (isTRUE(rR$is_sig)) " font-weight: bold;" else ""
    paste0(
      "<tr>",
      td(rL$Predictor, paste0(cell_style, boldL), "left"),
      td(rL$B,         paste0(cell_style, boldL), "right"),
      td(rL$beta,      paste0(cell_style, boldL), "right"),
      td(rL$SE,        paste0(cell_style, boldL), "right"),
      td(rL$p_fmt,     paste0(cell_style, boldL), "right"),
      td(rL$CI,        paste0(cell_style, boldL), "center"),
      spacer_td,
      td(rR$Predictor, paste0(cell_style, boldR), "left"),
      td(rR$B,         paste0(cell_style, boldR), "right"),
      td(rR$beta,      paste0(cell_style, boldR), "right"),
      td(rR$SE,        paste0(cell_style, boldR), "right"),
      td(rR$p_fmt,     paste0(cell_style, boldR), "right"),
      td(rR$CI,        paste0(cell_style, boldR), "center"),
      "</tr>"
    )
  }

  # Build one full band (two outcomes side-by-side: title + header +
  # predictor rows + fit rows with mid-rule above)
  band_html <- function(L, R, names_pair, with_top_rule = FALSE) {
    title_html  <- title_row(names_pair[1], names_pair[2],
                             with_top_rule = with_top_rule)
    body_rows   <- vapply(
      seq_len(max(nrow(L$coefs), nrow(R$coefs))),
      function(i) data_row(L$coefs[i, ], R$coefs[i, ]),
      character(1))
    fit_rows <- vapply(seq_len(nrow(L$fit)), function(i) {
      style <- if (i == 1L) s_fit else s_body  # mid-rule on first fit row
      data_row(L$fit[i, ], R$fit[i, ], cell_style = style)
    }, character(1))
    paste0(title_html, header_row,
           paste(body_rows, collapse = ""),
           paste(fit_rows,  collapse = ""))
  }

  top_band <- band_html(d[[1]], d[[2]], model_names[c(1, 2)])
  bot_band <- band_html(d[[3]], d[[4]], model_names[c(3, 4)],
                        with_top_rule = TRUE)

  # Section break between bands: APA panel tables put a thin horizontal
  # rule between panels. The empty row gives breathing room above; the
  # next row (band 2's title) carries the top rule.
  section_break <- paste0(
    "<tr><td colspan=\"", total_cols, "\" style=\"", s_break, "\"></td></tr>"
  )

  # <colgroup> for fixed widths
  col_tags <- paste0(
    "<col style=\"width:", col_w["Pred"], "px;\">",
    "<col style=\"width:", col_w["B"],    "px;\">",
    "<col style=\"width:", col_w["beta"], "px;\">",
    "<col style=\"width:", col_w["SE"],   "px;\">",
    "<col style=\"width:", col_w["p"],    "px;\">",
    "<col style=\"width:", col_w["CI"],   "px;\">",
    "<col style=\"width:", spacer_w,      "px;\">",
    "<col style=\"width:", col_w["Pred"], "px;\">",
    "<col style=\"width:", col_w["B"],    "px;\">",
    "<col style=\"width:", col_w["beta"], "px;\">",
    "<col style=\"width:", col_w["SE"],   "px;\">",
    "<col style=\"width:", col_w["p"],    "px;\">",
    "<col style=\"width:", col_w["CI"],   "px;\">"
  )
  colgroup_html <- paste0("<colgroup>", col_tags, "</colgroup>")

  # Footer Note: APA 7 places general notes first, then a separate
  # probability note line at the end. Both share one <tfoot> cell but
  # the probability line is separated by <br> per APA convention.
  general_note <- paste0(
    if (!is.null(note)) paste0(note, " ") else "",
    "<i>B</i> = unstandardized coefficient; β = standardized coefficient; ",
    "<i>SE</i> = standard error; 95% CI = 95% confidence interval. ",
    "Bold rows are significant (<i>p</i> &lt; .05)."
  )
  prob_note <- paste0("*<i>p</i> &lt; .05. ",
                      "**<i>p</i> &lt; .01. ",
                      "***<i>p</i> &lt; .001.")
  tfoot_html <- paste0(
    "<tfoot><tr><td colspan=\"", total_cols, "\" ",
    "style=\"border-top: 1px solid black; padding: 8px 5px 0 5px; ",
    "font-family: 'Times New Roman', Times, serif; font-size: 12pt; ",
    "text-align: left; white-space: normal;\">",
    "<em>Note.</em> ", general_note, "<br>",
    prob_note,
    "</td></tr></tfoot>"
  )

  # --- Assemble the single <table> ---
  table_html <- paste0(
    "<table style=\"", apa_css, "\">",
    colgroup_html,
    "<thead></thead>",
    "<tbody>",
    top_band,
    section_break,
    bot_band,
    "</tbody>",
    tfoot_html,
    "</table>"
  )

  out <- table_html
  attr(out, "format") <- "html"
  class(out) <- c("kableExtra", "knitr_kable")
  out
}

# =============================================================================
# 2. MULTIVERSE / SPECIFICATION CURVE TABLE
# =============================================================================

# Helper to extract EF results from a model
extract_ef_results <- function(model, outcome, specification, wave) {
  mod_summary <- summary(model)
  b <- coef(model)[-1]
  sx <- apply(model.matrix(model)[, -1, drop = FALSE], 2, sd)
  sy <- sd(model$model[[1]])
  std_beta <- b * sx / sy
  tidy(model) %>%
    filter(str_detect(term, "TEF|EF")) %>%
    mutate(
      outcome = outcome,
      specification = specification,
      wave = wave,
      r_squared = mod_summary$r.squared,
      adj_r_squared = mod_summary$adj.r.squared,
      n = nobs(model),
      beta = std_beta[term],
      term = label_terms(term)
    )
}

# Multiverse summary table: EF effects across all specifications
apa_table_multiverse <- function(results_df, title = "Multiverse Analysis Summary",
                                 note = NULL) {

  nb_num <- function(s) gsub("-", "‑", s, fixed = TRUE)
  nb_ci  <- function(s) gsub(", ", ", ", nb_num(s), fixed = TRUE)
  esc_p  <- function(s) sub("<", "&lt;", s, fixed = TRUE)

  # Outcome name -> acronym map (defined in the Note).
  outcome_acronym <- c(
    "NEGWF" = "NWF", "NEGFW" = "NFW", "POSWF" = "PWF", "POSFW" = "PFW",
    # Tolerate already-acronym'd inputs:
    "NWF" = "NWF", "NFW" = "NFW", "PWF" = "PWF", "PFW" = "PFW"
  )
  abbr_outcome <- function(o) {
    ifelse(o %in% names(outcome_acronym), unname(outcome_acronym[o]), o)
  }
  # Compact Executive Functioning to "EF" in term labels.
  abbr_ef <- function(s) gsub("Executive Functioning", "EF", s, fixed = TRUE)

  # Pre-compute raw numeric pieces (for OSC-style comparison) and the
  # display strings. The reference for each (outcome, term) is the FIRST
  # row that appears in results_df with that pairing — typically the bare
  # "No covariates" specification, which becomes the within-wave baseline
  # the rest of the multiverse is judged against.
  raw <- results_df %>%
    mutate(
      term_label = abbr_ef(label_terms(term)),
      outcome    = abbr_outcome(outcome),
      ci_lo      = estimate - 1.96 * std.error,
      ci_hi      = estimate + 1.96 * std.error,
      sig        = p.value < .05
    )

  # Reference = first row per (outcome, term)
  ref <- raw %>%
    group_by(outcome, term_label) %>%
    summarise(
      ref_est   = first(estimate),
      ref_ci_lo = first(ci_lo),
      ref_ci_hi = first(ci_hi),
      ref_sig   = first(sig),
      ref_row   = row_number()[1],  # 1 by construction
      .groups = "drop"
    )

  tbl <- raw %>%
    left_join(ref, by = c("outcome", "term_label")) %>%
    group_by(outcome, term_label) %>%
    mutate(is_reference = row_number() == 1L) %>%
    ungroup() %>%
    mutate(
      same_sign  = sign(estimate) == sign(ref_est),
      ci_overlap = !(ci_hi < ref_ci_lo | ci_lo > ref_ci_hi),
      Sign = case_when(
        is_reference          ~ "—",
        !ref_sig              ~ "—",
        same_sign             ~ "✓",
        TRUE                  ~ "✗"
      ),
      Sig = case_when(
        is_reference                 ~ "—",
        !ref_sig                     ~ "—",
        sig & same_sign              ~ "✓",
        TRUE                         ~ "✗"
      ),
      CI = case_when(
        is_reference                 ~ "—",
        !(ref_sig & sig)             ~ "—",
        ci_overlap                   ~ "✓",
        TRUE                         ~ "✗"
      ),
      b_star = paste0(nb_num(sprintf("%.3f", estimate)),
                      format_p_stars(p.value)),
      beta_fmt = ifelse(is.na(beta), "—",
                        nb_num(sprintf("%.3f", beta))),
      se   = nb_num(sprintf("%.3f", std.error)),
      p_fmt = esc_p(nb_num(format_p_apa(p.value))),
      ci_fmt = nb_ci(paste0("[", sprintf("%.3f", ci_lo), ", ",
                            sprintf("%.3f", ci_hi), "]")),
      is_main_ef = !str_detect(term, ":")
    ) %>%
    select(specification, outcome, term_label, b_star, beta_fmt, se,
           p_fmt, ci_fmt, Sign, Sig, CI, sig, is_main_ef)

  total_cols <- 11L
  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "EF = executive functioning. ",
    "NWF = negative work-to-family spillover; NFW = negative family-to-work spillover; ",
    "PWF = positive work-to-family spillover; PFW = positive family-to-work spillover. ",
    "β = standardized coefficient. ",
    "Bold rows are significant (<i>p</i> &lt; .05).",
    "\nSign, Sig, and CI columns apply the Open Science Collaboration (2015) criteria within wave, ",
    "comparing each specification's effect against the first (reference) specification for that ",
    "outcome and term. Sign = same sign as reference. Sig = same sign and significant. ",
    "CI = 95% CIs overlap (when both reference and row are significant). ",
    "✓ = criterion met. ✗ = criterion not met. — = not applicable (reference row, or reference effect non-significant).",
    "\n*<i>p</i> &lt; .05. **<i>p</i> &lt; .01. ***<i>p</i> &lt; .001."
  )

  # --- OSC criterion counts (across non-reference rows only) ---
  is_ref         <- tbl$Sign == "—" & tbl$Sig == "—" & tbl$CI == "—"
  # Sign / Sig denominator: non-reference rows whose reference effect was
  # significant (i.e., the criterion is applicable).
  eligible_sign  <- !is_ref & tbl$Sign != "—"
  eligible_sig   <- !is_ref & tbl$Sig  != "—"
  eligible_ci    <- !is_ref & tbl$CI   != "—"
  n_sign_denom   <- sum(eligible_sign)
  n_sig_denom    <- sum(eligible_sig)
  n_ci_denom     <- sum(eligible_ci)
  n_same_sign    <- sum(tbl$Sign == "✓")
  n_same_sig     <- sum(tbl$Sig  == "✓")
  n_ci_overlap   <- sum(tbl$CI   == "✓")

  pct <- function(num, den) {
    if (den > 0) sprintf("%.1f", 100 * num / den) else "N/A"
  }

  # Three summary rows appended below the body; they will be styled with a
  # thin top rule and italic-bold to mirror the replicability summary block.
  summary_rows <- tibble(
    specification = c(
      paste0("Step 1: Same sign (of ",   n_sign_denom, " applicable)"),
      paste0("Step 2: Same sign + significant (of ",
             n_sig_denom, " applicable)"),
      paste0("Step 3: Overlapping CIs (of ", n_ci_denom,
             " significant in both reference and row)")
    ),
    outcome = "", term_label = "", b_star = "", beta_fmt = "",
    se = "", p_fmt = "", ci_fmt = "",
    Sign = c(paste0(n_same_sign,  "/", n_sign_denom,
                    " (", pct(n_same_sign,  n_sign_denom), "%)"), "", ""),
    Sig  = c("", paste0(n_same_sig,   "/", n_sig_denom,
                        " (", pct(n_same_sig,   n_sig_denom),  "%)"), ""),
    CI   = c("", "", paste0(n_ci_overlap, "/", n_ci_denom,
                            " (", pct(n_ci_overlap, n_ci_denom), "%)")),
    sig  = FALSE, is_main_ef = FALSE
  )

  sig_rows  <- which(tbl$sig)
  n_total   <- nrow(tbl)
  tbl       <- bind_rows(tbl, summary_rows)
  n_with_summary <- nrow(tbl)
  summary_start  <- n_total + 1L

  apa_css <- paste0(
    "font-family: 'Times New Roman', Times, serif; ",
    "font-size: 11pt; border-collapse: collapse; margin: 0 auto; ",
    "table-layout: fixed; mso-table-layout-alt: fixed; ",
    "width: 945px; ",
    "border-top: 2px solid black; border-bottom: 2px solid black;"
  )
  # Allow Specification + Term to wrap so multi-word labels don't overflow
  # the narrowed columns. Numeric columns stay nowrap.
  cell_css_body <- "border: none; padding: 2px 8px; vertical-align: top;"
  cell_css_head <- paste0("border: none; padding: 3px 8px; ",
                          "border-top: 2px solid black; ",
                          "border-bottom: 1px solid black; ",
                          "vertical-align: bottom;")
  cell_css_num <- "border: none; padding: 2px 8px; white-space: nowrap; vertical-align: top;"

  k <- kable(tbl %>% select(-sig, -is_main_ef),
             col.names = c("Specification", "Outcome", "Term",
                           "<i>B</i>", "β", "<i>SE</i>", "<i>p</i>",
                           "95% CI", "Sign", "Sig", "CI"),
             caption = NULL,
             align = c("l", "l", "l", "r", "r", "r", "r", "c", "c", "c", "c"),
             escape = FALSE,
             table.attr = paste0("style=\"", apa_css, "\""))
  cell_css_summary <- paste0("border: none; padding: 4px 8px; ",
                             "border-top: 1px solid black; ",
                             "font-style: italic; font-weight: bold; ",
                             "vertical-align: top;")
  cell_css_summary_rest <- sub("border-top: 1px solid black; ", "",
                               cell_css_summary, fixed = TRUE)

  k <- k %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = cell_css_head) %>%
    row_spec(seq_len(n_total), extra_css = cell_css_body) %>%
    # No-wrap on the numeric & checkmark columns so coefficients,
    # SEs, p-values, CIs, and ✓/✗ glyphs stay on one line.
    column_spec(4:11, extra_css = cell_css_num) %>%
    # Summary block: thin top rule above first row, italic-bold throughout.
    row_spec(summary_start, extra_css = cell_css_summary)
  if (summary_start < n_with_summary) {
    k <- k %>% row_spec((summary_start + 1):n_with_summary,
                        extra_css = cell_css_summary_rest)
  }

  if (length(sig_rows)) k <- k %>% row_spec(sig_rows, bold = TRUE)

  # Collapse repeated cells in Specification + Outcome columns vertically.
  # (The summary block has unique Specification strings per row, so it is
  # not affected; empty Outcome cells in the summary collapse harmlessly.)
  k <- k %>% collapse_rows(columns = 1:2, valign = "top")

  # Tfoot Note spanning all 8 columns (Word respects colspan).
  note_html <- gsub("\n", "<br>", full_note, fixed = TRUE)
  tfoot_html <- paste0(
    "<tfoot>",
    "<tr><td colspan=\"", total_cols, "\" ",
    "style=\"border-top: 1px solid black; padding: 8px 8px 0 8px; ",
    "font-family: 'Times New Roman', Times, serif; font-size: 11pt; ",
    "text-align: left; white-space: normal;\">",
    "<em>Note.</em> ", note_html,
    "</td></tr>",
    "</tfoot>"
  )
  k[1] <- sub("</table>", paste0(tfoot_html, "</table>"), k[1], fixed = TRUE)

  # Inject a <colgroup> that narrows the Specification column and gives
  # room to Outcome / Term / numeric / OSC-criterion columns. Widths in px:
  # Spec 170, Outcome 55, Term 160, B 75, β 55, SE 60, p 65, CI 115,
  # Sign 40, Sig 40, CI(overlap) 40 -> total ≈ 945 px ≈ 9.8 in.
  col_tags <- paste0(
    "<col style=\"width:170px;\">",   # Specification
    "<col style=\"width:55px;\">",    # Outcome (acronym)
    "<col style=\"width:160px;\">",   # Term
    "<col style=\"width:75px;\">",    # B
    "<col style=\"width:55px;\">",    # β
    "<col style=\"width:60px;\">",    # SE
    "<col style=\"width:65px;\">",    # p
    "<col style=\"width:115px;\">",   # 95% CI
    "<col style=\"width:40px;\">",    # Sign
    "<col style=\"width:40px;\">",    # Sig
    "<col style=\"width:40px;\">"     # CI overlap
  )
  k[1] <- sub("(<table[^>]*>)",
              paste0("\\1<colgroup>", col_tags, "</colgroup>"),
              k[1])
  k
}

# Compact multiverse: one row per specification, columns per outcome
apa_table_multiverse_wide <- function(results_df,
                                      title = "Multiverse Summary: EF Main Effects",
                                      note = NULL) {

  nb_num <- function(s) gsub("-", "‑", s, fixed = TRUE)
  nb_ci  <- function(s) gsub(", ", ", ", nb_num(s), fixed = TRUE)
  esc_p  <- function(s) sub("<", "&lt;", s, fixed = TRUE)

  # Filter to main EF effect only (not interactions)
  main_ef <- results_df %>%
    filter(!str_detect(term, ":")) %>%
    mutate(
      b_star = paste0(nb_num(sprintf("%.3f", estimate)),
                      format_p_stars(p.value)),
      beta_fmt = ifelse(is.na(beta), "—",
                        nb_num(sprintf("%.3f", beta))),
      se_fmt = paste0("(", nb_num(sprintf("%.3f", std.error)), ")"),
      p_fmt  = esc_p(nb_num(format_p_apa(p.value))),
      ci = nb_ci(paste0("[", sprintf("%.3f", estimate - 1.96 * std.error),
                        ", ",
                        sprintf("%.3f", estimate + 1.96 * std.error), "]")),
      sig = p.value < .05
    )

  outcomes <- unique(main_ef$outcome)
  n_cols_per <- 5L
  total_cols <- 1L + n_cols_per * length(outcomes)

  wide <- main_ef %>%
    select(specification, outcome, b_star, beta_fmt, se_fmt, p_fmt, ci) %>%
    pivot_wider(id_cols = specification, names_from = outcome,
                values_from = c(b_star, beta_fmt, se_fmt, p_fmt, ci),
                names_glue = "{outcome}_{.value}")

  col_order <- c("specification", as.vector(rbind(
    paste0(outcomes, "_b_star"), paste0(outcomes, "_beta_fmt"),
    paste0(outcomes, "_se_fmt"), paste0(outcomes, "_p_fmt"),
    paste0(outcomes, "_ci"))))
  wide <- wide %>% select(any_of(col_order))

  sig_wide <- main_ef %>%
    select(specification, outcome, sig) %>%
    pivot_wider(id_cols = specification, names_from = outcome,
                values_from = sig)
  sig_any  <- apply(sig_wide[, -1, drop = FALSE], 1, any, na.rm = TRUE)
  sig_rows <- which(sig_any)
  n_total  <- nrow(wide)

  col_headers <- c(" ", rep(c("<i>B</i>", "β", "<i>SE</i>",
                              "<i>p</i>", "95% CI"),
                            length(outcomes)))
  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "β = standardized coefficient; <i>SE</i> in parentheses. ",
    "Bold rows are significant (<i>p</i> &lt; .05) for at least one outcome.",
    "\n*<i>p</i> &lt; .05. **<i>p</i> &lt; .01. ***<i>p</i> &lt; .001."
  )

  apa_css <- paste0(
    "font-family: 'Times New Roman', Times, serif; ",
    "font-size: 11pt; border-collapse: collapse; margin: 0 auto; ",
    "border-top: 2px solid black; border-bottom: 2px solid black;"
  )
  cell_css_body <- "border: none; padding: 2px 6px; white-space: nowrap;"
  cell_css_head <- paste0("border: none; padding: 3px 6px; ",
                          "border-top: 2px solid black; ",
                          "border-bottom: 1px solid black; ",
                          "white-space: nowrap;")

  k <- kable(wide, col.names = col_headers, caption = NULL,
             align = c("l", rep(c("r", "r", "r", "r", "c"),
                                length(outcomes))),
             escape = FALSE,
             table.attr = paste0("style=\"", apa_css, "\""))
  k <- k %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    add_header_above(c(" " = 1,
                       setNames(rep(n_cols_per, length(outcomes)), outcomes)),
                     bold = TRUE, line = TRUE, line_sep = 3) %>%
    row_spec(0, bold = TRUE, extra_css = cell_css_head) %>%
    row_spec(seq_len(n_total), extra_css = cell_css_body)

  if (length(sig_rows)) k <- k %>% row_spec(sig_rows, bold = TRUE)

  note_html <- gsub("\n", "<br>", full_note, fixed = TRUE)
  tfoot_html <- paste0(
    "<tfoot>",
    "<tr><td colspan=\"", total_cols, "\" ",
    "style=\"border-top: 1px solid black; padding: 8px 6px 0 6px; ",
    "font-family: 'Times New Roman', Times, serif; font-size: 11pt; ",
    "text-align: left; white-space: normal;\">",
    "<em>Note.</em> ", note_html,
    "</td></tr>",
    "</tfoot>"
  )
  k[1] <- sub("</table>", paste0(tfoot_html, "</table>"), k[1], fixed = TRUE)
  k
}

# =============================================================================
# 2B. CROSS-WAVE REPLICABILITY TABLE (OSC, 2015)
# =============================================================================

# --- Replicability summary table ---
# Expects a paired dataframe with columns:
#   pair_id, term_label, est_w1, se_w1, p_w1, est_w2, se_w2, p_w2,
#   sig_w1, sig_w2, same_sign, replicated_sign, replicated_sig, ci_overlap,
#   ci_lo_w1, ci_hi_w1, ci_lo_w2, ci_hi_w2
apa_table_replicability <- function(paired_df,
                                     title = "Cross-Wave Replicability (OSC, 2015)",
                                     note = NULL,
                                     wave_labels = c("W1", "W2"),
                                     col_widths = NULL,
                                     split_model = FALSE) {
  # `col_widths`, if supplied, is a numeric vector of pixel widths matching
  # the number of leading columns (7 when split_model = FALSE, 8 when TRUE).
  # When given, the function injects a <colgroup> + sets table-layout: fixed
  # so columns size to the declared widths rather than auto-expanding.
  #
  # `split_model = TRUE` splits the single "Model" column into two columns:
  # "Specification" (the model description) and "Outcome" (a short acronym).
  # Requires paired_df to have separate `specification` and `outcome`
  # columns. The outcome is rendered through `outcome_acronym_map`:
  #   NEGWF -> NWF, NEGFW -> NFW, POSWF -> PWF, POSFW -> PFW.

  # --- Summary statistics ---
  n_sig_w1   <- sum(paired_df$sig_w1)
  n_same_sign <- sum(paired_df$replicated_sign, na.rm = TRUE)
  n_same_sig  <- sum(paired_df$replicated_sig, na.rm = TRUE)
  n_both_sig  <- sum(paired_df$sig_w1 & paired_df$sig_w2)
  n_ci_over   <- sum(paired_df$ci_overlap, na.rm = TRUE)

  pct_sign <- if (n_sig_w1 > 0) sprintf("%.1f", 100 * n_same_sign / n_sig_w1) else "N/A"
  pct_sig  <- if (n_sig_w1 > 0) sprintf("%.1f", 100 * n_same_sig / n_sig_w1)  else "N/A"
  pct_ci   <- if (n_both_sig > 0) sprintf("%.1f", 100 * n_ci_over / n_both_sig) else "N/A"

  # Outcome name -> acronym for the split-model variant.
  outcome_acronym_map <- c(
    "NEGWF" = "NWF", "NEGFW" = "NFW",
    "POSWF" = "PWF", "POSFW" = "PFW",
    "NWF" = "NWF",   "NFW" = "NFW",
    "PWF" = "PWF",   "PFW" = "PFW"
  )
  abbr_outcome <- function(o) {
    ifelse(o %in% names(outcome_acronym_map),
           unname(outcome_acronym_map[o]), o)
  }

  # --- Detail rows ---
  detail <- paired_df %>%
    mutate(
      w1_coef = paste0(sprintf("%.3f", est_w1), " [",
                        sprintf("%.3f", ci_lo_w1), ", ",
                        sprintf("%.3f", ci_hi_w1), "]",
                        format_p_stars(p_w1)),
      w2_coef = paste0(sprintf("%.3f", est_w2), " [",
                        sprintf("%.3f", ci_lo_w2), ", ",
                        sprintf("%.3f", ci_hi_w2), "]",
                        format_p_stars(p_w2)),
      sign_col = case_when(!sig_w1 ~ "\u2014",
                           same_sign ~ "\u2713",
                           TRUE ~ "\u2717"),
      sig_col  = case_when(!sig_w1 ~ "\u2014",
                           replicated_sig ~ "\u2713",
                           TRUE ~ "\u2717"),
      ci_col   = case_when(!(sig_w1 & sig_w2) ~ "\u2014",
                           ci_overlap ~ "\u2713",
                           TRUE ~ "\u2717")
    )

  if (split_model) {
    stopifnot("paired_df must contain a `specification` column when split_model = TRUE" =
                "specification" %in% names(paired_df),
              "paired_df must contain an `outcome` column when split_model = TRUE" =
                "outcome" %in% names(paired_df))
    detail <- detail %>%
      mutate(outcome_lab = abbr_outcome(outcome)) %>%
      select(specification, outcome_lab, term_label,
             w1_coef, w2_coef, sign_col, sig_col, ci_col)
  } else {
    detail <- detail %>%
      select(pair_id, term_label, w1_coef, w2_coef,
             sign_col, sig_col, ci_col)
  }

  # --- Summary rows ---
  step1 <- paste0("Step 1: Same sign (of ", n_sig_w1, " sig in ",
                  wave_labels[1], ")")
  step2 <- paste0("Step 2: Same sign + significant (of ", n_sig_w1,
                  " sig in ", wave_labels[1], ")")
  step3 <- paste0("Step 3: Overlapping CIs (of ", n_both_sig,
                  " sig in both)")

  if (split_model) {
    summary_rows <- tibble(
      specification = c(step1, step2, step3),
      outcome_lab   = "",
      term_label    = "",
      w1_coef = "", w2_coef = "",
      sign_col = c(paste0(n_same_sign, "/", n_sig_w1, " (", pct_sign, "%)"), "", ""),
      sig_col  = c("", paste0(n_same_sig, "/", n_sig_w1, " (", pct_sig, "%)"), ""),
      ci_col   = c("", "", paste0(n_ci_over, "/", n_both_sig, " (", pct_ci, "%)"))
    )
  } else {
    summary_rows <- tibble(
      pair_id    = c(step1, step2, step3),
      term_label = "",
      w1_coef    = "", w2_coef = "",
      sign_col = c(paste0(n_same_sign, "/", n_sig_w1, " (", pct_sign, "%)"), "", ""),
      sig_col  = c("", paste0(n_same_sig, "/", n_sig_w1, " (", pct_sig, "%)"), ""),
      ci_col   = c("", "", paste0(n_ci_over, "/", n_both_sig, " (", pct_ci, "%)"))
    )
  }

  final_tbl <- bind_rows(detail, summary_rows)

  # --- Footnote ---
  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "Replicability assessed using the Open Science Collaboration (2015) criteria. ",
    "Step 1: percentage of ", wave_labels[1],
    "-significant effects with the same sign in ", wave_labels[2], ". ",
    "Step 2: percentage of ", wave_labels[1],
    "-significant effects that are also significant and same sign in ",
    wave_labels[2], ". ",
    "Step 3: percentage of effects significant in both waves with overlapping 95% CIs.\n",
    "\u2713 = criterion met. \u2717 = criterion not met. \u2014 = not applicable.\n",
    "*<i>p</i> &lt; .05. **<i>p</i> &lt; .01. ***<i>p</i> &lt; .001.")

  # (Previously bolded the W1-significant rows; removed because the bolding
  # was not explained in the Note and confused readers.)
  summary_start <- nrow(detail) + 1
  n_total <- nrow(final_tbl)
  total_cols <- if (split_model) 8L else 7L
  n_wrap_cols <- if (split_model) 3L else 2L  # cols allowed to wrap

  # Non-breaking minus + non-breaking space inside CI brackets and between
  # the coefficient and the "[..." so the W1/W2 columns stay on one line
  # in Word at 12pt.
  nb_num <- function(s) gsub("-", "‑", s, fixed = TRUE)
  nb_ci  <- function(s) {
    s <- nb_num(s)
    s <- gsub(", ", ", ", s, fixed = TRUE)   # nbsp after comma
    s <- gsub("] [", "] [", s, fixed = TRUE) # (no-op if absent)
    s <- gsub(" [", " [", s, fixed = TRUE)   # nbsp between coef and [
    s
  }
  final_tbl <- final_tbl %>%
    mutate(w1_coef = nb_ci(w1_coef),
           w2_coef = nb_ci(w2_coef))

  # APA 7: top/header-bottom/bottom rules + interior rule before summary.
  # Font-size set in *points* so Word renders at the requested size on
  # paste (CSS pixels would be scaled down to ~9pt in Word).
  apa_css <- paste0(
    "font-family: 'Times New Roman', Times, serif; ",
    "font-size: 12pt; border-collapse: collapse; margin: 0 auto; ",
    if (!is.null(col_widths))
      paste0("table-layout: fixed; mso-table-layout-alt: fixed; ",
             "width: ", sum(col_widths), "px; ") else "",
    "border-top: 2px solid black; border-bottom: 2px solid black;"
  )
  # Model + Term columns wrap on whitespace when col_widths is supplied
  # (long Model labels would otherwise blow past their fixed column).
  cell_css_body <- "border: none; padding: 3px 8px; white-space: nowrap; vertical-align: top;"
  cell_css_wrap <- "border: none; padding: 3px 8px; vertical-align: top;"
  cell_css_head <- paste0("border: none; padding: 4px 8px; ",
                          "border-top: 2px solid black; ",
                          "border-bottom: 1px solid black;")
  cell_css_mid  <- paste0("border: none; padding: 4px 8px; ",
                          "border-top: 1px solid black; ",
                          "font-style: italic; font-weight: bold; ",
                          "white-space: nowrap;")

  col_names_vec <- if (split_model)
    c("Specification", "Outcome", "Term",
      paste0(wave_labels[1], " <i>B</i> [95% CI]"),
      paste0(wave_labels[2], " <i>B</i> [95% CI]"),
      "Sign", "Sig", "CI")
  else
    c("Model", "Term",
      paste0(wave_labels[1], " <i>B</i> [95% CI]"),
      paste0(wave_labels[2], " <i>B</i> [95% CI]"),
      "Sign", "Sig", "CI")

  align_vec <- if (split_model)
    c("l", "l", "l", "c", "c", "c", "c", "c")
  else
    c("l", "l", "c", "c", "c", "c", "c")

  k <- kable(final_tbl,
             col.names = col_names_vec,
             caption = NULL,
             align = align_vec,
             escape = FALSE,
             table.attr = paste0("style=\"", apa_css, "\""))

  k <- k %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 12, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = cell_css_head) %>%
    row_spec(seq_len(n_total), extra_css = cell_css_body) %>%
    # Allow Model (or Specification+Outcome) + Term columns to wrap when
    # column widths are fixed.
    column_spec(seq_len(n_wrap_cols), extra_css = cell_css_wrap)


  # Interior rule + italic-bold styling for the summary block (replaces
  # pack_rows, which would otherwise inject a section header row that
  # confuses Word paste).
  k <- k %>%
    row_spec(summary_start, extra_css = cell_css_mid)
  if (summary_start < n_total) {
    k <- k %>%
      row_spec((summary_start + 1):n_total,
               extra_css = sub("border-top: 1px solid black; ", "",
                               cell_css_mid, fixed = TRUE))
  }

  # Inject full-width tfoot Note (colspan), preserving kable attributes.
  note_html <- gsub("\n", "<br>", full_note, fixed = TRUE)
  tfoot_html <- paste0(
    "<tfoot>",
    "<tr><td colspan=\"", total_cols, "\" ",
    "style=\"border-top: 1px solid black; padding: 6px 8px 0 8px; ",
    "font-family: 'Times New Roman', Times, serif; font-size: 12pt; ",
    "text-align: left; white-space: normal;\">",
    "<em>Note.</em> ", note_html,
    "</td></tr>",
    "</tfoot>"
  )
  k[1] <- sub("</table>", paste0(tfoot_html, "</table>"), k[1], fixed = TRUE)

  # Inject <colgroup> with explicit widths when requested. Word + browser
  # then size columns to these widths instead of auto-expanding to the
  # available space, which is what produces the whitespace.
  if (!is.null(col_widths)) {
    stopifnot("length(col_widths) must equal total_cols" =
                length(col_widths) == total_cols)
    col_tags <- paste(vapply(col_widths,
                              function(w) paste0("<col style=\"width:", w, "px;\">"),
                              character(1)),
                       collapse = "")
    k[1] <- sub("(<table[^>]*>)",
                paste0("\\1<colgroup>", col_tags, "</colgroup>"),
                k[1])
  }
  k
}

# =============================================================================
# 3. CFA AND MEASUREMENT INVARIANCE TABLES
# =============================================================================

# --- CFA Fit Indices Table ---
apa_table_cfa_fit <- function(fits, fit_names, title = "CFA Model Fit Indices",
                              note = NULL) {

  indices <- c("chisq", "df", "pvalue", "cfi", "tli",
               "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")

  tbl <- map2_dfr(fits, fit_names, function(fit, name) {
    fm <- fitMeasures(fit, indices)
    tibble(
      Model = name,
      `chi-sq` = sprintf("%.2f", fm["chisq"]),
      df = as.integer(fm["df"]),
      p = esc_lt(format_p_apa(fm["pvalue"])),
      CFI = sprintf("%.3f", fm["cfi"]),
      TLI = sprintf("%.3f", fm["tli"]),
      RMSEA = sprintf("%.3f", fm["rmsea"]),
      `RMSEA 90% CI` = nb_comma(nb_minus(paste0(
        "[", sprintf("%.3f", fm["rmsea.ci.lower"]),
        ", ", sprintf("%.3f", fm["rmsea.ci.upper"]), "]"))),
      SRMR = sprintf("%.3f", fm["srmr"])
    )
  })

  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "Good fit: CFI &gt; .95, TLI &gt; .95, RMSEA &lt; .06, SRMR &lt; .08."
  )
  total_cols <- ncol(tbl)
  css <- apa7_css(11)

  k <- kable(tbl, caption = NULL,
             col.names = c("Model", "χ²", "<i>df</i>", "<i>p</i>",
                           "CFI", "TLI", "RMSEA", "RMSEA 90% CI", "SRMR"),
             align = c("l", rep("r", 5), "r", "c", "r"),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(tbl)), extra_css = css$body)

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# --- Standardized Factor Loadings Table ---
apa_table_cfa_loadings <- function(fit, title = "Standardized Factor Loadings",
                                   note = NULL) {

  std_sol <- standardizedSolution(fit)
  loadings <- std_sol %>%
    filter(op == "=~") %>%
    mutate(
      Factor = case_when(
        lhs == "neg_wtf" ~ "Neg W-to-F",
        lhs == "pos_wtf" ~ "Pos W-to-F",
        lhs == "neg_ftw" ~ "Neg F-to-W",
        lhs == "pos_ftw" ~ "Pos F-to-W",
        TRUE ~ lhs
      ),
      Item = rhs,
      Loading = nb_minus(sprintf("%.3f", est.std)),
      SE = sprintf("%.3f", se),
      sig = pvalue < .05
    ) %>%
    select(Factor, Item, Loading, SE, sig)

  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "All loadings standardized. All p &lt; .001 unless noted."
  )
  total_cols <- 4L
  css <- apa7_css(11)

  k <- kable(loadings %>% select(-sig),
             col.names = c("Factor", "Item", "λ", "SE"),
             caption = NULL, align = c("l", "l", "r", "r"),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(loadings)), extra_css = css$body) %>%
    collapse_rows(columns = 1, valign = "top")

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# --- Factor Correlations Table ---
apa_table_cfa_cors <- function(fit, title = "Factor Correlations", note = NULL) {

  factors <- c("neg_wtf", "pos_wtf", "neg_ftw", "pos_ftw")
  factor_labels <- c("neg_wtf" = "Neg W-to-F", "pos_wtf" = "Pos W-to-F",
                     "neg_ftw" = "Neg F-to-W", "pos_ftw" = "Pos F-to-W")

  std_sol <- standardizedSolution(fit)
  cors <- std_sol %>%
    filter(op == "~~", lhs != rhs, lhs %in% factors, rhs %in% factors) %>%
    mutate(
      lhs = factor_labels[lhs],
      rhs = factor_labels[rhs],
      r = paste0(nb_minus(sprintf("%.3f", est.std)), format_p_stars(pvalue))
    ) %>%
    select(lhs, rhs, r)

  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "*<i>p</i> &lt; .05. **<i>p</i> &lt; .01. ***<i>p</i> &lt; .001."
  )
  total_cols <- 3L
  css <- apa7_css(11)

  k <- kable(cors, col.names = c("Factor 1", "Factor 2", "<i>r</i>"),
             caption = NULL, align = c("l", "l", "r"),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(cors)), extra_css = css$body)

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# --- Measurement Invariance Summary Table ---
apa_table_invariance <- function(fits, title = "Measurement Invariance Across Gender",
                                 note = NULL) {
  # fits = named list: list(Configural=, Metric=, Scalar=, Strict=)

  level_names <- names(fits)

  tbl <- map_dfr(seq_along(fits), function(i) {
    fm <- fitMeasures(fits[[i]], c("chisq", "df", "cfi", "rmsea", "srmr"))
    tibble(
      Level = level_names[i],
      `chi-sq` = sprintf("%.2f", fm["chisq"]),
      df = as.integer(fm["df"]),
      CFI = sprintf("%.3f", fm["cfi"]),
      RMSEA = sprintf("%.3f", fm["rmsea"]),
      SRMR = sprintf("%.3f", fm["srmr"])
    )
  })

  cfi_vals  <- sapply(fits, function(x) fitMeasures(x, "cfi"))
  delta_cfi <- c(NA, diff(cfi_vals))
  tbl$dCFI <- ifelse(is.na(delta_cfi), "—",
                     nb_minus(sprintf("%.4f", delta_cfi)))

  rmsea_vals  <- sapply(fits, function(x) fitMeasures(x, "rmsea"))
  delta_rmsea <- c(NA, diff(rmsea_vals))
  tbl$dRMSEA <- ifelse(is.na(delta_rmsea), "—",
                       nb_minus(sprintf("%.4f", delta_rmsea)))

  tbl$Decision <- ifelse(is.na(delta_cfi), "—",
                         ifelse(abs(delta_cfi) < .01, "Supported",
                                "Not supported"))

  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "ΔCFI &lt; .01 supports invariance (Chen, 2007). ",
    "Configural = same structure; Metric = equal loadings; ",
    "Scalar = equal intercepts; Strict = equal residuals."
  )
  total_cols <- ncol(tbl)
  css <- apa7_css(11)

  k <- kable(tbl, caption = NULL,
             col.names = c("Level", "χ²", "<i>df</i>", "CFI", "RMSEA", "SRMR",
                           "ΔCFI", "ΔRMSEA", "Decision"),
             align = c("l", "r", "r", "r", "r", "r", "r", "r", "l"),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(tbl)), extra_css = css$body)

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# =============================================================================
# 4. POWER ANALYSIS TABLES
# =============================================================================

# --- Minimum Detectable Effects Table ---
apa_table_power_mde <- function(samples,
                                title = "Minimum Detectable Effect Sizes (alpha = .05)",
                                note = NULL) {
  
  if (!requireNamespace("WebPower", quietly = TRUE))
    stop("Install WebPower: install.packages('WebPower')")
  library(WebPower)
  
  models <- list(
    "Main effect (9 predictors)" = list(p1 = 8, p2 = 1),
    "2-way interaction (11 predictors)" = list(p1 = 10, p2 = 1),
    "3-way interaction (16 predictors)" = list(p1 = 15, p2 = 1)
  )
  
  tbl <- map_dfr(names(samples), function(sample_name) {
    n_obs <- samples[[sample_name]]
    map_dfr(names(models), function(model_name) {
      m <- models[[model_name]]
      res_80 <- wp.regression(n = n_obs, p1 = m$p1, p2 = m$p2,
                              alpha = 0.05, power = 0.80)
      res_90 <- wp.regression(n = n_obs, p1 = m$p1, p2 = m$p2,
                              alpha = 0.05, power = 0.90)
      tibble(
        Sample = paste0(sample_name, " (n = ", n_obs, ")"),
        Model = model_name,
        `f2 (80%)` = sprintf("%.4f", res_80$f2),
        `dR2 (80%)` = sprintf("%.4f", res_80$f2 / (1 + res_80$f2)),
        `f2 (90%)` = sprintf("%.4f", res_90$f2),
        `dR2 (90%)` = sprintf("%.4f", res_90$f2 / (1 + res_90$f2))
      )
    })
  })
  
  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "<i>f</i>² = Cohen's effect size for regression. ",
    "Δ<i>R</i>² = incremental variance explained by the focal predictor. ",
    "Small <i>f</i>² = .02, medium = .15, large = .35 (Cohen, 1988)."
  )
  css <- apa7_css(11)
  total_cols <- ncol(tbl)

  k <- kable(tbl, caption = NULL,
             col.names = c("Sample", "Model",
                           "<i>f</i>² (80%)", "Δ<i>R</i>² (80%)",
                           "<i>f</i>² (90%)", "Δ<i>R</i>² (90%)"),
             align = c("l", "l", rep("r", 4)),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(tbl)), extra_css = css$body) %>%
    collapse_rows(columns = 1, valign = "top")

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# --- Power at Fixed Effect Sizes Table ---
apa_table_power_fixed <- function(samples,
                                  f2_values = c(0.01, 0.02, 0.05, 0.10, 0.15),
                                  title = "Statistical Power at Fixed Effect Sizes (alpha = .05)",
                                  note = NULL) {
  
  if (!requireNamespace("WebPower", quietly = TRUE))
    stop("Install WebPower: install.packages('WebPower')")
  library(WebPower)
  
  models <- list("Main effect" = 8, "2-way" = 10, "3-way" = 15)
  
  tbl <- map_dfr(names(samples), function(sample_name) {
    n_obs <- samples[[sample_name]]
    map_dfr(f2_values, function(f2) {
      dR2 <- f2 / (1 + f2)
      row <- tibble(
        Sample = paste0(sample_name, " (n = ", n_obs, ")"),
        `f2` = sprintf("%.2f", f2),
        `dR2` = sprintf("%.3f", dR2)
      )
      for (model_name in names(models)) {
        p1 <- models[[model_name]]
        res <- wp.regression(n = n_obs, p1 = p1, p2 = 1, f2 = f2,
                             alpha = 0.05, power = NULL)
        row[[model_name]] <- paste0(sprintf("%.1f", res$power * 100), "%")
      }
      row
    })
  })
  
  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "Power computed for testing a single coefficient. ",
    "Adequate power ≥ 80%."
  )
  css <- apa7_css(11)
  total_cols <- ncol(tbl)

  k <- kable(tbl, caption = NULL,
             col.names = c("Sample", "<i>f</i>²", "Δ<i>R</i>²", names(models)),
             align = c("l", "r", "r", rep("r", length(models))),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(tbl)), extra_css = css$body) %>%
    collapse_rows(columns = 1, valign = "top")

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# --- Power for Standardized Betas Table ---
apa_table_power_beta <- function(n_obs, n_predictors, sample_name,
                                 beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20),
                                 r_squared_x = 0.3, alpha = 0.05,
                                 title = NULL, note = NULL) {
  
  df_resid <- n_obs - n_predictors - 1
  t_crit <- qt(1 - alpha/2, df_resid)
  
  tbl <- tibble(
    Beta = sprintf("%.2f", beta_values),
    `SE(Beta)` = NA_character_,
    NCP = NA_character_,
    Power = NA_character_
  )
  
  for (i in seq_along(beta_values)) {
    beta <- beta_values[i]
    se_beta <- 1 / sqrt((1 - r_squared_x) * df_resid)
    ncp <- beta / se_beta
    power <- 1 - pt(t_crit, df_resid, ncp) + pt(-t_crit, df_resid, ncp)
    tbl$`SE(Beta)`[i] <- sprintf("%.4f", se_beta)
    tbl$NCP[i] <- sprintf("%.2f", ncp)
    tbl$Power[i] <- paste0(sprintf("%.1f", power * 100), "%")
  }
  
  if (is.null(title))
    title <- paste0("Power for Standardized Beta: ", sample_name,
                    " (n = ", n_obs, ", k = ", n_predictors, ")")
  
  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "<i>R</i>²_x = ", r_squared_x,
    " (multicollinearity of focal predictor with other predictors). ",
    "α = ", alpha, ", two-tailed."
  )
  css <- apa7_css(11)
  total_cols <- 4L

  k <- kable(tbl, caption = NULL,
             col.names = c("β", "<i>SE</i>(β)", "NCP", "Power"),
             align = c("r", "r", "r", "r"),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(tbl)), extra_css = css$body)

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# =============================================================================
# 5. DESCRIPTIVE STATISTICS TABLE
# =============================================================================

apa_table_descriptives <- function(data, vars, var_names = NULL,
                                   scale_items = NULL, ids = NULL,
                                   id_col = "M2ID",
                                   title = "Descriptive Statistics", note = NULL) {

  if (is.null(var_names)) var_names <- vars

  # If regression-sample IDs were provided, lock `data` to exactly those rows
  # before any descriptive / reliability statistic is computed.
  if (!is.null(ids)) {
    if (!id_col %in% names(data))
      stop("ID column '", id_col, "' not found in data.")
    data <- data[data[[id_col]] %in% ids, , drop = FALSE]
    if (!setequal(data[[id_col]], ids))
      stop("Descriptive data IDs do not match regression IDs.")
    if (nrow(data) != length(ids))
      stop("Descriptive data row count (", nrow(data),
           ") != regression sample size (", length(ids), ").")
  }

  # Standardized Cronbach's alpha for multi-item scales. `data` has already
  # been locked to the regression-sample IDs above, so we restrict to that
  # sample and then keep respondents with complete raw-item data on each
  # scale (per-scale listwise). `n` returned = number of regression-sample
  # respondents with complete item data for that scale.
  n_analytic <- nrow(data)
  compute_alpha <- function(v) {
    items <- scale_items[[v]]
    if (is.null(items) || length(items) < 2)
      return(list(alpha = NA_real_, n = NA_integer_))
    item_cols <- intersect(items, names(data))
    if (length(item_cols) < 2)
      return(list(alpha = NA_real_, n = NA_integer_))
    item_df <- as.data.frame(data[, item_cols, drop = FALSE])
    item_df <- item_df[complete.cases(item_df), , drop = FALSE]
    n_used <- nrow(item_df)
    if (n_used < 2) return(list(alpha = NA_real_, n = n_used))
    res <- try(suppressWarnings(suppressMessages(
      psych::alpha(item_df, check.keys = TRUE, warnings = FALSE,
                   use = "complete.obs")
    )), silent = TRUE)
    if (inherits(res, "try-error"))
      return(list(alpha = NA_real_, n = n_used))
    list(alpha = as.numeric(res$total$std.alpha), n = n_used)
  }

  tbl <- map2_dfr(vars, var_names, function(v, vn) {
    x <- data[[v]]
    # Coerce factor (e.g., recoded gender) to 0/1 numeric for descriptives
    if (is.factor(x)) x <- as.numeric(x) - 1
    x <- x[!is.na(x)]
    a_info <- if (!is.null(scale_items)) compute_alpha(v)
              else list(alpha = NA_real_, n = NA_integer_)
    tibble(
      Variable = vn,
      n = length(x),
      M = sprintf("%.2f", mean(x)),
      SD = sprintf("%.2f", sd(x)),
      Min = sprintf("%.2f", min(x)),
      Max = sprintf("%.2f", max(x)),
      `α` = ifelse(is.na(a_info$alpha), "—",
                        sub("^0", "", sprintf("%.2f", a_info$alpha))),
      .alpha_n = a_info$n
    )
  })

  # --- Reliability sample report ---
  # Alphas are computed per-scale on the regression sample subset to
  # respondents with complete raw-item data. n_alpha <= regression N
  # whenever the pre-scored scale was valid for a respondent missing >= 1
  # raw item; this is expected and informational, not an error.
  if (!is.null(scale_items)) {
    chk <- tbl %>% filter(!is.na(.alpha_n))
    cat("\n--- Reliability n (regression sample N = ", n_analytic,
        ") ---\n", sep = "")
    for (i in seq_len(nrow(chk))) {
      drop_n <- n_analytic - chk$.alpha_n[i]
      cat(sprintf("  %-32s  n_alpha = %4d  (%d dropped for item-level NA)\n",
                  chk$Variable[i], chk$.alpha_n[i], drop_n))
    }
  }

  tbl <- tbl %>% select(-.alpha_n)

  full_note <- paste0(
    if (!is.null(note)) paste0(note, "\n") else "",
    "α = standardized Cronbach's alpha, computed on the regression sample ",
    "restricted to respondents with complete raw-item data on that scale. ",
    "— = not applicable (single-item or demographic variable)."
  )

  css <- apa7_css(11)
  total_cols <- ncol(tbl)

  k <- kable(tbl, caption = NULL, align = c("l", rep("r", 6)),
             col.names = c("Variable", "<i>n</i>", "<i>M</i>", "<i>SD</i>",
                           "Min", "Max", "α"),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(tbl)), extra_css = css$body_wrap)

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# =============================================================================
# 6. CORRELATION MATRIX TABLE
# =============================================================================

apa_table_correlations <- function(data, vars, var_names = NULL,
                                   title = "Bivariate Correlations", note = NULL) {
  
  if (is.null(var_names)) var_names <- vars

  # Coerce any factor columns (e.g., recoded gender) to 0/1 numeric for cor()
  data <- data
  for (v in vars) {
    if (is.factor(data[[v]])) data[[v]] <- as.numeric(data[[v]]) - 1
  }
  cor_mat <- cor(data[vars], use = "pairwise.complete.obs")
  n_vars <- length(vars)
  
  # Lower triangle with stars
  tbl <- matrix("", n_vars, n_vars)
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i == j) {
        tbl[i, j] <- "---"
      } else if (j < i) {
        r <- cor_mat[i, j]
        n_pair <- sum(complete.cases(data[c(vars[i], vars[j])]))
        t_val <- r * sqrt(n_pair - 2) / sqrt(1 - r^2)
        p_val <- 2 * pt(abs(t_val), n_pair - 2, lower.tail = FALSE)
        tbl[i, j] <- paste0(sprintf("%.3f", r), format_p_stars(p_val))
      }
    }
  }
  
  # Apply non-breaking minus to correlation cells so e.g. "‑.342*" stays
  # on one line in Word.
  tbl[] <- lapply(tbl, function(col) {
    if (is.character(col)) nb_minus(col) else col
  })

  tbl <- as.data.frame(tbl)
  names(tbl) <- seq_len(n_vars)
  tbl <- bind_cols(tibble(Variable = paste0(seq_len(n_vars), ". ", var_names)), tbl)

  full_note <- paste0(if (!is.null(note)) paste0(note, "\n") else "",
                      "*<i>p</i> &lt; .05. **<i>p</i> &lt; .01. ***<i>p</i> &lt; .001.")
  css <- apa7_css(11)
  total_cols <- ncol(tbl)

  k <- kable(tbl, caption = NULL, align = c("l", rep("r", n_vars)),
             escape = FALSE,
             table.attr = paste0("style=\"", css$table, "\"")) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css$head) %>%
    row_spec(seq_len(nrow(tbl)), extra_css = css$body_wrap)

  apa7_tfoot(k, total_cols, full_note, font_size = 11)
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================
#
# Output directory structure:
#   output/tables/regressions/   <- regression HTML + Word tables
#   output/tables/multiverse/    <- specification-curve tables
#   output/tables/cfa/           <- CFA fit, loadings, invariance
#   output/tables/power/         <- MDE and power tables
#   output/tables/descriptives/  <- descriptive stats & correlations
#
# --- After running 03_regressions.R ---
#
# # Main effects tables -> output/tables/regressions/
# tbl1 <- apa_table_multi(
#   models = list(m1_negwf_m2, m2_negfw_m2, m3_poswf_m2, m4_posfw_m2),
#   model_names = c("Neg W->F", "Neg F->W", "Pos W->F", "Pos F->W"),
#   title = "Table 1\nMain Effects of EF on Work-Family Spillover (MIDUS 2)")
# apa_save_html(tbl1, "table01_main_effects_m2.html", DIRS$regressions)
#
# # Moderation tables -> output/tables/regressions/
# tbl5 <- apa_table_multi(
#   models = list(m9_super_negfw_m2, m10_super_negwf_m2,
#                 m11_super_posfw_m2, m12_super_poswf_m2),
#   model_names = c("Neg F->W", "Neg W->F", "Pos F->W", "Pos W->F"),
#   title = "Table 5\nSupervisor Support x EF Interactions (MIDUS 2)")
# apa_save_html(tbl5, "table05_supervisor_moderation_m2.html", DIRS$regressions)
#
# # Word export -> output/tables/regressions/
# apa_export_word(m1_negwf_m2, "table01_main_negwf_m2.docx",
#                 "Main Effects: Neg W->F (MIDUS 2)", dir = DIRS$regressions)
#
# --- After running 04_multiverse.R ---
#
# # Multiverse tables -> output/tables/multiverse/
# results_m2 <- bind_rows(
#   extract_ef_results(m1_nocov_m2, "NEGWF", "No covariates", "MIDUS 2"),
#   extract_ef_results(m2_nocov_m2, "NEGFW", "No covariates", "MIDUS 2"),
#   extract_ef_results(m1_demog_m2, "NEGWF", "Demographics only", "MIDUS 2"),
#   # ... all other multiverse models ...
# )
# tbl_mv <- apa_table_multiverse(results_m2, title = "Multiverse Analysis (MIDUS 2)")
# apa_save_html(tbl_mv, "multiverse_long_m2.html", DIRS$multiverse)
#
# tbl_mw <- apa_table_multiverse_wide(results_m2,
#   title = "Multiverse Summary: EF Main Effects (MIDUS 2)")
# apa_save_html(tbl_mw, "multiverse_wide_m2.html", DIRS$multiverse)
#
# --- After running 05_cfa_invariance.R ---
#
# # CFA tables -> output/tables/cfa/
# tbl_fit <- apa_table_cfa_fit(list(fit_m2, fit_m3), c("MIDUS 2", "MIDUS 3"),
#   title = "CFA Fit Indices for 4-Factor Spillover Model")
# apa_save_html(tbl_fit, "cfa_fit_indices.html", DIRS$cfa)
#
# tbl_load <- apa_table_cfa_loadings(fit_m2, title = "Standardized Loadings (MIDUS 2)")
# apa_save_html(tbl_load, "cfa_loadings_m2.html", DIRS$cfa)
#
# tbl_cor <- apa_table_cfa_cors(fit_m2, title = "Factor Correlations (MIDUS 2)")
# apa_save_html(tbl_cor, "cfa_factor_correlations_m2.html", DIRS$cfa)
#
# tbl_inv <- apa_table_invariance(
#   list(Configural = inv_m2$configural, Metric = inv_m2$metric,
#        Scalar = inv_m2$scalar, Strict = inv_m2$strict),
#   title = "Measurement Invariance Across Gender (MIDUS 2)")
# apa_save_html(tbl_inv, "invariance_gender_m2.html", DIRS$cfa)
#
# --- After running 06_power_analysis.R ---
#
# # Power tables -> output/tables/power/
# samples <- list("MIDUS II" = 1300, "MIDUS III" = 670)
#
# tbl_mde <- apa_table_power_mde(samples)
# apa_save_html(tbl_mde, "power_mde.html", DIRS$power)
#
# tbl_pf <- apa_table_power_fixed(samples)
# apa_save_html(tbl_pf, "power_fixed_effects.html", DIRS$power)
#
# tbl_b1 <- apa_table_power_beta(1300, 8, "MIDUS II - Main Effects")
# apa_save_html(tbl_b1, "power_beta_m2_main.html", DIRS$power)
#
# tbl_b2 <- apa_table_power_beta(1300, 10, "MIDUS II - 2-Way Interactions")
# apa_save_html(tbl_b2, "power_beta_m2_2way.html", DIRS$power)
#
# tbl_b3 <- apa_table_power_beta(670, 8, "MIDUS III - Main Effects")
# apa_save_html(tbl_b3, "power_beta_m3_main.html", DIRS$power)
#
# --- Descriptive statistics -> output/tables/descriptives/ ---
#
# tbl_desc <- apa_table_descriptives(df_m2_all, vars = ..., var_names = ...)
# apa_save_html(tbl_desc, "descriptives_m2.html", DIRS$descriptives)
#
# tbl_corr <- apa_table_correlations(df_m2_all, vars = ..., var_names = ...)
# apa_save_html(tbl_corr, "correlations_m2.html", DIRS$descriptives)
#
# --- Replicability tables -> output/tables/replicability/ ---
#
# tbl_rep <- apa_table_replicability(reg_replicability,
#   title = "Cross-Wave Replicability: Primary Regressions (OSC, 2015)")
# apa_save_html(tbl_rep, "replicability_regressions.html", DIRS$replicability)
#
# tbl_rep_mv <- apa_table_replicability(mv_replicability,
#   title = "Cross-Wave Replicability: Multiverse Specifications (OSC, 2015)")
# apa_save_html(tbl_rep_mv, "replicability_multiverse.html", DIRS$replicability)

# =============================================================================
# AUTO-GENERATE TABLES (if data available from upstream scripts)
# =============================================================================

# --- Regression tables (rebuilt from model objects in 03_regressions.R) ---

if (exists("m1_negwf_m2")) {
  reg_specs <- list(
    list(n = 1,  models = list(m1_negwf_m2, m2_negfw_m2, m3_poswf_m2, m4_posfw_m2),
         names = c("Neg W\u2192F", "Neg F\u2192W", "Pos W\u2192F", "Pos F\u2192W"),
         title = "Main Effects of Executive Functioning on Work-Family Spillover (MIDUS 2)",
         file  = "table01_main_effects_m2"),
    list(n = 2,  models = list(m1_negwf_m3, m2_negfw_m3, m3_poswf_m3, m4_posfw_m3),
         names = c("Neg W\u2192F", "Neg F\u2192W", "Pos W\u2192F", "Pos F\u2192W"),
         title = "Main Effects of Executive Functioning on Work-Family Spillover (MIDUS 3)",
         file  = "table02_main_effects_m3"),
    list(n = 3,  models = list(m5_gender_negfw_m2, m6_gender_negwf_m2, m7_gender_posfw_m2, m8_gender_poswf_m2),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Gender \u00D7 Executive Functioning Interactions (MIDUS 2)",
         file  = "table03_gender_moderation_m2"),
    list(n = 4,  models = list(m5_gender_negfw_m3, m6_gender_negwf_m3, m7_gender_posfw_m3, m8_gender_poswf_m3),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Gender \u00D7 Executive Functioning Interactions (MIDUS 3)",
         file  = "table04_gender_moderation_m3"),
    list(n = 5,  models = list(m9_super_negfw_m2, m10_super_negwf_m2, m11_super_posfw_m2, m12_super_poswf_m2),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Supervisor Support \u00D7 Executive Functioning Interactions (MIDUS 2)",
         file  = "table05_supervisor_moderation_m2"),
    list(n = 6,  models = list(m9_super_negfw_m3, m10_super_negwf_m3, m11_super_posfw_m3, m12_super_poswf_m3),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Supervisor Support \u00D7 Executive Functioning Interactions (MIDUS 3)",
         file  = "table06_supervisor_moderation_m3"),
    list(n = 7,  models = list(m13_cowork_negfw_m2, m14_cowork_negwf_m2, m15_cowork_posfw_m2, m16_cowork_poswf_m2),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Coworker Support \u00D7 Executive Functioning Interactions (MIDUS 2)",
         file  = "table07_coworker_moderation_m2"),
    list(n = 8,  models = list(m13_cowork_negfw_m3, m14_cowork_negwf_m3, m15_cowork_posfw_m3, m16_cowork_poswf_m3),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Coworker Support \u00D7 Executive Functioning Interactions (MIDUS 3)",
         file  = "table08_coworker_moderation_m3"),
    list(n = 9,  models = list(m17_spouse_negfw_m2, m18_spouse_negwf_m2, m19_spouse_posfw_m2, m20_spouse_poswf_m2),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Spousal Support \u00D7 Executive Functioning Interactions (MIDUS 2)",
         file  = "table09_spousal_moderation_m2"),
    list(n = 10, models = list(m17_spouse_negfw_m3, m18_spouse_negwf_m3, m19_spouse_posfw_m3, m20_spouse_poswf_m3),
         names = c("Neg F\u2192W", "Neg W\u2192F", "Pos F\u2192W", "Pos W\u2192F"),
         title = "Spousal Support \u00D7 Executive Functioning Interactions (MIDUS 3)",
         file  = "table10_spousal_moderation_m3")
  )

  # Slug used in the per-outcome filename.
  outcome_slug_map <- c(
    "Neg W→F" = "negwf", "Neg F→W" = "negfw",
    "Pos W→F" = "poswf", "Pos F→W" = "posfw"
  )

  # One HTML file per (spec, outcome). Files are suffixed a-d so the four
  # outcomes for a given spec sit alphabetically together. No in-table
  # title is added — outcome identity is conveyed by the filename and by
  # the dissertation's external Table N caption.
  outcome_letter <- c("a", "b", "c", "d")
  for (spec in reg_specs) {
    for (j in seq_along(spec$models)) {
      short_name <- spec$names[j]
      slug       <- if (short_name %in% names(outcome_slug_map))
                      unname(outcome_slug_map[short_name])
                    else gsub("[^A-Za-z0-9]+", "", short_name)
      apa_save_html(
        apa_table_single(spec$models[[j]]),
        paste0(spec$file, "_", outcome_letter[j], "_", slug, ".html"),
        DIRS$regressions)
    }
  }

  # --- Uncentered (raw) interaction tables (parallel folder) ---------------
  # For each interaction spec (tables 3-10), look up the `_raw` sibling of
  # each centered model and emit a sibling HTML file under
  # output/tables/regressions_uncentered/. Main-effects specs (tables 1-2)
  # don't have _raw versions (centering is irrelevant without interactions),
  # so they are skipped here. Filenames mirror the centered ones but with a
  # `_raw_uncentered.html` suffix so they sort alongside cleanly.
  raw_model_for <- function(centered_model) {
    # `_raw` objects are named after the global symbol that holds the
    # centered model. Recover that name from the call expression that
    # was used to build the spec.
    nm <- deparse(substitute(centered_model))
    raw_name <- paste0(nm, "_raw")
    if (exists(raw_name, envir = .GlobalEnv)) get(raw_name, envir = .GlobalEnv)
    else NULL
  }
  for (spec in reg_specs) {
    # Tables 1-2 are main effects; raw == centered there, skip.
    if (!grepl("moderation", spec$file, fixed = TRUE)) next
    for (j in seq_along(spec$models)) {
      centered_mod <- spec$models[[j]]
      # Walk the list of all `_raw` global symbols and find the one whose
      # coefficients match — easier: spec$models was built by name lookup,
      # so we can find the original name via a reverse-search of m{1..20}.
      raw_mod <- NULL
      for (cand in ls(envir = .GlobalEnv, pattern = "^m[0-9]+_.*_raw$")) {
        if (identical(get(sub("_raw$", "", cand), envir = .GlobalEnv),
                      centered_mod)) {
          raw_mod <- get(cand, envir = .GlobalEnv)
          break
        }
      }
      if (is.null(raw_mod)) next  # no matching _raw model -> skip silently
      short_name <- spec$names[j]
      slug <- if (short_name %in% names(outcome_slug_map))
                unname(outcome_slug_map[short_name])
              else gsub("[^A-Za-z0-9]+", "", short_name)
      apa_save_html(
        apa_table_single(raw_mod),
        paste0(spec$file, "_", outcome_letter[j], "_", slug,
               "_raw_uncentered.html"),
        DIRS$regressions_uncentered)
    }
  }
}

# --- Multiverse tables (from 04_multiverse.R) ---

if (exists("results_m2")) {
  mv_long_m2 <- apa_table_multiverse(
    results_m2,
    title = apa_caption(11, "Multiverse Analysis: EF Effects Across Specifications (MIDUS 2)"))
  apa_save_html(mv_long_m2, "multiverse_long_m2.html", DIRS$multiverse)

  mv_wide_m2 <- apa_table_multiverse_wide(
    results_m2,
    title = apa_caption(12, "Multiverse Summary: EF Main Effects (MIDUS 2)"))
  apa_save_html(mv_wide_m2, "multiverse_wide_m2.html", DIRS$multiverse)
}

if (exists("results_m3")) {
  mv_long_m3 <- apa_table_multiverse(
    results_m3,
    title = apa_caption(13, "Multiverse Analysis: EF Effects Across Specifications (MIDUS 3)"))
  apa_save_html(mv_long_m3, "multiverse_long_m3.html", DIRS$multiverse)

  mv_wide_m3 <- apa_table_multiverse_wide(
    results_m3,
    title = apa_caption(14, "Multiverse Summary: EF Main Effects (MIDUS 3)"))
  apa_save_html(mv_wide_m3, "multiverse_wide_m3.html", DIRS$multiverse)
}

# --- CFA tables (from 05_cfa_invariance.R) ---

if (exists("fit_m2")) {
  cfa_fits <- list(fit_m2)
  cfa_names <- c("MIDUS 2")
  if (exists("fit_m3")) {
    cfa_fits <- list(fit_m2, fit_m3)
    cfa_names <- c("MIDUS 2", "MIDUS 3")
  }
  apa_save_html(
    apa_table_cfa_fit(cfa_fits, cfa_names,
      title = apa_caption(15, "Confirmatory Factor Analysis Model Fit Indices")),
    "cfa_fit_indices.html", DIRS$cfa)
  apa_save_html(
    apa_table_cfa_loadings(fit_m2,
      title = apa_caption(16, "Standardized Factor Loadings (MIDUS 2)")),
    "cfa_loadings_m2.html", DIRS$cfa)
  apa_save_html(
    apa_table_cfa_cors(fit_m2,
      title = apa_caption(17, "Latent Factor Correlations (MIDUS 2)")),
    "cfa_factor_correlations_m2.html", DIRS$cfa)
}

if (exists("fit_m3")) {
  apa_save_html(
    apa_table_cfa_loadings(fit_m3,
      title = apa_caption(18, "Standardized Factor Loadings (MIDUS 3)")),
    "cfa_loadings_m3.html", DIRS$cfa)
  apa_save_html(
    apa_table_cfa_cors(fit_m3,
      title = apa_caption(19, "Latent Factor Correlations (MIDUS 3)")),
    "cfa_factor_correlations_m3.html", DIRS$cfa)
}

if (exists("inv_m2")) {
  apa_save_html(
    apa_table_invariance(
      list(Configural = inv_m2$configural, Metric = inv_m2$metric,
           Scalar = inv_m2$scalar, Strict = inv_m2$strict),
      title = apa_caption(20, "Measurement Invariance Across Gender (MIDUS 2)")),
    "invariance_gender_m2.html", DIRS$cfa)
}

if (exists("inv_m3")) {
  apa_save_html(
    apa_table_invariance(
      list(Configural = inv_m3$configural, Metric = inv_m3$metric,
           Scalar = inv_m3$scalar, Strict = inv_m3$strict),
      title = apa_caption(21, "Measurement Invariance Across Gender (MIDUS 3)")),
    "invariance_gender_m3.html", DIRS$cfa)
}

# --- Power tables (from 06_power_analysis.R) ---

if (exists("samples")) {
  apa_save_html(
    apa_table_power_mde(samples,
      title = apa_caption(22, "Minimum Detectable Effect Sizes (\u03B1 = .05)")),
    "power_mde.html", DIRS$power)
  apa_save_html(
    apa_table_power_fixed(samples,
      title = apa_caption(23, "Statistical Power at Fixed Effect Sizes (\u03B1 = .05)")),
    "power_fixed_effects.html", DIRS$power)

  beta_vals <- c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20)
  power_tbl_num <- 24
  for (s in names(samples)) {
    n <- samples[[s]]
    tag <- tolower(gsub(" ", "", s))
    for (k in c(8, 10, 15)) {
      k_label <- switch(as.character(k),
                         "8" = "Main Effects", "10" = "2-Way Interactions",
                         "15" = "3-Way Interactions")
      apa_save_html(
        apa_table_power_beta(n, k, paste0(s, " - ", k_label),
                             beta_values = beta_vals,
                             title = apa_caption(power_tbl_num,
                               paste0("Power for Standardized \u03B2: ",
                                      s, " (n = ", n, ", k = ", k, ")"))),
        paste0("power_beta_", tag, "_k", k, ".html"), DIRS$power)
      power_tbl_num <- power_tbl_num + 1
    }
  }
}

# --- Descriptive tables (from 03b_descriptives.R) ---

desc_tbl_num <- 30
if (exists("descriptives_data")) {
  for (wave in names(descriptives_data)) {
    dd <- descriptives_data[[wave]]
    wave_label <- if (wave == "m2") "MIDUS 2" else "MIDUS 3"
    n_obs <- nrow(dd$data)

    desc_tbl <- apa_table_descriptives(
      dd$data, vars = dd$vars, var_names = dd$labels,
      scale_items = dd$scale_items, ids = dd$ids,
      title = apa_caption(desc_tbl_num,
        paste0("Descriptive Statistics (", wave_label, ", N = ", n_obs, ")")))
    apa_save_html(desc_tbl,
                  paste0("descriptives_", wave, ".html"), DIRS$descriptives)
    desc_tbl_num <- desc_tbl_num + 1

    cor_tbl <- apa_table_correlations(
      dd$data, vars = dd$vars, var_names = dd$labels,
      title = apa_caption(desc_tbl_num,
        paste0("Bivariate Correlations (", wave_label, ", N = ", n_obs, ")")))
    apa_save_html(cor_tbl,
                  paste0("correlations_", wave, ".html"), DIRS$descriptives)
    desc_tbl_num <- desc_tbl_num + 1
  }
}

# --- Replicability tables (from 03_regressions.R and 04_multiverse.R) ---
if (exists("reg_replicability")) {
  reg_replicability_table <- apa_table_replicability(
    reg_replicability,
    title = apa_caption(34, "Cross-Wave Replicability of Primary Regressions (OSC, 2015)"))
  apa_save_html(reg_replicability_table,
                "replicability_regressions.html", DIRS$replicability)
}

if (exists("mv_replicability")) {
  mv_replicability_table <- apa_table_replicability(
    mv_replicability,
    title = apa_caption(35, "Cross-Wave Replicability of Multiverse Specifications (OSC, 2015)"))
  apa_save_html(mv_replicability_table,
                "replicability_multiverse.html", DIRS$replicability)

  # Additional cross-wave multiverse view at 12pt with full verbose
  # specification labels (no abbreviation). Numeric / acronym / criterion
  # cells are sized so they stay on a single line in Word at 12pt Times;
  # the Specification column wraps to two or three lines per row, which
  # is the standard APA 7 treatment of long row labels.
  mv_replicability_midus <- mv_replicability %>%
    mutate(term_label = gsub("Executive Functioning", "EF",
                             term_label, fixed = TRUE))
  mv_replicability_midus_table <- apa_table_replicability(
    mv_replicability_midus,
    title       = apa_caption(35,
      "Cross-Wave Multiverse Results: MIDUS 2 ↔ MIDUS 3 (OSC, 2015)"),
    note        = paste0(
      "Outcome acronyms: NWF = negative work-to-family spillover; ",
      "NFW = negative family-to-work spillover; ",
      "PWF = positive work-to-family spillover; ",
      "PFW = positive family-to-work spillover. EF = executive functioning."),
    wave_labels = c("MIDUS 2", "MIDUS 3"),
    split_model = TRUE,
    # 12pt widths: numeric/Outcome/Term/criterion cells stay on one line,
    # Specification (column 1) wraps to 2-3 lines per row.
    # Total = 280 + 55 + 105 + 200 + 200 + 40 + 40 + 40 = 960 px ≈ 10".
    # Fits Letter landscape at 0.5" margins.
    col_widths  = c(Specification = 280L, Outcome = 55L, Term = 105L,
                    MIDUS2 = 200L, MIDUS3 = 200L,
                    Sign = 40L, Sig = 40L, CI = 40L))
  apa_save_html(mv_replicability_midus_table,
                "multiverse_cross_wave.html", DIRS$replicability)
}

cat("=============================================================================\n")
cat("APA TABLE FUNCTIONS LOADED SUCCESSFULLY\n")
cat("=============================================================================\n")
cat("\nOutput directories:\n")
cat("  output/tables/regressions/    Regression HTML + Word tables\n")
cat("  output/tables/multiverse/     Specification-curve tables\n")
cat("  output/tables/cfa/            CFA fit, loadings, invariance\n")
cat("  output/tables/power/          MDE and power tables\n")
cat("  output/tables/descriptives/   Descriptive stats & correlations\n")
cat("  output/tables/replicability/  Cross-wave replicability tables\n")
cat("\nAvailable functions:\n")
cat("  REGRESSION:     apa_table_single(), apa_table_multi()\n")
cat("  MULTIVERSE:     apa_table_multiverse(), apa_table_multiverse_wide()\n")
cat("  REPLICABILITY:  apa_table_replicability()\n")
cat("  CFA/MI:         apa_table_cfa_fit(), apa_table_cfa_loadings(),\n")
cat("                  apa_table_cfa_cors(), apa_table_invariance()\n")
cat("  POWER:          apa_table_power_mde(), apa_table_power_fixed(),\n")
cat("                  apa_table_power_beta()\n")
cat("  DESCRIPTIVES:   apa_table_descriptives(), apa_table_correlations()\n")
cat("  EXPORT:         apa_save_html()\n")
cat("  HELPER:         extract_ef_results()\n")
