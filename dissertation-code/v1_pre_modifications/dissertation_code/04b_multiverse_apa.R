# =============================================================================
# MULTIVERSE - APA-FORMATTED RESULTS TABLES (APA 7th ed.)
# Reads `results_all` from 04_multiverse.R and writes strict APA tables.
# Runs after 04_multiverse.R.
# =============================================================================

library(tidyverse)
library(knitr)
library(kableExtra)
library(htmltools)

apa_dir <- "output/tables/multiverse_apa"
dir.create(apa_dir, recursive = TRUE, showWarnings = FALSE)

# --- Formatting helpers ------------------------------------------------------
fmt_p <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p))))
}
stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
         ifelse(p < .01,  "**",
         ifelse(p < .05,  "*", ""))))
}
fmt3      <- function(x) sprintf("%.3f", x)
fmt_beta  <- function(x) sub("^(-?)0", "\\1", sprintf("%.3f", x))   # APA: no leading 0 for r/beta
fmt_p_lz  <- function(x) sub("^(-?)0", "\\1", fmt_p(x))             # ditto for p

# --- APA 7 CSS theme ---------------------------------------------------------
apa_css <- "
<style>
  /* Landscape US Letter content area (11in x 8.5in minus 1in margins) */
  @page { size: 11in 8.5in landscape; margin: 0.75in; }
  body { font-family: 'Times New Roman', Times, serif; font-size: 11pt;
         color: #000; background: #fff; margin: 24px;
         max-width: 9.5in; }
  .apa-table-number { font-weight: normal; margin: 0 0 2px 0; }
  .apa-table-title  { font-style: italic; font-weight: normal; margin: 0 0 8px 0; }

  /* APA table: no vertical rules; horizontal rules only at top, under
     header(s), and at bottom of body. White-space:nowrap prevents
     cell values from wrapping to a new line. */
  table.apa { border-collapse: collapse; border-spacing: 0;
              margin: 0; font-family: 'Times New Roman', Times, serif;
              font-size: 11pt; table-layout: auto; }
  table.apa, table.apa th, table.apa td { border: none; }
  table.apa th, table.apa td {
    padding: 3px 6px;
    vertical-align: bottom;
    white-space: nowrap;            /* keep values on one line */
  }
  table.apa thead tr:first-child th { border-top: 1px solid #000; }
  table.apa thead tr:last-child  th { border-bottom: 1px solid #000; }
  table.apa tbody tr:last-child  td { border-bottom: 1px solid #000; }
  /* Spanner header rule: bottom border under multi-column group labels */
  table.apa thead tr.apa-spanner th[colspan] { border-bottom: 1px solid #000; }
  table.apa thead tr.apa-spanner th         { padding-bottom: 2px; }

  /* APA column-header formatting: centered, regular weight, italic
     statistical symbols handled inline. */
  table.apa th { font-weight: normal; text-align: center; }
  table.apa td.apa-text { text-align: left;  white-space: normal; }  /* allow wrap in label cols */
  table.apa td.apa-num  { text-align: center; }

  .apa-note { font-size: 11pt; margin-top: 6px; max-width: 9.5in; }
  .apa-note .apa-italic { font-style: italic; }
  /* Secondary line inside packed multi-statistic cells */
  .apa-sub { font-size: 9.5pt; }
</style>
"

# Build a row of <th> for one header line. `row_class` lets us mark the
# spanner row so the APA rule under group labels renders.
.th_row <- function(labels, spans = rep(1, length(labels)), row_class = NULL) {
  cells <- mapply(function(lbl, sp) {
    sp_attr <- if (sp > 1) sprintf(' colspan="%d"', sp) else ""
    sprintf('<th%s>%s</th>', sp_attr, lbl)
  }, labels, spans)
  cls <- if (!is.null(row_class)) sprintf(' class="%s"', row_class) else ""
  sprintf("<tr%s>%s</tr>", cls, paste(cells, collapse = ""))
}

# Build APA HTML table from a data.frame, with optional grouped header
# df:           character data.frame (already formatted strings)
# col_classes:  vector of "text" or "num" per column (controls alignment)
# header_top:   optional list(labels = ..., spans = ...) for an upper header row
# header_main:  vector of column header labels (lower header row)
.apa_table_html <- function(df, col_classes, header_main, header_top = NULL) {
  thead <- ""
  if (!is.null(header_top)) {
    thead <- paste0(thead, .th_row(header_top$labels, header_top$spans,
                                   row_class = "apa-spanner"))
  }
  thead <- paste0(thead, .th_row(header_main))
  thead <- paste0("<thead>", thead, "</thead>")

  tbody_rows <- apply(df, 1, function(row) {
    cells <- mapply(function(val, cls) {
      sprintf('<td class="apa-%s">%s</td>', cls, val)
    }, row, col_classes)
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })
  tbody <- paste0("<tbody>", paste(tbody_rows, collapse = ""), "</tbody>")

  paste0('<table class="apa">', thead, tbody, "</table>")
}

# Wrap in APA shell (number, italic title, note) and save
apa_save <- function(table_html, table_num, title, note, file_out) {
  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    apa_css, "</head><body>",
    sprintf('<p class="apa-table-number">Table %s</p>', table_num),
    sprintf('<p class="apa-table-title">%s</p>', title),
    table_html,
    sprintf('<p class="apa-note"><span class="apa-italic">Note.</span> %s</p>', note),
    "</body></html>"
  )
  writeLines(html, file_out)
}

# --- Output labels -----------------------------------------------------------
outcome_label <- c(
  NEGWF = "Neg W→F",
  NEGFW = "Neg F→W",
  POSWF = "Pos W→F",
  POSFW = "Pos F→W"
)

is_interaction <- function(term) str_detect(term, ":")

# --- Working table -----------------------------------------------------------
mv <- results_all %>%
  mutate(
    is_int      = is_interaction(term),
    outcome_lbl = unname(outcome_label[outcome]),
    ci_lo       = estimate - 1.96 * std.error,
    ci_hi       = estimate + 1.96 * std.error,
    sig         = !is.na(p.value) & p.value < .05
  )

# =============================================================================
# TABLE 1: EF main effects across the multiverse
# =============================================================================
tbl_main <- mv %>%
  filter(!is_int) %>%
  transmute(
    wave, specification, outcome_lbl,
    B    = paste0(fmt3(estimate), stars(p.value)),
    SE   = fmt3(std.error),
    Beta = fmt_beta(beta),
    CI   = sprintf("[%s, %s]", fmt3(ci_lo), fmt3(ci_hi)),
    p    = fmt_p_lz(p.value),
    N    = as.character(n),
    R2   = fmt_beta(r_squared)
  )

render_main_table <- function(df_wave, wave_lbl, table_num, file_out) {
  outcomes_in_order <- unname(outcome_label)

  # Build a packed two-line cell per (spec, outcome)
  packed <- df_wave %>%
    mutate(
      outcome_lbl = factor(outcome_lbl, levels = outcomes_in_order),
      cell = paste0(
        B, " (", SE, ") ", CI,
        "<br><span class='apa-sub'>",
        "&beta;&nbsp;=&nbsp;", Beta,
        ", <i>p</i>&nbsp;=&nbsp;", p,
        ", <i>R</i><sup>2</sup>&nbsp;=&nbsp;", R2,
        "</span>"
      )
    ) %>%
    select(specification, N, outcome_lbl, cell)

  wide <- packed %>%
    pivot_wider(id_cols = c(specification, N),
                names_from = outcome_lbl,
                values_from = cell) %>%
    select(specification, all_of(outcomes_in_order), N)

  header_main <- c("Specification",
                   outcomes_in_order,
                   "<i>N</i>")
  col_classes <- c("text", rep("num", length(outcomes_in_order)), "num")

  tbl_html <- .apa_table_html(
    df          = wide,
    col_classes = col_classes,
    header_main = header_main
  )

  title <- sprintf(
    "Multiverse Analysis of Executive Functioning Main Effects on Work–Family Spillover (%s)",
    wave_lbl
  )
  note <- paste0(
    "Each cell reports the executive functioning coefficient from one ",
    "multiverse specification, formatted as ",
    "<span class='apa-italic'>B</span> (<span class='apa-italic'>SE</span>) ",
    "[95% CI] on the first line and ",
    "&beta;, <span class='apa-italic'>p</span>, ",
    "<span class='apa-italic'>R</span><sup>2</sup> on the second. ",
    "<span class='apa-italic'>B</span> = unstandardized regression coefficient; ",
    "<span class='apa-italic'>SE</span> = standard error; ",
    "&beta; = standardized regression coefficient; ",
    "CI = confidence interval; ",
    "<span class='apa-italic'>N</span> = sample size (constant across outcomes within a specification); ",
    "<span class='apa-italic'>R</span><sup>2</sup> = coefficient of determination.<br>",
    "* <span class='apa-italic'>p</span> &lt; .05. ",
    "** <span class='apa-italic'>p</span> &lt; .01. ",
    "*** <span class='apa-italic'>p</span> &lt; .001."
  )

  apa_save(tbl_html, table_num, title, note, file_out)
}

render_main_table(filter(tbl_main, wave == "MIDUS 2"), "MIDUS 2", "1",
                  file.path(apa_dir, "table1_main_effects_m2.html"))
render_main_table(filter(tbl_main, wave == "MIDUS 3"), "MIDUS 3", "2",
                  file.path(apa_dir, "table1_main_effects_m3.html"))

# =============================================================================
# TABLE 2: EF Interaction terms across the multiverse
# =============================================================================
tbl_int <- mv %>%
  filter(is_int) %>%
  mutate(
    moderator = str_remove(term, "^[^:]+:"),
    moderator = recode(moderator,
      B1PRSEX     = "Gender",        C1PRSEX      = "Gender",
      B1SJCSS_sum = "Supervisor",    C1SJCSS_sum  = "Supervisor",
      B1SJCCS_sum = "Coworker",      C1SJCCS_sum  = "Coworker",
      B1SSPEMP_sum= "Spousal",       C1SSPEMP_sum = "Spousal",
      B1PAGE_M2   = "Age",           C1PRAGE      = "Age",
      B1PB1       = "Education",     C1PB1        = "Education",
      B1STINC1    = "Income",        C1STINC      = "Income",
      B1SJCDS_sum = "Job Demands",   C1SJCDS_sum  = "Job Demands",
      FAM_DEMANDS_M2 = "Family Demands",
      FAM_DEMANDS_M3 = "Family Demands"
    )
  ) %>%
  transmute(
    wave,
    Specification = specification,
    Moderator     = moderator,
    Outcome       = outcome_lbl,
    B             = paste0(fmt3(estimate), stars(p.value)),
    SE            = fmt3(std.error),
    Beta          = fmt_beta(beta),
    CI            = sprintf("[%s, %s]", fmt3(ci_lo), fmt3(ci_hi)),
    p             = fmt_p_lz(p.value),
    N             = as.character(n),
    R2            = fmt_beta(r_squared)
  ) %>%
  arrange(wave, Moderator, Specification, Outcome)

render_int_table <- function(df, wave_lbl, table_num, file_out) {
  df_w <- df %>% filter(wave == wave_lbl) %>% select(-wave)
  if (nrow(df_w) == 0) return(invisible(NULL))

  header_main <- c("Specification", "Moderator", "Outcome",
                   "<i>B</i>", "<i>SE</i>", "&beta;", "95% CI",
                   "<i>p</i>", "<i>N</i>", "<i>R</i><sup>2</sup>")
  col_classes <- c("text", "text", "text", rep("num", 7))

  tbl_html <- .apa_table_html(
    df          = df_w,
    col_classes = col_classes,
    header_main = header_main
  )

  title <- sprintf(
    "Multiverse Analysis of Executive Functioning × Moderator Interactions on Work–Family Spillover (%s)",
    wave_lbl
  )
  note <- paste0(
    "<span class='apa-italic'>B</span> = unstandardized regression coefficient; ",
    "<span class='apa-italic'>SE</span> = standard error; ",
    "&beta; = standardized regression coefficient; ",
    "CI = confidence interval. ",
    "Each row reports the executive functioning × moderator interaction ",
    "coefficient from one multiverse specification.<br>",
    "* <span class='apa-italic'>p</span> &lt; .05. ",
    "** <span class='apa-italic'>p</span> &lt; .01. ",
    "*** <span class='apa-italic'>p</span> &lt; .001."
  )

  apa_save(tbl_html, table_num, title, note, file_out)
}

render_int_table(tbl_int, "MIDUS 2", "3",
                 file.path(apa_dir, "table2_interactions_m2.html"))
render_int_table(tbl_int, "MIDUS 3", "4",
                 file.path(apa_dir, "table2_interactions_m3.html"))

# =============================================================================
# TABLE 3: Robustness summary
# =============================================================================
tbl_summary <- mv %>%
  mutate(term_type = ifelse(is_int, "EF × Moderator", "EF main effect")) %>%
  group_by(wave, term_type, outcome_lbl) %>%
  summarise(
    n_specs    = n(),
    n_sig      = sum(sig),
    pct_sig    = sprintf("%.1f", 100 * mean(sig)),
    median_b   = fmt_beta(median(beta, na.rm = TRUE)),
    range_b    = sprintf("[%s, %s]",
                         fmt_beta(min(beta, na.rm = TRUE)),
                         fmt_beta(max(beta, na.rm = TRUE))),
    median_p   = fmt_p_lz(median(p.value, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(wave, term_type, outcome_lbl) %>%
  transmute(
    Wave        = wave,
    `Term Type` = term_type,
    Outcome     = outcome_lbl,
    n_specs     = as.character(n_specs),
    n_sig       = as.character(n_sig),
    pct_sig     = pct_sig,
    median_b    = median_b,
    range_b     = range_b,
    median_p    = median_p
  )

header_main <- c("Wave", "Term Type", "Outcome",
                 "<i>k</i> Specs", "<i>k</i> Sig", "% Sig",
                 "Median &beta;", "&beta; Range",
                 "Median <i>p</i>")
col_classes <- c("text", "text", "text", rep("num", 6))
tbl_html <- .apa_table_html(
  df          = tbl_summary,
  col_classes = col_classes,
  header_main = header_main
)
title <- "Multiverse Robustness Summary by Wave, Term Type, and Outcome"
note <- paste0(
  "Robustness summary across all multiverse specifications. ",
  "% Sig = percentage of specifications with <span class='apa-italic'>p</span> &lt; .05. ",
  "&beta; = standardized regression coefficient."
)
apa_save(tbl_html, "5", title, note,
         file.path(apa_dir, "table3_robustness_summary.html"))

# Tidy long CSV for downstream use
write_csv(mv %>% select(-sig, -is_int, -outcome_lbl),
          file.path(apa_dir, "multiverse_results_long.csv"))

cat("APA multiverse tables written to:\n", apa_dir, "\n", sep = "")
cat("  - table1_main_effects_m2.html\n",
    "  - table1_main_effects_m3.html\n",
    "  - table2_interactions_m2.html\n",
    "  - table2_interactions_m3.html\n",
    "  - table3_robustness_summary.html\n",
    "  - multiverse_results_long.csv\n", sep = "")

stopifnot(
  "tbl_main empty"    = nrow(tbl_main) > 0,
  "tbl_int empty"     = nrow(tbl_int) > 0,
  "tbl_summary empty" = nrow(tbl_summary) > 0
)
cat("04b_multiverse_apa checks passed.\n")
