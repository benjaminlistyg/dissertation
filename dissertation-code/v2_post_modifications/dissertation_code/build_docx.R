# =============================================================================
# Build the native APA 7 Word document for v2 (post-modifications)
# =============================================================================
# Run interactively from inside v2_post_modifications/dissertation_code/.
# Loads the data + regression models from the upstream pipeline and writes
# results.docx into v2_post_modifications/output/consolidated-results/.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(broom); library(stringr); library(purrr)
  library(haven); library(officer)
  library(flextable, exclude = "footnote")
})

# Mask guard
select <- dplyr::select
filter <- dplyr::filter

cat("[1/4] Sourcing upstream pipeline (01-06) ...\n")
source("01_midus_data_read.R")
source("02_midus_scoring.R")
source("03_regressions.R")
source("03b_descriptives.R")
source("03c_probe_interactions.R")
source("04_multiverse.R")
source("05_cfa_invariance.R")
source("06_power_analysis.R")

cat("[2/4] Sourcing 07 to load formatting helpers ...\n")
source("07_midus_apa_tables.R")

out_dir <- file.path("..", "output", "consolidated-results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- APA 7 styling primitives ----------------------------------------------
TIMES <- "Times New Roman"
PT <- 12L
PT_NOTE <- 11L

fp_text_body    <- fp_text(font.family = TIMES, font.size = PT)
fp_text_note    <- fp_text(font.family = TIMES, font.size = PT_NOTE)
fp_text_bold    <- fp_text(font.family = TIMES, font.size = PT, bold = TRUE)
fp_text_it      <- fp_text(font.family = TIMES, font.size = PT, italic = TRUE)
fp_text_note_it <- fp_text(font.family = TIMES, font.size = PT_NOTE, italic = TRUE)

thick <- fp_border(width = 1.5, color = "black")
thin  <- fp_border(width = 0.5, color = "black")

apa_ft <- function(ft, note = NULL, col_widths = NULL,
                   align_centre_cols = NULL, align_right_cols = NULL) {
  ft <- ft %>%
    font(fontname = TIMES, part = "all") %>%
    fontsize(size = PT, part = "all") %>%
    fontsize(size = PT_NOTE, part = "footer") %>%
    padding(padding.top = 3, padding.bottom = 3,
            padding.left = 5, padding.right = 5, part = "all") %>%
    align(align = "left", j = 1, part = "all") %>%
    border_remove() %>%
    hline_top(border = thick, part = "header") %>%
    hline_bottom(border = thin, part = "header") %>%
    hline_bottom(border = thick, part = "body")
  if (!is.null(align_right_cols))
    ft <- align(ft, j = align_right_cols, align = "right", part = "all")
  if (!is.null(align_centre_cols))
    ft <- align(ft, j = align_centre_cols, align = "center", part = "all")
  if (!is.null(col_widths))
    ft <- width(ft, j = seq_along(col_widths), width = col_widths)
  if (!is.null(note)) {
    ft <- ft %>%
      add_footer_lines(values = "") %>%
      compose(part = "footer", i = 1, j = 1,
              value = as_paragraph(
                as_chunk("Note. ", props = fp_text_note_it),
                as_chunk(note,     props = fp_text_note))) %>%
      merge_at(i = 1, j = seq_len(ncol_keys(ft)), part = "footer") %>%
      align(align = "left", part = "footer")
  }
  ft
}

# --- Regression flextable ---------------------------------------------------
make_regression_ft <- function(model) {
  betas <- std_betas(model)
  rows <- tidy(model) %>%
    mutate(
      Predictor = label_terms(term),
      B   = sprintf("%.3f", estimate),
      Bstar = paste0(B, format_p_stars(p.value)),
      Beta = ifelse(is.na(betas[term]), "—",
                    sub("^0", "", sub("^-0", "-", sprintf("%.3f", betas[term])))),
      SE  = sprintf("%.3f", std.error),
      t   = sprintf("%.2f", statistic),
      p   = ifelse(p.value < .001, "< .001",
                   sub("^0", "", sprintf("%.3f", p.value))),
      CI  = sprintf("[%.3f, %.3f]",
                    estimate - 1.96 * std.error,
                    estimate + 1.96 * std.error),
      is_sig = p.value < .05
    ) %>%
    select(Predictor, Bstar, Beta, SE, t, p, CI, is_sig)

  s <- summary(model)
  r2 <- sub("^0", "", sprintf("%.3f", s$r.squared))
  adj <- sub("^0", "", sprintf("%.3f", s$adj.r.squared))
  fst <- s$fstatistic
  note <- sprintf(
    "R² = %s, Adjusted R² = %s, F(%d, %d) = %.2f, N = %d. B = unstandardized coefficient; β = standardized coefficient; SE = standard error; 95%% CI = 95%% confidence interval. Bold rows are significant (p < .05). *p < .05. **p < .01. ***p < .001.",
    r2, adj, as.integer(fst[2]), as.integer(fst[3]), fst[1], nobs(model))

  display <- rows %>% select(Predictor, Bstar, Beta, SE, t, p, CI)
  ft <- flextable(display) %>%
    set_header_labels(
      Predictor = "Predictor", Bstar = "B", Beta = "β",
      SE = "SE", t = "t", p = "p", CI = "95% CI") %>%
    apa_ft(note = note,
           col_widths = c(2.4, 0.85, 0.7, 0.75, 0.65, 0.85, 2.20),
           align_right_cols = c("Bstar","Beta","SE","t","p"),
           align_centre_cols = "CI") %>%
    italic(j = c("Bstar","SE","t","p"), italic = TRUE, part = "header")

  sig_rows <- which(rows$is_sig)
  if (length(sig_rows)) ft <- bold(ft, i = sig_rows, part = "body")
  ft
}

# --- Descriptives flextable -------------------------------------------------
make_desc_ft <- function(data, vars, var_names, scale_items, ids) {
  if (!is.null(ids)) data <- data[data$M2ID %in% ids, , drop = FALSE]
  compute_alpha <- function(v) {
    its <- scale_items[[v]]
    if (is.null(its) || length(its) < 2) return(NA_real_)
    cols <- intersect(its, names(data))
    if (length(cols) < 2) return(NA_real_)
    df <- as.data.frame(data[, cols, drop = FALSE])
    df <- df[complete.cases(df), , drop = FALSE]
    if (nrow(df) < 2) return(NA_real_)
    res <- try(suppressWarnings(suppressMessages(
      psych::alpha(df, check.keys = TRUE, warnings = FALSE, use = "complete.obs")
    )), silent = TRUE)
    if (inherits(res, "try-error")) return(NA_real_)
    as.numeric(res$total$std.alpha)
  }
  tbl <- purrr::map2_dfr(vars, var_names, function(v, vn) {
    x <- data[[v]]
    if (is.factor(x)) x <- as.numeric(x) - 1
    x <- x[!is.na(x)]
    a <- compute_alpha(v)
    tibble(
      Variable = vn,
      n  = length(x),
      M  = sprintf("%.2f", mean(x)),
      SD = sprintf("%.2f", sd(x)),
      Min = sprintf("%.2f", min(x)),
      Max = sprintf("%.2f", max(x)),
      alpha = ifelse(is.na(a), "—",
                     sub("^0", "", sprintf("%.2f", a)))
    )
  })
  note <- "α = standardized Cronbach's alpha computed on the regression sample restricted to respondents with complete raw-item data on that scale. — = not applicable (single-item or demographic variable)."
  flextable(tbl) %>%
    set_header_labels(Variable="Variable", n="n", M="M", SD="SD",
                       Min="Min", Max="Max", alpha="α") %>%
    apa_ft(note = note,
           col_widths = c(2.4, 0.55, 0.6, 0.6, 0.6, 0.6, 0.55),
           align_right_cols = c("n","M","SD","Min","Max","alpha")) %>%
    italic(j = c("n","M","SD"), italic = TRUE, part = "header")
}

# --- Correlation matrix flextable -------------------------------------------
make_corr_ft <- function(data, vars, var_names) {
  for (v in vars) {
    if (is.factor(data[[v]])) data[[v]] <- as.numeric(data[[v]]) - 1
  }
  if (is.null(var_names)) var_names <- vars
  cor_mat <- cor(data[vars], use = "pairwise.complete.obs")
  n <- length(vars)
  cells <- matrix("", n, n)
  for (i in 1:n) for (j in 1:n) {
    if (i == j) cells[i,j] <- "—"
    else if (j < i) {
      r <- cor_mat[i, j]
      np <- sum(complete.cases(data[c(vars[i], vars[j])]))
      t_val <- r * sqrt(np - 2) / sqrt(1 - r^2)
      pv <- 2 * pt(abs(t_val), np - 2, lower.tail = FALSE)
      cells[i,j] <- paste0(sub("^0", "", sub("^-0", "-", sprintf("%.3f", r))),
                           format_p_stars(pv))
    }
  }
  tbl <- as.data.frame(cells, stringsAsFactors = FALSE)
  names(tbl) <- as.character(seq_len(n))
  tbl <- bind_cols(tibble(Variable = paste0(seq_len(n), ". ", var_names)), tbl)
  ft <- flextable(tbl) %>%
    apa_ft(note = "*p < .05. **p < .01. ***p < .001.",
           align_right_cols = as.character(seq_len(n)))
  ft <- width(ft, j = 1, width = 2.4)
  ft <- width(ft, j = 2:(n+1), width = pmax(0.45, 5 / n))
  ft
}

# --- Replicability flextable ------------------------------------------------
make_repl_ft <- function(paired_df) {
  n_sig_w1    <- sum(paired_df$sig_w1)
  n_same_sign <- sum(paired_df$replicated_sign, na.rm = TRUE)
  n_same_sig  <- sum(paired_df$replicated_sig,  na.rm = TRUE)
  n_both_sig  <- sum(paired_df$sig_w1 & paired_df$sig_w2)
  n_ci_over   <- sum(paired_df$ci_overlap, na.rm = TRUE)
  pct_sign <- if (n_sig_w1 > 0)  sprintf("%.1f", 100*n_same_sign/n_sig_w1) else "N/A"
  pct_sig  <- if (n_sig_w1 > 0)  sprintf("%.1f", 100*n_same_sig/n_sig_w1)  else "N/A"
  pct_ci   <- if (n_both_sig > 0) sprintf("%.1f", 100*n_ci_over/n_both_sig) else "N/A"

  detail <- paired_df %>%
    mutate(
      W1 = paste0(sprintf("%.3f", est_w1), " [",
                  sprintf("%.3f", ci_lo_w1), ", ",
                  sprintf("%.3f", ci_hi_w1), "]",
                  format_p_stars(p_w1)),
      W2 = paste0(sprintf("%.3f", est_w2), " [",
                  sprintf("%.3f", ci_lo_w2), ", ",
                  sprintf("%.3f", ci_hi_w2), "]",
                  format_p_stars(p_w2)),
      Sign = case_when(!sig_w1 ~ "—",
                       same_sign ~ "✓", TRUE ~ "✗"),
      Sig  = case_when(!sig_w1 ~ "—",
                       replicated_sig ~ "✓", TRUE ~ "✗"),
      CI   = case_when(!(sig_w1 & sig_w2) ~ "—",
                       ci_overlap ~ "✓", TRUE ~ "✗")
    ) %>%
    select(pair_id, term_label, W1, W2, Sign, Sig, CI)

  summary_rows <- tibble(
    pair_id = c(
      paste0("Step 1: Same sign (of ", n_sig_w1, " sig in W1)"),
      paste0("Step 2: Same sign + significant (of ", n_sig_w1, " sig in W1)"),
      paste0("Step 3: Overlapping CIs (of ", n_both_sig, " sig in both)")
    ),
    term_label = "", W1 = "", W2 = "",
    Sign = c(paste0(n_same_sign,"/",n_sig_w1," (",pct_sign,"%)"),"",""),
    Sig  = c("", paste0(n_same_sig,"/",n_sig_w1," (",pct_sig,"%)"),""),
    CI   = c("","",paste0(n_ci_over,"/",n_both_sig," (",pct_ci,"%)"))
  )
  final <- bind_rows(detail, summary_rows)

  n_detail <- nrow(detail); n_total <- nrow(final)
  note <- "Replicability assessed using the Open Science Collaboration (2015) criteria. Step 1: percentage of W1-significant effects with the same sign in W2. Step 2: percentage of W1-significant effects also significant and same sign in W2. Step 3: percentage of effects significant in both waves with overlapping 95% CIs. ✓ = criterion met. ✗ = criterion not met. — = not applicable. *p < .05. **p < .01. ***p < .001."

  ft <- flextable(final) %>%
    set_header_labels(pair_id="Model", term_label="Term",
                       W1="W1 B [95% CI]", W2="W2 B [95% CI]",
                       Sign="Sign", Sig="Sig", CI="CI") %>%
    apa_ft(note = note,
           col_widths = c(1.6, 1.5, 1.4, 1.4, 0.4, 0.4, 0.4),
           align_centre_cols = c("W1","W2","Sign","Sig","CI"))

  if (n_detail < n_total) {
    ft <- italic(ft, i = (n_detail+1):n_total, italic = TRUE, part = "body")
    ft <- bold(ft,   i = (n_detail+1):n_total, part = "body")
  }
  ft <- hline(ft, i = n_detail, border = thin, part = "body")
  ft
}

cat("[3/4] Building 45 flextables ...\n")

desc_m2 <- descriptives_data$m2
desc_m3 <- descriptives_data$m3

ft_list <- list()

ft_list[[1]] <- make_desc_ft(desc_m2$data, desc_m2$vars, desc_m2$labels,
                              desc_m2$scale_items, desc_m2$ids)
ft_list[[2]] <- make_desc_ft(desc_m3$data, desc_m3$vars, desc_m3$labels,
                              desc_m3$scale_items, desc_m3$ids)
ft_list[[3]] <- make_corr_ft(desc_m2$data, desc_m2$vars, desc_m2$labels)
ft_list[[4]] <- make_corr_ft(desc_m3$data, desc_m3$vars, desc_m3$labels)

reg_models <- list(
  m1_negwf_m2, m1_negwf_m3, m2_negfw_m2, m2_negfw_m3,
  m3_poswf_m2, m3_poswf_m3, m4_posfw_m2, m4_posfw_m3,
  m6_gender_negwf_m2, m6_gender_negwf_m3, m5_gender_negfw_m2, m5_gender_negfw_m3,
  m8_gender_poswf_m2, m8_gender_poswf_m3, m7_gender_posfw_m2, m7_gender_posfw_m3,
  m10_super_negwf_m2, m10_super_negwf_m3, m9_super_negfw_m2, m9_super_negfw_m3,
  m12_super_poswf_m2, m12_super_poswf_m3, m11_super_posfw_m2, m11_super_posfw_m3,
  m14_cowork_negwf_m2, m14_cowork_negwf_m3, m13_cowork_negfw_m2, m13_cowork_negfw_m3,
  m16_cowork_poswf_m2, m16_cowork_poswf_m3, m15_cowork_posfw_m2, m15_cowork_posfw_m3,
  m18_spouse_negwf_m2, m18_spouse_negwf_m3, m17_spouse_negfw_m2, m17_spouse_negfw_m3,
  m20_spouse_poswf_m2, m20_spouse_poswf_m3, m19_spouse_posfw_m2, m19_spouse_posfw_m3
)
for (i in seq_along(reg_models)) {
  ft_list[[4 + i]] <- make_regression_ft(reg_models[[i]])
}

ft_list[[45]] <- make_repl_ft(reg_replicability)
stopifnot(length(ft_list) == 45)

table_titles <- c(
  "Descriptive Statistics (MIDUS 2)",
  "Descriptive Statistics (MIDUS 3)",
  "Bivariate Correlations (MIDUS 2)",
  "Bivariate Correlations (MIDUS 3)",
  "Main Effects: Negative Work-to-Family Spillover (MIDUS 2)",
  "Main Effects: Negative Work-to-Family Spillover (MIDUS 3)",
  "Main Effects: Negative Family-to-Work Spillover (MIDUS 2)",
  "Main Effects: Negative Family-to-Work Spillover (MIDUS 3)",
  "Main Effects: Positive Work-to-Family Spillover (MIDUS 2)",
  "Main Effects: Positive Work-to-Family Spillover (MIDUS 3)",
  "Main Effects: Positive Family-to-Work Spillover (MIDUS 2)",
  "Main Effects: Positive Family-to-Work Spillover (MIDUS 3)",
  "Gender × EF: Negative Work-to-Family Spillover (MIDUS 2)",
  "Gender × EF: Negative Work-to-Family Spillover (MIDUS 3)",
  "Gender × EF: Negative Family-to-Work Spillover (MIDUS 2)",
  "Gender × EF: Negative Family-to-Work Spillover (MIDUS 3)",
  "Gender × EF: Positive Work-to-Family Spillover (MIDUS 2)",
  "Gender × EF: Positive Work-to-Family Spillover (MIDUS 3)",
  "Gender × EF: Positive Family-to-Work Spillover (MIDUS 2)",
  "Gender × EF: Positive Family-to-Work Spillover (MIDUS 3)",
  "Supervisor Support × EF: Negative Work-to-Family Spillover (MIDUS 2)",
  "Supervisor Support × EF: Negative Work-to-Family Spillover (MIDUS 3)",
  "Supervisor Support × EF: Negative Family-to-Work Spillover (MIDUS 2)",
  "Supervisor Support × EF: Negative Family-to-Work Spillover (MIDUS 3)",
  "Supervisor Support × EF: Positive Work-to-Family Spillover (MIDUS 2)",
  "Supervisor Support × EF: Positive Work-to-Family Spillover (MIDUS 3)",
  "Supervisor Support × EF: Positive Family-to-Work Spillover (MIDUS 2)",
  "Supervisor Support × EF: Positive Family-to-Work Spillover (MIDUS 3)",
  "Coworker Support × EF: Negative Work-to-Family Spillover (MIDUS 2)",
  "Coworker Support × EF: Negative Work-to-Family Spillover (MIDUS 3)",
  "Coworker Support × EF: Negative Family-to-Work Spillover (MIDUS 2)",
  "Coworker Support × EF: Negative Family-to-Work Spillover (MIDUS 3)",
  "Coworker Support × EF: Positive Work-to-Family Spillover (MIDUS 2)",
  "Coworker Support × EF: Positive Work-to-Family Spillover (MIDUS 3)",
  "Coworker Support × EF: Positive Family-to-Work Spillover (MIDUS 2)",
  "Coworker Support × EF: Positive Family-to-Work Spillover (MIDUS 3)",
  "Spousal Support × EF: Negative Work-to-Family Spillover (MIDUS 2)",
  "Spousal Support × EF: Negative Work-to-Family Spillover (MIDUS 3)",
  "Spousal Support × EF: Negative Family-to-Work Spillover (MIDUS 2)",
  "Spousal Support × EF: Negative Family-to-Work Spillover (MIDUS 3)",
  "Spousal Support × EF: Positive Work-to-Family Spillover (MIDUS 2)",
  "Spousal Support × EF: Positive Work-to-Family Spillover (MIDUS 3)",
  "Spousal Support × EF: Positive Family-to-Work Spillover (MIDUS 2)",
  "Spousal Support × EF: Positive Family-to-Work Spillover (MIDUS 3)",
  "Cross-Wave Replicability of Primary Regressions (OSC, 2015)"
)
stopifnot(length(table_titles) == 45)

cat("[4/4] Composing docx ...\n")

# APA 7 paragraph formats: double-spacing throughout, 0.5" first-line indent
# on body text only. Headings use same font/size as body (TNR 12pt); Level 1
# is centered + bold, Level 2 is flush-left + bold (APA 7 §2.27).
pp_body      <- fp_par(text.align = "left", line_spacing = 2.0,
                       padding.top = 0, padding.bottom = 0)
# Note: officer's fp_par doesn't expose first-line indent. A 0.5" first-line
# indent (APA 7 §2.24) is injected post-write by the Python helper below.
pp_table_num <- fp_par(text.align = "left", line_spacing = 2.0,
                       padding.top = 12, padding.bottom = 0)
pp_table_ttl <- fp_par(text.align = "left", line_spacing = 2.0,
                       padding.top = 0, padding.bottom = 6)
pp_h1 <- fp_par(text.align = "center", line_spacing = 2.0,
                padding.top = 12, padding.bottom = 0)
pp_h2 <- fp_par(text.align = "left", line_spacing = 2.0,
                padding.top = 12, padding.bottom = 0)
pp_h3 <- fp_par(text.align = "left", line_spacing = 2.0,
                padding.top = 8,  padding.bottom = 0)

fp_text_h1 <- fp_text(font.family = TIMES, font.size = PT, bold = TRUE)
fp_text_h2 <- fp_text(font.family = TIMES, font.size = PT, bold = TRUE)

add_h1 <- function(doc, text) body_add_fpar(doc, fpar(ftext(text, fp_text_h1), fp_p = pp_h1))
add_h2 <- function(doc, text) body_add_fpar(doc, fpar(ftext(text, fp_text_h2), fp_p = pp_h2))
add_p  <- function(doc, ...)  body_add_fpar(doc, fpar(..., fp_p = pp_body))
tx <- function(s) ftext(s, fp_text_body)
ti <- function(s) ftext(s, fp_text_it)
N  <- tx; I <- ti

doc <- read_docx()

# APA 7 page numbers: top-right of every page (header).
# officer's `header_default` lets us define a header reference shared across sections.
hdr_pgnum_par <- fpar(
  run_word_field(field = "PAGE", prop = fp_text_body),
  fp_p = fp_par(text.align = "right", line_spacing = 1.0,
                padding.top = 0, padding.bottom = 0)
)

section_portrait <- prop_section(
  page_size    = page_size(orient = "portrait", width = 8.5, height = 11),
  page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1,
                          header = 0.5, footer = 0.5),
  type         = "nextPage",
  header_default = block_list(hdr_pgnum_par)
)
section_landscape <- prop_section(
  page_size    = page_size(orient = "landscape", width = 8.5, height = 11),
  page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5,
                          header = 0.25, footer = 0.25),
  header_default = block_list(hdr_pgnum_par)
)

# --- Narrative (outcome-by-outcome, flowing prose, M2 then M3 in same para) ---
doc <- add_h1(doc, "Results")

doc <- add_h2(doc, "Descriptive Statistics and Bivariate Correlations")
doc <- add_p(doc,
  N("Descriptive statistics and bivariate correlations for all study variables are presented in Tables 1 through 4. Tables 1 and 2 report means, standard deviations, and standardized Cronbach's alpha for each multi-item scale, computed on the regression-analytic sample. Tables 3 and 4 report bivariate correlations among all study variables for MIDUS 2 ("),
  I("N"), N(" = 1,289) and MIDUS 3 ("), I("N"), N(" = 729), respectively. Across both waves, job demands and family demands showed the strongest correlations with the negative spillover outcomes, and the social support variables (supervisor, coworker, spousal) showed the strongest positive associations with positive spillover."))

doc <- add_h2(doc, "Analytic Approach")
doc <- add_p(doc,
  N("A series of multiple linear regression models were estimated to test all hypotheses and research questions. MIDUS 2 ("),
  I("N"), N(" = 1,289) and MIDUS 3 ("), I("N"), N(" = 729) were analyzed separately to assess replicability. All models controlled for job demands, family demands, household income, household size, age, gender (where not serving as a moderator), and education. Gender was coded 0 = male and 1 = female. Continuous predictors involved in interactions — executive functioning, supervisor support, coworker support, spousal support, job demands, and family demands — were mean-centered prior to forming interaction terms. Both unstandardized ("),
  I("B"), N(") and standardized (β) coefficients are reported along with 95% confidence intervals and exact "),
  I("p"), N("-values."))

# --- Main Effects ---
doc <- add_h2(doc, "Main Effects of Executive Functioning (Hypotheses 1–4)")
doc <- add_p(doc, N("Hypotheses 1–4 predicted that executive functioning would be negatively related to negative spillover (H1: work-to-family; H2: family-to-work) and positively related to positive spillover (H3: work-to-family; H4: family-to-work). Results are presented in Tables 5 through 12, organized by outcome with MIDUS 2 reported first followed by MIDUS 3."))
for (n in 5:12) doc <- body_add_fpar(doc, fpar()) # placeholder; tables go at end

# --- Each moderation block: terse paragraph + later table refs ---
doc <- add_h2(doc, "Gender × Executive Functioning Moderation (Hypotheses 5–6, Research Questions 1–2)")
doc <- add_p(doc, N("Hypotheses 5–6 predicted that gender would moderate the relationship between executive functioning and negative spillover. Research questions 1–2 explored whether gender moderated the relationship between executive functioning and positive spillover. Results are presented in Tables 13 through 20."))

doc <- add_h2(doc, "Supervisor Support × Executive Functioning Moderation (Hypotheses 7–8, Research Questions 3–4)")
doc <- add_p(doc, N("Hypotheses 7–8 predicted that supervisor support would buffer the relationship between executive functioning and negative spillover. Research questions 3–4 explored supervisor support moderation for positive spillover. Results are presented in Tables 21 through 28."))

doc <- add_h2(doc, "Coworker Support × Executive Functioning Moderation (Hypotheses 9–10, Research Questions 5–6)")
doc <- add_p(doc, N("Hypotheses 9–10 predicted that coworker support would buffer the relationship between executive functioning and negative spillover. Research questions 5–6 explored coworker support moderation for positive spillover. Results are presented in Tables 29 through 36."))

doc <- add_h2(doc, "Spousal Support × Executive Functioning Moderation (Hypotheses 11–12, Research Questions 7–8)")
doc <- add_p(doc, N("Hypotheses 11–12 predicted that spousal support would buffer the relationship between executive functioning and negative spillover. Research questions 7–8 explored spousal support moderation for positive spillover. Results are presented in Tables 37 through 44."))

doc <- add_h2(doc, "Cross-Wave Replicability (OSC, 2015)")
doc <- add_p(doc, N("To formally assess replicability, the Open Science Collaboration (2015) criteria were applied to evaluate whether effects significant in MIDUS 2 replicated in MIDUS 3 (Table 45)."))

# =============================================================================
# APPENDIX FLEXTABLE BUILDERS — multiverse, CFA, power
# =============================================================================

# --- Multiverse long (EF coefficients across specs, one row per term × spec) ---
make_mv_long_ft <- function(results_df, wave_label) {
  out_acr <- c(NEGWF = "NWF", NEGFW = "NFW", POSWF = "PWF", POSFW = "PFW")
  tbl <- results_df %>%
    mutate(
      Outcome = ifelse(outcome %in% names(out_acr), unname(out_acr[outcome]), outcome),
      Term = gsub("Executive Functioning", "EF", label_terms(term), fixed = TRUE),
      B   = paste0(sprintf("%.3f", estimate), format_p_stars(p.value)),
      Beta = ifelse(is.na(beta), "—",
                    sub("^0", "", sub("^-0", "-", sprintf("%.3f", beta)))),
      SE  = sprintf("%.3f", std.error),
      p   = ifelse(p.value < .001, "< .001",
                   sub("^0", "", sprintf("%.3f", p.value))),
      CI  = sprintf("[%.3f, %.3f]",
                    estimate - 1.96 * std.error,
                    estimate + 1.96 * std.error),
      is_sig = p.value < .05
    ) %>%
    select(Specification = specification, Outcome, Term, B, Beta, SE, p, CI, is_sig)

  display <- tbl %>% select(-is_sig)
  note <- "EF = executive functioning. NWF/NFW/PWF/PFW = negative/positive work-to-family / family-to-work spillover. β = standardized coefficient. Bold = significant (p < .05). *p < .05. **p < .01. ***p < .001."
  ft <- flextable(display) %>%
    set_header_labels(B = "B", Beta = "β", SE = "SE", p = "p", CI = "95% CI") %>%
    apa_ft(note = note,
           col_widths = c(2.4, 0.55, 1.4, 0.85, 0.55, 0.65, 0.65, 1.4),
           align_right_cols = c("B","Beta","SE","p"),
           align_centre_cols = "CI") %>%
    italic(j = c("B","SE","p"), italic = TRUE, part = "header")
  sig_rows <- which(tbl$is_sig)
  if (length(sig_rows)) ft <- bold(ft, i = sig_rows, part = "body")
  ft
}

# --- Multiverse wide (EF main effect only, columns per outcome) ---
make_mv_wide_ft <- function(results_df) {
  out_acr <- c(NEGWF = "NWF", NEGFW = "NFW", POSWF = "PWF", POSFW = "PFW")
  main_ef <- results_df %>%
    filter(!str_detect(term, ":")) %>%
    mutate(Outcome = ifelse(outcome %in% names(out_acr),
                            unname(out_acr[outcome]), outcome),
           B = paste0(sprintf("%.3f", estimate), format_p_stars(p.value)))
  wide <- main_ef %>%
    select(specification, Outcome, B) %>%
    pivot_wider(names_from = Outcome, values_from = B)
  note <- "Cell entries are the EF main-effect B with significance stars. NWF/NFW/PWF/PFW = negative/positive work-to-family / family-to-work spillover. *p < .05. **p < .01. ***p < .001."
  flextable(wide) %>%
    set_header_labels(specification = "Specification") %>%
    apa_ft(note = note,
           col_widths = c(2.8, 1.4, 1.4, 1.4, 1.4),
           align_centre_cols = c("NWF","NFW","PWF","PFW"))
}

# --- CFA Fit indices ---
make_cfa_fit_ft <- function(fits, fit_names) {
  indices <- c("chisq", "df", "pvalue", "cfi", "tli",
               "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
  tbl <- map2_dfr(fits, fit_names, function(fit, name) {
    fm <- lavaan::fitMeasures(fit, indices)
    tibble(
      Model = name,
      `χ²` = sprintf("%.2f", fm["chisq"]),
      df  = as.integer(fm["df"]),
      p   = ifelse(fm["pvalue"] < .001, "< .001",
                   sub("^0", "", sprintf("%.3f", fm["pvalue"]))),
      CFI = sprintf("%.3f", fm["cfi"]),
      TLI = sprintf("%.3f", fm["tli"]),
      RMSEA = sprintf("%.3f", fm["rmsea"]),
      `90% CI` = sprintf("[%.3f, %.3f]",
                         fm["rmsea.ci.lower"], fm["rmsea.ci.upper"]),
      SRMR = sprintf("%.3f", fm["srmr"])
    )
  })
  note <- "Good fit: CFI > .95, TLI > .95, RMSEA < .06, SRMR < .08."
  flextable(tbl) %>%
    apa_ft(note = note,
           col_widths = c(1.6, 0.85, 0.55, 0.7, 0.65, 0.65, 0.7, 1.3, 0.65),
           align_right_cols = c("χ²","df","p","CFI","TLI","RMSEA","SRMR"),
           align_centre_cols = "90% CI") %>%
    italic(j = c("df","p"), italic = TRUE, part = "header")
}

# --- CFA Loadings ---
make_cfa_loadings_ft <- function(fit) {
  fac_lbl <- c("neg_wtf" = "Neg W→F", "pos_wtf" = "Pos W→F",
               "neg_ftw" = "Neg F→W", "pos_ftw" = "Pos F→W")
  std <- lavaan::standardizedSolution(fit)
  load <- std %>%
    filter(op == "=~") %>%
    mutate(Factor = ifelse(lhs %in% names(fac_lbl), unname(fac_lbl[lhs]), lhs),
           Item = rhs,
           λ  = sprintf("%.3f", est.std),
           SE = sprintf("%.3f", se),
           p  = ifelse(pvalue < .001, "< .001",
                      sub("^0", "", sprintf("%.3f", pvalue)))) %>%
    select(Factor, Item, λ, SE, p)
  flextable(load) %>%
    apa_ft(note = "All loadings standardized.",
           col_widths = c(1.5, 1.5, 0.85, 0.85, 0.85),
           align_right_cols = c("λ","SE","p")) %>%
    merge_v(j = "Factor", part = "body")
}

# --- CFA Factor correlations ---
make_cfa_cors_ft <- function(fit) {
  facs <- c("neg_wtf","pos_wtf","neg_ftw","pos_ftw")
  fac_lbl <- c(neg_wtf="Neg W→F", pos_wtf="Pos W→F",
               neg_ftw="Neg F→W", pos_ftw="Pos F→W")
  std <- lavaan::standardizedSolution(fit)
  cors <- std %>%
    filter(op == "~~", lhs != rhs, lhs %in% facs, rhs %in% facs) %>%
    mutate(`Factor 1` = unname(fac_lbl[lhs]),
           `Factor 2` = unname(fac_lbl[rhs]),
           r = paste0(sprintf("%.3f", est.std), format_p_stars(pvalue))) %>%
    select(`Factor 1`, `Factor 2`, r)
  flextable(cors) %>%
    apa_ft(note = "*p < .05. **p < .01. ***p < .001.",
           col_widths = c(2, 2, 1.2),
           align_right_cols = "r") %>%
    italic(j = "r", italic = TRUE, part = "header")
}

# --- Measurement invariance ---
make_invariance_ft <- function(fits) {
  level_names <- names(fits)
  tbl <- map_dfr(seq_along(fits), function(i) {
    fm <- lavaan::fitMeasures(fits[[i]], c("chisq", "df", "cfi", "rmsea", "srmr"))
    tibble(Level = level_names[i],
           `χ²` = sprintf("%.2f", fm["chisq"]),
           df = as.integer(fm["df"]),
           CFI = sprintf("%.3f", fm["cfi"]),
           RMSEA = sprintf("%.3f", fm["rmsea"]),
           SRMR = sprintf("%.3f", fm["srmr"]))
  })
  cfi_vals <- sapply(fits, function(x) lavaan::fitMeasures(x, "cfi"))
  d_cfi <- c(NA, diff(cfi_vals))
  tbl$ΔCFI <- ifelse(is.na(d_cfi), "—", sprintf("%.4f", d_cfi))
  rmsea_vals <- sapply(fits, function(x) lavaan::fitMeasures(x, "rmsea"))
  d_rmsea <- c(NA, diff(rmsea_vals))
  tbl$ΔRMSEA <- ifelse(is.na(d_rmsea), "—", sprintf("%.4f", d_rmsea))
  tbl$Decision <- ifelse(is.na(d_cfi), "—",
                         ifelse(abs(d_cfi) < .01, "Supported", "Not supported"))
  note <- "|ΔCFI| < .01 supports invariance (Chen, 2007). Configural = same structure; Metric = equal loadings; Scalar = equal intercepts; Strict = equal residuals."
  flextable(tbl) %>%
    apa_ft(note = note,
           col_widths = c(1.4, 0.85, 0.5, 0.7, 0.75, 0.7, 0.8, 0.9, 1.4),
           align_right_cols = c("χ²","df","CFI","RMSEA","SRMR","ΔCFI","ΔRMSEA")) %>%
    italic(j = "df", italic = TRUE, part = "header")
}

# --- Power MDE ---
make_power_mde_ft <- function(samples) {
  library(WebPower)
  models <- list(
    "Main effect (9 predictors)" = list(p1 = 8, p2 = 1),
    "2-way interaction (11 predictors)" = list(p1 = 10, p2 = 1),
    "3-way interaction (16 predictors)" = list(p1 = 15, p2 = 1)
  )
  tbl <- map_dfr(names(samples), function(sn) {
    n_obs <- samples[[sn]]
    map_dfr(names(models), function(mn) {
      m <- models[[mn]]
      r80 <- WebPower::wp.regression(n = n_obs, p1 = m$p1, p2 = m$p2,
                                      alpha = 0.05, power = 0.80)
      r90 <- WebPower::wp.regression(n = n_obs, p1 = m$p1, p2 = m$p2,
                                      alpha = 0.05, power = 0.90)
      tibble(
        Sample = paste0(sn, " (n = ", n_obs, ")"),
        Model  = mn,
        `f² (80%)`  = sprintf("%.4f", r80$f2),
        `ΔR² (80%)` = sprintf("%.4f", r80$f2 / (1 + r80$f2)),
        `f² (90%)`  = sprintf("%.4f", r90$f2),
        `ΔR² (90%)` = sprintf("%.4f", r90$f2 / (1 + r90$f2))
      )
    })
  })
  note <- "Small f² = .02, medium = .15, large = .35 (Cohen, 1988). α = .05."
  flextable(tbl) %>%
    apa_ft(note = note,
           col_widths = c(1.7, 2.3, 1.1, 1.1, 1.1, 1.1),
           align_right_cols = 3:6) %>%
    merge_v(j = "Sample", part = "body")
}

# --- Power Fixed Effects ---
make_power_fixed_ft <- function(samples,
                                 f2_values = c(0.01, 0.02, 0.05, 0.10, 0.15)) {
  library(WebPower)
  models <- list("Main effect" = 8, "2-way" = 10, "3-way" = 15)
  tbl <- map_dfr(names(samples), function(sn) {
    n_obs <- samples[[sn]]
    map_dfr(f2_values, function(f2) {
      row <- tibble(
        Sample = paste0(sn, " (n = ", n_obs, ")"),
        `f²` = sprintf("%.2f", f2),
        `ΔR²` = sprintf("%.3f", f2 / (1 + f2)))
      for (mn in names(models)) {
        res <- WebPower::wp.regression(n = n_obs, p1 = models[[mn]], p2 = 1,
                                        f2 = f2, alpha = 0.05, power = NULL)
        row[[mn]] <- paste0(sprintf("%.1f", res$power * 100), "%")
      }
      row
    })
  })
  note <- "Power for testing one coefficient. α = .05. Adequate power ≥ 80%."
  flextable(tbl) %>%
    apa_ft(note = note,
           col_widths = c(1.7, 0.7, 0.85, 1.1, 1.1, 1.1),
           align_right_cols = 2:6) %>%
    merge_v(j = "Sample", part = "body")
}

# --- Power for Standardized Beta ---
make_power_beta_ft <- function(n_obs, n_predictors,
                                beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20),
                                r_squared_x = 0.3, alpha = 0.05) {
  df_resid <- n_obs - n_predictors - 1
  t_crit <- qt(1 - alpha/2, df_resid)
  tbl <- tibble(`β` = sprintf("%.2f", beta_values))
  ses <- numeric(length(beta_values))
  pwrs <- character(length(beta_values))
  for (i in seq_along(beta_values)) {
    beta <- beta_values[i]
    se_beta <- 1 / sqrt((1 - r_squared_x) * df_resid)
    ncp <- beta / se_beta
    pwr <- 1 - pt(t_crit, df_resid, ncp) + pt(-t_crit, df_resid, ncp)
    ses[i]  <- se_beta
    pwrs[i] <- paste0(sprintf("%.1f", pwr * 100), "%")
  }
  tbl$`SE(β)` <- sprintf("%.4f", ses)
  tbl$Power   <- pwrs
  note <- sprintf("R²_x = %.2f (focal × covariate collinearity). α = %.2f, two-tailed.",
                  r_squared_x, alpha)
  flextable(tbl) %>%
    apa_ft(note = note,
           col_widths = c(1.1, 1.3, 1.3),
           align_right_cols = 1:3)
}

cat("Building appendix flextables ...\n")

# --- Multiverse ---
mv_long_m2_ft <- make_mv_long_ft(results_m2, "MIDUS 2")
mv_long_m3_ft <- make_mv_long_ft(results_m3, "MIDUS 3")
mv_wide_m2_ft <- make_mv_wide_ft(results_m2)
mv_wide_m3_ft <- make_mv_wide_ft(results_m3)

# --- CFA ---
cfa_fit_ft     <- make_cfa_fit_ft(list(fit_m2, fit_m3), c("MIDUS 2", "MIDUS 3"))
cfa_load_m2_ft <- make_cfa_loadings_ft(fit_m2)
cfa_load_m3_ft <- make_cfa_loadings_ft(fit_m3)
cfa_cor_m2_ft  <- make_cfa_cors_ft(fit_m2)
cfa_cor_m3_ft  <- make_cfa_cors_ft(fit_m3)

inv_m2_ft <- make_invariance_ft(list(
  Configural = inv_m2$configural, Metric = inv_m2$metric,
  Scalar     = inv_m2$scalar,     Strict = inv_m2$strict))
inv_m3_ft <- make_invariance_ft(list(
  Configural = inv_m3$configural, Metric = inv_m3$metric,
  Scalar     = inv_m3$scalar,     Strict = inv_m3$strict))

# --- Power ---
samples_local <- list("MIDUS 2" = nobs(m1_negwf_m2),
                       "MIDUS 3" = nobs(m1_negwf_m3))
power_mde_ft   <- make_power_mde_ft(samples_local)
power_fixed_ft <- make_power_fixed_ft(samples_local)
power_b_m2_8  <- make_power_beta_ft(samples_local[["MIDUS 2"]], 8)
power_b_m2_10 <- make_power_beta_ft(samples_local[["MIDUS 2"]], 10)
power_b_m2_15 <- make_power_beta_ft(samples_local[["MIDUS 2"]], 15)
power_b_m3_8  <- make_power_beta_ft(samples_local[["MIDUS 3"]], 8)
power_b_m3_10 <- make_power_beta_ft(samples_local[["MIDUS 3"]], 10)
power_b_m3_15 <- make_power_beta_ft(samples_local[["MIDUS 3"]], 15)

# --- Significant interaction figures (paths relative to working dir) ---
fig_dir <- file.path("..", "output", "figures", "interactions", "significant")
sig_figs <- list(
  list(file = "m14_cowork_negwf_m2_combined.png",
       title = "Coworker Support × Executive Functioning Interaction on Negative Work-to-Family Spillover (MIDUS 2)",
       width = 9, height = 3.4),
  list(file = "m12_super_poswf_m3_combined.png",
       title = "Supervisor Support × Executive Functioning Interaction on Positive Work-to-Family Spillover (MIDUS 3)",
       width = 9, height = 3.4),
  list(file = "m5_gender_negfw_m3_simpleslopes.png",
       title = "Gender × Executive Functioning Interaction on Negative Family-to-Work Spillover (MIDUS 3)",
       width = 6, height = 4.5)
)
sig_figs <- Filter(function(x) file.exists(file.path(fig_dir, x$file)), sig_figs)

# =============================================================================
# COMPOSE DOCUMENT
# =============================================================================

# --- Close the narrative as a portrait section, then switch to landscape ---
doc <- body_end_block_section(doc, value = block_section(section_portrait))
doc <- add_h1(doc, "Tables")
for (n in seq_along(ft_list)) {
  doc <- body_add_break(doc)
  doc <- body_add_fpar(doc, fpar(
    ftext(paste0("Table ", n), fp_text_bold), fp_p = pp_table_num))
  doc <- body_add_fpar(doc, fpar(
    ftext(table_titles[n], fp_text_it), fp_p = pp_table_ttl))
  doc <- body_add_flextable(doc, ft_list[[n]], align = "left")
}

# --- Significant Interaction Figures (main results) ---
if (length(sig_figs) > 0) {
  doc <- body_add_break(doc)
  doc <- add_h1(doc, "Significant Interaction Figures")
  for (i in seq_along(sig_figs)) {
    sf <- sig_figs[[i]]
    doc <- body_add_break(doc)
    doc <- body_add_fpar(doc, fpar(
      ftext(paste0("Figure ", i), fp_text_bold), fp_p = pp_table_num))
    doc <- body_add_fpar(doc, fpar(
      ftext(sf$title, fp_text_it), fp_p = pp_table_ttl))
    doc <- body_add_img(doc, src = file.path(fig_dir, sf$file),
                        width = sf$width, height = sf$height)
  }
}

# =============================================================================
# APPENDIX
# =============================================================================
doc <- body_add_break(doc)
doc <- add_h1(doc, "Appendix")

# --- Appendix A: Multiverse ---
appx_tables <- list(
  list(label = "A1", title = "Multiverse Analysis: EF Effects Across Specifications (MIDUS 2)", ft = mv_long_m2_ft),
  list(label = "A2", title = "Multiverse Analysis: EF Effects Across Specifications (MIDUS 3)", ft = mv_long_m3_ft),
  list(label = "A3", title = "Multiverse Summary: EF Main Effects Only (MIDUS 2)", ft = mv_wide_m2_ft),
  list(label = "A4", title = "Multiverse Summary: EF Main Effects Only (MIDUS 3)", ft = mv_wide_m3_ft),
  list(label = "B1", title = "Confirmatory Factor Analysis Model Fit Indices", ft = cfa_fit_ft),
  list(label = "B2", title = "Standardized Factor Loadings (MIDUS 2)", ft = cfa_load_m2_ft),
  list(label = "B3", title = "Standardized Factor Loadings (MIDUS 3)", ft = cfa_load_m3_ft),
  list(label = "B4", title = "Latent Factor Correlations (MIDUS 2)", ft = cfa_cor_m2_ft),
  list(label = "B5", title = "Latent Factor Correlations (MIDUS 3)", ft = cfa_cor_m3_ft),
  list(label = "B6", title = "Measurement Invariance Across Gender (MIDUS 2)", ft = inv_m2_ft),
  list(label = "B7", title = "Measurement Invariance Across Gender (MIDUS 3)", ft = inv_m3_ft),
  list(label = "C1", title = "Minimum Detectable Effect Sizes (α = .05)", ft = power_mde_ft),
  list(label = "C2", title = "Statistical Power at Fixed Effect Sizes (α = .05)", ft = power_fixed_ft),
  list(label = "C3", title = "Power for Standardized β: MIDUS 2 — Main Effects (k = 8)", ft = power_b_m2_8),
  list(label = "C4", title = "Power for Standardized β: MIDUS 2 — 2-Way Interactions (k = 10)", ft = power_b_m2_10),
  list(label = "C5", title = "Power for Standardized β: MIDUS 2 — 3-Way Interactions (k = 15)", ft = power_b_m2_15),
  list(label = "C6", title = "Power for Standardized β: MIDUS 3 — Main Effects (k = 8)", ft = power_b_m3_8),
  list(label = "C7", title = "Power for Standardized β: MIDUS 3 — 2-Way Interactions (k = 10)", ft = power_b_m3_10),
  list(label = "C8", title = "Power for Standardized β: MIDUS 3 — 3-Way Interactions (k = 15)", ft = power_b_m3_15)
)

# Section sub-headings interleaved
prev_section_letter <- ""
for (a in appx_tables) {
  section_letter <- substr(a$label, 1, 1)
  if (section_letter != prev_section_letter) {
    section_title <- switch(section_letter,
      "A" = "Appendix A. Multiverse Analyses",
      "B" = "Appendix B. Confirmatory Factor Analysis and Measurement Invariance",
      "C" = "Appendix C. Power Analysis"
    )
    doc <- body_add_break(doc)
    doc <- add_h1(doc, section_title)
    prev_section_letter <- section_letter
  }
  doc <- body_add_break(doc)
  doc <- body_add_fpar(doc, fpar(
    ftext(paste0("Table ", a$label), fp_text_bold), fp_p = pp_table_num))
  doc <- body_add_fpar(doc, fpar(
    ftext(a$title, fp_text_it), fp_p = pp_table_ttl))
  doc <- body_add_flextable(doc, a$ft, align = "left")
}

doc <- body_set_default_section(doc, section_landscape)

out_path <- file.path(out_dir, "results.docx")
print(doc, target = out_path)

# --- APA 7 post-process: inject 0.5" first-line indent on body paragraphs ---
# officer's fp_par lacks first-line-indent support, so we patch document.xml
# after writing. Body paragraphs are detected as: not in a table, has double-
# line spacing (w:line="480"), text is non-empty, alignment is not center.
apa_indent_py <- '
import sys, zipfile, shutil, re, os
from lxml import etree
src = sys.argv[1]
ns = {"w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main"}
W = "{%s}" % ns["w"]
tmp = src + ".tmp"
shutil.copy(src, tmp)
with zipfile.ZipFile(tmp, "r") as zin:
    members = {n: zin.read(n) for n in zin.namelist()}
doc = etree.fromstring(members["word/document.xml"])
body = doc.find(W + "body")
def in_table(p):
    e = p.getparent()
    while e is not None:
        if e.tag == W + "tbl":
            return True
        e = e.getparent()
    return False
n_patched = 0
for p in body.iter(W + "p"):
    if in_table(p):
        continue
    text = "".join(t.text or "" for t in p.iter(W + "t"))
    if not text.strip():
        continue
    pPr = p.find(W + "pPr")
    if pPr is None:
        continue
    jc = pPr.find(W + "jc")
    if jc is not None and jc.get(W + "val") in ("center", "right"):
        continue
    spacing = pPr.find(W + "spacing")
    if spacing is None or spacing.get(W + "line") != "480":
        continue
    ind = pPr.find(W + "ind")
    if ind is None:
        ind = etree.SubElement(pPr, W + "ind")
    ind.set(W + "firstLine", "720")  # 720 twips = 0.5"
    n_patched += 1
members["word/document.xml"] = etree.tostring(
    doc, xml_declaration=True, encoding="UTF-8", standalone=True)
with zipfile.ZipFile(src, "w", zipfile.ZIP_DEFLATED) as zout:
    for n, data in members.items():
        zout.writestr(n, data)
os.remove(tmp)
print(f"Indented {n_patched} body paragraphs", file=sys.stderr)
'
py_script <- file.path(tempdir(), "apa_indent.py")
writeLines(apa_indent_py, py_script)
sys_ret <- system2("python3", c(py_script, out_path), stderr = TRUE)
cat(paste(sys_ret, collapse = "\n"), "\n")

cat("Wrote:", out_path, "\n")
cat("Size:", file.info(out_path)$size, "bytes\n")
