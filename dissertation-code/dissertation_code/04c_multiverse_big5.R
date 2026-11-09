# =============================================================================
# MIDUS DISSERTATION - MULTIVERSE ANALYSIS: BIG FIVE ARMS (E & F)
# Two additional multiverse arms:
#   Arm E: Main-analysis models (m1-m20) with Big Five traits added as controls
#   Arm F: EF + Big Five only (no other controls; focal moderators retained
#          in the moderation models)
# Big Five: MIDUS pre-scored trait scales
#   MIDUS 2: B1SNEURO B1SEXTRA B1SOPEN B1SCONS1 B1SAGREE
#   MIDUS 3: C1SNEURO C1SEXTRA C1SOPEN C1SCONS1 C1SAGREE
# Standalone: run from the project root (dissertation-code/), e.g.
#   source("dissertation_code/04c_multiverse_big5.R")
# =============================================================================

library(tidyverse)
library(broom)
library(haven)
library(knitr)
library(kableExtra)

select <- dplyr::select
filter <- dplyr::filter

# =============================================================================
# DATA: ANALYTIC SAMPLES + BIG FIVE
# =============================================================================

# Rebuild the analytic samples if this script is run on its own
if (!exists("df_m2_analytic") || !exists("df_m3_analytic")) {
  source("dissertation_code/01_midus_data_read.R")
  source("dissertation_code/02_midus_scoring.R")
}

big5_m2_vars <- c("B1SNEURO", "B1SEXTRA", "B1SOPEN", "B1SCONS1", "B1SAGREE")
big5_m3_vars <- c("C1SNEURO", "C1SEXTRA", "C1SOPEN", "C1SCONS1", "C1SAGREE")

big5_m2 <- haven::read_sav("M2_P1_SURVEY_N4963_20200720.sav",
                           col_select = all_of(c("M2ID", big5_m2_vars)))
big5_m3 <- haven::read_sav("M3_P1_SURVEY_N3294_20181219.sav",
                           col_select = all_of(c("M2ID", big5_m3_vars)))

df_m2_b5 <- left_join(df_m2_analytic, big5_m2, by = "M2ID")
df_m3_b5 <- left_join(df_m3_analytic, big5_m3, by = "M2ID")

# Consistent complete-case samples across all Arm E/F models: main-analysis
# variables (03_regressions.R) plus the Big Five
all_vars_m2 <- c("B1SNEGWF", "B1SNEGFW", "B1SPOSWF", "B1SPOSFW",
                 "B3TEFZ1", "B1SJCDS_sum", "FAM_DEMANDS_M2",
                 "B1STINC1", "B1PHHSIZE_oth", "B1PAGE_M2", "B1PRSEX", "B1PB1",
                 "B1SJCSS_sum", "B1SJCCS_sum", "B1SSPEMP_sum", big5_m2_vars)
all_vars_m3 <- c("C1SNEGWF", "C1SNEGFW", "C1SPOSWF", "C1SPOSFW",
                 "C3TEF", "C1SJCDS_sum", "FAM_DEMANDS_M3",
                 "C1STINC", "C1PHHSIZE_oth", "C1PRAGE", "C1PRSEX", "C1PB1",
                 "C1SJCSS_sum", "C1SJCCS_sum", "C1SSPEMP_sum", big5_m3_vars)

df_m2_b5 <- df_m2_b5 %>% filter(complete.cases(across(all_of(all_vars_m2))))
df_m3_b5 <- df_m3_b5 %>% filter(complete.cases(across(all_of(all_vars_m3))))

# Re-center continuous predictors within the Big Five complete-case samples
# (mirrors the centering convention of 02_midus_scoring.R)
df_m2_b5$B3TEFZ1_c  <- scale(df_m2_b5$B3TEFZ1,      center = TRUE, scale = FALSE)
df_m2_b5$B1SJCSS_c  <- scale(df_m2_b5$B1SJCSS_sum,  center = TRUE, scale = FALSE)
df_m2_b5$B1SJCCS_c  <- scale(df_m2_b5$B1SJCCS_sum,  center = TRUE, scale = FALSE)
df_m2_b5$B1SSPEMP_c <- scale(df_m2_b5$B1SSPEMP_sum, center = TRUE, scale = FALSE)

df_m3_b5$C3TEF_c    <- scale(df_m3_b5$C3TEF,        center = TRUE, scale = FALSE)
df_m3_b5$C1SJCSS_c  <- scale(df_m3_b5$C1SJCSS_sum,  center = TRUE, scale = FALSE)
df_m3_b5$C1SJCCS_c  <- scale(df_m3_b5$C1SJCCS_sum,  center = TRUE, scale = FALSE)
df_m3_b5$C1SSPEMP_c <- scale(df_m3_b5$C1SSPEMP_sum, center = TRUE, scale = FALSE)

cat("=== Big Five Arm Sample Sizes ===\n")
cat("MIDUS 2:", nrow(df_m2_b5), "\n")
cat("MIDUS 3:", nrow(df_m3_b5), "\n")

stopifnot(
  "df_m2_b5 is empty" = nrow(df_m2_b5) > 0,
  "df_m3_b5 is empty" = nrow(df_m3_b5) > 0,
  "M2 Big5 has NAs"   = !anyNA(df_m2_b5[big5_m2_vars]),
  "M3 Big5 has NAs"   = !anyNA(df_m3_b5[big5_m3_vars])
)

# =============================================================================
# MODEL FORMULAS
# =============================================================================

b5_m2  <- paste(big5_m2_vars, collapse = " + ")
b5_m3  <- paste(big5_m3_vars, collapse = " + ")
ctl_m2 <- "B1SJCDS_sum + FAM_DEMANDS_M2 + B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1"
ctl_m3 <- "C1SJCDS_sum + FAM_DEMANDS_M3 + C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1"

outcomes_m2 <- c(NEGWF = "B1SNEGWF", NEGFW = "B1SNEGFW",
                 POSWF = "B1SPOSWF", POSFW = "B1SPOSFW")
outcomes_m3 <- c(NEGWF = "C1SNEGWF", NEGFW = "C1SNEGFW",
                 POSWF = "C1SPOSWF", POSFW = "C1SPOSFW")

# Moderator terms (centered, as in 03_regressions.R); gender enters uncentered
mods_m2 <- c(Main = NA, Gender = "B1PRSEX", Supervisor = "B1SJCSS_c",
             Coworker = "B1SJCCS_c", Spouse = "B1SSPEMP_c")
mods_m3 <- c(Main = NA, Gender = "C1PRSEX", Supervisor = "C1SJCSS_c",
             Coworker = "C1SJCCS_c", Spouse = "C1SSPEMP_c")

make_formula <- function(outcome, ef, ef_c, moderator, b5, controls = NULL) {
  focal <- if (is.na(moderator)) ef else paste0(ef_c, " * ", moderator)
  rhs <- paste(c(focal, controls, b5), collapse = " + ")
  as.formula(paste(outcome, "~", rhs))
}

run_wave <- function(data, outcomes, mods, ef, ef_c, b5, controls, arm_label) {
  results <- list()
  for (mod_name in names(mods)) {
    for (out_name in names(outcomes)) {
      # In moderation model sets, gender stays a control in Arm E (as in the
      # main analysis) via `controls`; Arm F drops all controls
      ctl <- controls
      if (!is.null(ctl) && mod_name == "Gender") {
        ctl <- gsub("\\s*\\+\\s*(B1PRSEX|C1PRSEX)", "", ctl)
      }
      f <- make_formula(outcomes[[out_name]], ef, ef_c, mods[[mod_name]], b5, ctl)
      m <- lm(f, data = data)
      results[[paste(mod_name, out_name, sep = "_")]] <- list(
        model = m, outcome = out_name, model_set = mod_name, arm = arm_label
      )
    }
  }
  results
}

# =============================================================================
# ARM E: MAIN-ANALYSIS MODELS + BIG FIVE CONTROLS
# =============================================================================

armE_m2 <- run_wave(df_m2_b5, outcomes_m2, mods_m2, "B3TEFZ1", "B3TEFZ1_c",
                    b5_m2, ctl_m2, "Arm E: Full controls + Big Five")
armE_m3 <- run_wave(df_m3_b5, outcomes_m3, mods_m3, "C3TEF", "C3TEF_c",
                    b5_m3, ctl_m3, "Arm E: Full controls + Big Five")

# =============================================================================
# ARM F: EF + BIG FIVE ONLY (NO OTHER CONTROLS)
# =============================================================================

armF_m2 <- run_wave(df_m2_b5, outcomes_m2, mods_m2, "B3TEFZ1", "B3TEFZ1_c",
                    b5_m2, NULL, "Arm F: EF + Big Five only")
armF_m3 <- run_wave(df_m3_b5, outcomes_m3, mods_m3, "C3TEF", "C3TEF_c",
                    b5_m3, NULL, "Arm F: EF + Big Five only")

# =============================================================================
# RESULTS EXTRACTION (EF-RELATED TERMS)
# =============================================================================

extract_ef <- function(entry, wave) {
  m <- entry$model
  mod_summary <- summary(m)
  b  <- coef(m)[-1]
  sx <- apply(model.matrix(m)[, -1, drop = FALSE], 2, sd)
  sy <- sd(m$model[[1]])
  std_beta <- b * sx / sy

  tidy(m) %>%
    filter(str_detect(term, "TEFZ1|C3TEF")) %>%
    mutate(
      wave = wave,
      arm = entry$arm,
      model_set = entry$model_set,
      outcome = entry$outcome,
      r_squared = mod_summary$r.squared,
      n = nobs(m),
      beta = std_beta[term]
    )
}

results_b5 <- bind_rows(
  map_dfr(armE_m2, extract_ef, wave = "MIDUS 2"),
  map_dfr(armE_m3, extract_ef, wave = "MIDUS 3"),
  map_dfr(armF_m2, extract_ef, wave = "MIDUS 2"),
  map_dfr(armF_m3, extract_ef, wave = "MIDUS 3")
)

# Normalized term labels so M2/M3 coefficients pair across waves
b5_term_labels <- c(
  "B3TEFZ1" = "EF", "C3TEF" = "EF",
  "B3TEFZ1_c" = "EF", "C3TEF_c" = "EF",
  "B3TEFZ1_c:B1PRSEX"   = "EF x Gender",
  "B3TEFZ1_c:B1SJCSS_c" = "EF x Supervisor Support",
  "B3TEFZ1_c:B1SJCCS_c" = "EF x Coworker Support",
  "B3TEFZ1_c:B1SSPEMP_c" = "EF x Spousal Support",
  "C3TEF_c:C1PRSEX"    = "EF x Gender",
  "C3TEF_c:C1SJCSS_c"  = "EF x Supervisor Support",
  "C3TEF_c:C1SJCCS_c"  = "EF x Coworker Support",
  "C3TEF_c:C1SSPEMP_c" = "EF x Spousal Support"
)
results_b5 <- results_b5 %>%
  mutate(term_label = ifelse(term %in% names(b5_term_labels),
                             b5_term_labels[term], term))

# =============================================================================
# CROSS-WAVE REPLICABILITY (OSC, 2015)
# =============================================================================
# Same three-step criteria as 03_regressions.R / 04_multiverse.R:
#   Step 1: W1-significant effect has the same sign in W2
#   Step 2: significant in both waves with the same sign
#   Step 3: both-wave-significant effects have overlapping 95% CIs

b5_w1 <- results_b5 %>%
  filter(wave == "MIDUS 2") %>%
  select(arm, model_set, outcome, term_label,
         est_w1 = estimate, se_w1 = std.error, p_w1 = p.value, beta_w1 = beta)

b5_w2 <- results_b5 %>%
  filter(wave == "MIDUS 3") %>%
  select(arm, model_set, outcome, term_label,
         est_w2 = estimate, se_w2 = std.error, p_w2 = p.value, beta_w2 = beta)

b5_paired_df <- inner_join(b5_w1, b5_w2,
                           by = c("arm", "model_set", "outcome", "term_label")) %>%
  mutate(
    ci_lo_w1 = est_w1 - 1.96 * se_w1,
    ci_hi_w1 = est_w1 + 1.96 * se_w1,
    ci_lo_w2 = est_w2 - 1.96 * se_w2,
    ci_hi_w2 = est_w2 + 1.96 * se_w2,
    sig_w1 = p_w1 < .05,
    sig_w2 = p_w2 < .05,
    same_sign = sign(est_w1) == sign(est_w2),
    replicated_sign = sig_w1 & same_sign,
    replicated_sig  = sig_w1 & sig_w2 & same_sign,
    ci_overlap = sig_w1 & sig_w2 & (ci_lo_w1 <= ci_hi_w2) & (ci_lo_w2 <= ci_hi_w1)
  )

n_sig_w1_b5   <- sum(b5_paired_df$sig_w1)
n_both_sig_b5 <- sum(b5_paired_df$sig_w1 & b5_paired_df$sig_w2)

cat("\n=== OSC (2015) Replicability: Big Five Arms ===\n")
if (n_sig_w1_b5 > 0) {
  cat(sprintf("Step 1 - Same sign:          %d/%d (%.1f%%)\n",
              sum(b5_paired_df$replicated_sign), n_sig_w1_b5,
              100 * sum(b5_paired_df$replicated_sign) / n_sig_w1_b5))
  cat(sprintf("Step 2 - Sign + significant: %d/%d (%.1f%%)\n",
              sum(b5_paired_df$replicated_sig), n_sig_w1_b5,
              100 * sum(b5_paired_df$replicated_sig) / n_sig_w1_b5))
} else {
  cat("Steps 1-2: N/A (no W1 effects significant)\n")
}
if (n_both_sig_b5 > 0) {
  cat(sprintf("Step 3 - Overlapping CIs:    %d/%d (%.1f%%)\n",
              sum(b5_paired_df$ci_overlap), n_both_sig_b5,
              100 * sum(b5_paired_df$ci_overlap) / n_both_sig_b5))
} else {
  cat("Step 3: N/A (no effects significant in both waves)\n")
}

# =============================================================================
# OUTPUT
# =============================================================================

out_dir <- "output/tables/multiverse_big5"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(results_b5 %>%
            select(arm, wave, model_set, outcome, term, term_label,
                   estimate, std.error, beta, p.value, r_squared, n),
          file.path(out_dir, "multiverse_big5_results_long.csv"))
write_csv(b5_paired_df, file.path(out_dir, "multiverse_big5_replicability.csv"))

# --- Summary HTML table ---
fmt_est <- function(est, p) sprintf("%.3f%s", est,
                                    ifelse(p < .001, "***",
                                           ifelse(p < .01, "**",
                                                  ifelse(p < .05, "*", ""))))

summary_tbl <- b5_paired_df %>%
  arrange(arm, model_set, outcome, term_label) %>%
  transmute(
    Arm = arm,
    `Model set` = model_set,
    Outcome = outcome,
    Term = term_label,
    `B (M2)` = fmt_est(est_w1, p_w1),
    `beta (M2)` = sprintf("%.3f", beta_w1),
    `B (M3)` = fmt_est(est_w2, p_w2),
    `beta (M3)` = sprintf("%.3f", beta_w2),
    `Same sign` = ifelse(same_sign, "Yes", "No"),
    `Sig. both waves` = ifelse(sig_w1 & sig_w2, "Yes", "No"),
    `Replicated (OSC)` = ifelse(replicated_sig & ci_overlap, "Yes",
                                ifelse(sig_w1, "No", "N/A"))
  )

n_specs  <- nrow(b5_paired_df)
n_sig_m2 <- sum(b5_paired_df$sig_w1)
n_sig_m3 <- sum(b5_paired_df$sig_w2)

summary_tbl %>%
  kbl(caption = paste0(
        "Multiverse Arms E and F: EF Effects with Big Five Personality Controls. ",
        "MIDUS 2 N = ", nrow(df_m2_b5), "; MIDUS 3 N = ", nrow(df_m3_b5), ". ",
        n_specs, " paired EF terms; significant in MIDUS 2: ", n_sig_m2,
        "; significant in MIDUS 3: ", n_sig_m3, ". ",
        "Arm E adds the Big Five (neuroticism, extraversion, openness, ",
        "conscientiousness, agreeableness) to the full main-analysis control set. ",
        "Arm F retains only EF, the focal moderator (where applicable), and the ",
        "Big Five. Replicability follows Open Science Collaboration (2015) criteria. ",
        "* p < .05. ** p < .01. *** p < .001."),
      format = "html", escape = TRUE) %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  save_kable(file.path(out_dir, "multiverse_big5_summary.html"))

cat("\nBig Five arm outputs written to", out_dir, "\n")

stopifnot(
  "no Arm E rows"   = any(grepl("Arm E", results_b5$arm)),
  "no Arm F rows"   = any(grepl("Arm F", results_b5$arm)),
  "pairing dropped rows" = nrow(b5_paired_df) * 2 == nrow(results_b5)
)
cat("04c_multiverse_big5 checks passed.\n")
