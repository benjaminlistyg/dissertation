# =============================================================================
# MIDUS DISSERTATION ANALYSIS - REGRESSION MODELS WITH APA TABLES
# Executive Functioning and Work-Family Spillover
# =============================================================================

library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)

# =============================================================================
# CREATE CONSISTENT SAMPLES
# =============================================================================

# --- Base variables (used in all models) ---
base_vars_m2 <- c("B1SNEGWF", "B1SNEGFW", "B1SPOSWF", "B1SPOSFW",
                  "B3TEFZ1", "B1SJCDS_mean", "FAM_DEMANDS_M2",
                  "B1STINC1", "B1PHHSIZE_oth", "B1PAGE_M2", "B1PRSEX", "B1PB1")

base_vars_m3 <- c("C1SNEGWF", "C1SNEGFW", "C1SPOSWF", "C1SPOSFW",
                  "C3TEF", "C1SJCDS_mean", "FAM_DEMANDS_M3",
                  "C1STINC", "C1PHHSIZE_oth", "C1PRAGE", "C1PRSEX", "C1PB1")

# --- Consistent sample across ALL models ---
all_vars_m2 <- c(base_vars_m2, "B1SJCSS_mean", "B1SJCCS_mean", "B1SSPEMP_mean")
all_vars_m3 <- c(base_vars_m3, "C1SJCSS_mean", "C1SJCCS_mean", "C1SSPEMP_mean")

df_m2_all <- df_m2_analytic %>% filter(complete.cases(across(all_of(all_vars_m2))))
df_m3_all <- df_m3_analytic %>% filter(complete.cases(across(all_of(all_vars_m3))))

# --- Regression-sample IDs (carried but never entered as predictors) ---
# These IDs identify exactly the respondents the lm() models see, and are
# exported so 03b/07 can filter the descriptive/reliability data to the
# same set of respondents.
stopifnot(
  "M2ID missing from df_m2_all" = "M2ID" %in% names(df_m2_all),
  "M2ID missing from df_m3_all" = "M2ID" %in% names(df_m3_all),
  "duplicate M2ID in df_m2_all" = !anyDuplicated(df_m2_all$M2ID),
  "duplicate M2ID in df_m3_all" = !anyDuplicated(df_m3_all$M2ID)
)
reg_ids_m2 <- df_m2_all$M2ID
reg_ids_m3 <- df_m3_all$M2ID

cat("=== Consistent Sample Sizes ===\n")
cat("MIDUS 2:", nrow(df_m2_all), " (", length(reg_ids_m2), " IDs)\n", sep = "")
cat("MIDUS 3:", nrow(df_m3_all), " (", length(reg_ids_m3), " IDs)\n", sep = "")

# --- Sanity checks ---
stopifnot(
  "df_m2_all is empty"          = nrow(df_m2_all) > 0,
  "df_m3_all is empty"          = nrow(df_m3_all) > 0,
  "df_m2_all has NAs in vars"   = sum(complete.cases(df_m2_all[all_vars_m2])) == nrow(df_m2_all),
  "df_m3_all has NAs in vars"   = sum(complete.cases(df_m3_all[all_vars_m3])) == nrow(df_m3_all),
  "df_m2_all bigger than analytic" = nrow(df_m2_all) <= nrow(df_m2_analytic),
  "df_m3_all bigger than analytic" = nrow(df_m3_all) <= nrow(df_m3_analytic)
)
cat("03_regressions sample checks passed.\n")

# =============================================================================
# APA TABLE FUNCTIONS
# =============================================================================

format_p_stars <- function(p) {
  case_when(
    p < .001 ~ "***",
    p < .01 ~ "**",
    p < .05 ~ "*",
    TRUE ~ ""
  )
}

# Variable name labels
var_labels <- c(
  "(Intercept)" = "Intercept",
  # MIDUS 2
  "B3TEFZ1" = "Executive Functioning",
  "B1SJCDS_mean" = "Job Demands",
  "FAM_DEMANDS_M2" = "Family Demands",
  "B1STINC1" = "Household Income",
  "B1PHHSIZE_oth" = "Household Size",
  "B1PAGE_M2" = "Age",
  "B1PRSEX" = "Gender",
  "B1PB1" = "Education",
  "B1SJCSS_mean" = "Supervisor Support",
  "B1SJCCS_mean" = "Coworker Support",
  "B1SSPEMP_mean" = "Spousal Support",
  # MIDUS 3
  "C3TEF" = "Executive Functioning",
  "C1SJCDS_mean" = "Job Demands",
  "FAM_DEMANDS_M3" = "Family Demands",
  "C1STINC" = "Household Income",
  "C1PHHSIZE_oth" = "Household Size",
  "C1PRAGE" = "Age",
  "C1PRSEX" = "Gender",
  "C1PB1" = "Education",
  "C1SJCSS_mean" = "Supervisor Support",
  "C1SJCCS_mean" = "Coworker Support",
  "C1SSPEMP_mean" = "Spousal Support",
  # Interactions
  "B3TEFZ1:B1PRSEX" = "Executive Functioning × Gender",
  "B3TEFZ1:B1SJCSS_mean" = "Executive Functioning × Supervisor Support",
  "B3TEFZ1:B1SJCCS_mean" = "Executive Functioning × Coworker Support",
  "B3TEFZ1:B1SSPEMP_mean" = "Executive Functioning × Spousal Support",
  "C3TEF:C1PRSEX" = "Executive Functioning × Gender",
  "C3TEF:C1SJCSS_mean" = "Executive Functioning × Supervisor Support",
  "C3TEF:C1SJCCS_mean" = "Executive Functioning × Coworker Support",
  "C3TEF:C1SSPEMP_mean" = "Executive Functioning × Spousal Support",
  # Factor-coded gender aliases (Male reference -> coefficient name *PRSEXFemale)
  "B1PRSEXFemale" = "Gender",
  "C1PRSEXFemale" = "Gender",
  "B3TEFZ1:B1PRSEXFemale" = "Executive Functioning × Gender",
  "B1PRSEXFemale:B3TEFZ1" = "Executive Functioning × Gender",
  "C3TEF:C1PRSEXFemale"   = "Executive Functioning × Gender",
  "C1PRSEXFemale:C3TEF"   = "Executive Functioning × Gender",
  "B3TEFZ1_c:B1PRSEXFemale" = "Executive Functioning × Gender",
  "B1PRSEXFemale:B3TEFZ1_c" = "Executive Functioning × Gender",
  "C3TEF_c:C1PRSEXFemale"   = "Executive Functioning × Gender",
  "C1PRSEXFemale:C3TEF_c"   = "Executive Functioning × Gender"
)

# Function to apply labels
label_terms <- function(term) {
  ifelse(term %in% names(var_labels), var_labels[term], term)
}

# Single model table
apa_table_single <- function(model, title = "Regression Results", note = NULL) {
  mod_summary <- summary(model)
  
  tbl <- tidy(model) %>%
    mutate(
      term = label_terms(term),
      b = sprintf("%.3f", estimate),
      se = sprintf("%.3f", std.error),
      t = sprintf("%.2f", statistic),
      p = format_p_stars(p.value),
      ci = paste0("[", sprintf("%.3f", estimate - 1.96*std.error), ", ", 
                  sprintf("%.3f", estimate + 1.96*std.error), "]"),
      sig = p.value < .05
    ) %>%
    select(term, b, se, t, p, ci, sig)
  
  r2 <- sprintf("%.3f", mod_summary$r.squared)
  adj_r2 <- sprintf("%.3f", mod_summary$adj.r.squared)
  f_stat <- mod_summary$fstatistic
  fit_note <- paste0("R² = ", r2, ", Adjusted R² = ", adj_r2, 
                     ", F(", f_stat[2], ", ", f_stat[3], ") = ", sprintf("%.2f", f_stat[1]), 
                     ", N = ", nobs(model))
  full_note <- paste0(if (!is.null(note)) paste0(note, "\n") else "", 
                      fit_note, "\n*p < .05. **p < .01. ***p < .001.")
  
  # Find rows
  ef_rows <- which(str_detect(tbl$term, "Executive Functioning"))
  sig_rows <- which(tbl$sig)
  
  kable(tbl %>% select(-sig), col.names = c("Predictor", "B", "SE", "t", "", "95% CI"),
        caption = title, align = c("l", "r", "r", "r", "l", "c")) %>%
    kable_styling(bootstrap_options = "condensed", full_width = FALSE,
                  html_font = "Times New Roman") %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(sig_rows, bold = TRUE) %>%
    row_spec(ef_rows, underline = TRUE) %>%
    footnote(general = full_note, general_title = "Note. ", footnote_as_chunk = TRUE)
}

# Multiple models side-by-side
apa_table_multi <- function(models, model_names, title = "Regression Results", note = NULL) {
  
  # Get significance info from first model for each term
  sig_info <- map2_dfr(models, model_names, function(mod, name) {
    tidy(mod) %>%
      mutate(model = name,
             term = label_terms(term),
             sig = p.value < .05) %>%
      select(model, term, sig)
  })
  
  all_coefs <- map2_dfr(models, model_names, function(mod, name) {
    tidy(mod) %>%
      mutate(model = name,
             term = label_terms(term),
             coef = paste0(sprintf("%.3f", estimate), format_p_stars(p.value)),
             se = paste0("(", sprintf("%.3f", std.error), ")")) %>%
      select(model, term, coef, se)
  })
  
  predictors <- unique(all_coefs$term)
  
  coef_wide <- all_coefs %>%
    pivot_wider(id_cols = term, names_from = model,
                values_from = c(coef, se), names_glue = "{model}_{.value}")
  
  col_order <- c("term", as.vector(rbind(paste0(model_names, "_coef"), paste0(model_names, "_se"))))
  coef_wide <- coef_wide %>% 
    select(all_of(col_order)) %>%
    mutate(term = factor(term, levels = predictors)) %>%
    arrange(term) %>%
    mutate(term = as.character(term))
  
  # Check if any model has significant effect for each term
  sig_any <- sig_info %>%
    group_by(term) %>%
    summarise(any_sig = any(sig), .groups = "drop")
  
  # Model fit stats
  fit_rows <- map_dfr(seq_along(models), function(i) {
    s <- summary(models[[i]])
    tibble(term = c("R²", "Adjusted R²", "N")[c(TRUE, TRUE, TRUE)],
           coef = c(sprintf("%.3f", s$r.squared), 
                    sprintf("%.3f", s$adj.r.squared),
                    as.character(nobs(models[[i]]))),
           se = "", model = model_names[i])
  }) %>%
    pivot_wider(id_cols = term, names_from = model,
                values_from = c(coef, se), names_glue = "{model}_{.value}") %>%
    select(all_of(col_order))
  
  final_tbl <- bind_rows(coef_wide, fit_rows)
  col_headers <- c(" ", rep(c("B", "SE"), length(model_names)))
  full_note <- paste0(if (!is.null(note)) paste0(note, "\n") else "", 
                      "*p < .05. **p < .01. ***p < .001.")
  
  # Find rows containing Executive Functioning
  ef_rows <- which(str_detect(final_tbl$term, "Executive Functioning"))
  
  # Find rows with any significant effect (in coefficient rows only)
  sig_rows <- which(final_tbl$term %in% sig_any$term[sig_any$any_sig])
  
  kable(final_tbl, col.names = col_headers, caption = title,
        align = c("l", rep("r", length(model_names)*2))) %>%
    kable_styling(bootstrap_options = "condensed", full_width = FALSE,
                  html_font = "Times New Roman") %>%
    add_header_above(setNames(c(1, rep(2, length(model_names))), c(" ", model_names))) %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(sig_rows, bold = TRUE) %>%
    row_spec(ef_rows, underline = TRUE) %>%
    footnote(general = full_note, general_title = "Note. ", footnote_as_chunk = TRUE)
}

# Interaction table (bolds interaction term)
apa_table_interaction <- function(model, title = "Moderation Analysis", note = NULL) {
  mod_summary <- summary(model)
  
  tbl <- tidy(model) %>%
    mutate(
      is_int = str_detect(term, ":"),
      term = label_terms(term),
      b = sprintf("%.3f", estimate),
      se = sprintf("%.3f", std.error),
      t = sprintf("%.2f", statistic),
      p = format_p_stars(p.value),
      ci = paste0("[", sprintf("%.3f", estimate - 1.96*std.error), ", ", 
                  sprintf("%.3f", estimate + 1.96*std.error), "]"),
      sig = p.value < .05
    ) %>%
    select(term, b, se, t, p, ci, is_int, sig)
  
  r2 <- sprintf("%.3f", mod_summary$r.squared)
  adj_r2 <- sprintf("%.3f", mod_summary$adj.r.squared)
  f_stat <- mod_summary$fstatistic
  fit_note <- paste0("R² = ", r2, ", Adjusted R² = ", adj_r2,
                     ", F(", f_stat[2], ", ", f_stat[3], ") = ", sprintf("%.2f", f_stat[1]),
                     ", N = ", nobs(model))
  full_note <- paste0(if (!is.null(note)) paste0(note, "\n") else "",
                      fit_note, "\n*p < .05. **p < .01. ***p < .001.")
  
  # Find rows
  ef_rows <- which(str_detect(tbl$term, "Executive Functioning"))
  sig_rows <- which(tbl$sig)
  
  kable(tbl %>% select(-is_int, -sig), col.names = c("Predictor", "B", "SE", "t", "", "95% CI"),
        caption = title, align = c("l", "r", "r", "r", "l", "c")) %>%
    kable_styling(bootstrap_options = "condensed", full_width = FALSE,
                  html_font = "Times New Roman") %>%
    row_spec(0, bold = TRUE) %>%
    row_spec(sig_rows, bold = TRUE) %>%
    row_spec(ef_rows, underline = TRUE) %>%
    footnote(general = full_note, general_title = "Note. ", footnote_as_chunk = TRUE)
}

# =============================================================================
# MIDUS 2 REGRESSIONS
# =============================================================================

# H1: EF → Negative Work-to-Family Spillover
m1_negwf_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_all)

# H2: EF → Negative Family-to-Work Spillover
m2_negfw_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_all)

# H3: EF → Positive Work-to-Family Spillover
m3_poswf_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_all)

# H4: EF → Positive Family-to-Work Spillover
m4_posfw_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_all)

# Gender Moderation
m5_gender_negfw_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m6_gender_negwf_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m7_gender_posfw_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m8_gender_poswf_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)

# Supervisor Support Moderation
m9_super_negfw_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                          B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m10_super_negwf_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m11_super_posfw_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m12_super_poswf_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)

# Coworker Support Moderation
m13_cowork_negfw_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m14_cowork_negwf_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m15_cowork_posfw_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m16_cowork_poswf_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)

# Spousal Support Moderation
m17_spouse_negfw_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m18_spouse_negwf_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m19_spouse_posfw_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)
m20_spouse_poswf_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_mean + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_all)

# =============================================================================
# MIDUS 3 REGRESSIONS
# =============================================================================

# H1-H4: Main Effects
m1_negwf_m3 <- lm(C1SNEGWF ~ C3TEF + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_all)
m2_negfw_m3 <- lm(C1SNEGFW ~ C3TEF + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_all)
m3_poswf_m3 <- lm(C1SPOSWF ~ C3TEF + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_all)
m4_posfw_m3 <- lm(C1SPOSFW ~ C3TEF + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_all)

# Gender Moderation
m5_gender_negfw_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m6_gender_negwf_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m7_gender_posfw_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m8_gender_poswf_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)

# Supervisor Support Moderation
m9_super_negfw_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                          C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m10_super_negwf_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m11_super_posfw_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m12_super_poswf_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)

# Coworker Support Moderation
m13_cowork_negfw_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m14_cowork_negwf_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m15_cowork_posfw_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m16_cowork_poswf_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)

# Spousal Support Moderation
m17_spouse_negfw_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m18_spouse_negwf_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m19_spouse_posfw_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)
m20_spouse_poswf_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_mean + FAM_DEMANDS_M3 +
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_all)

# =============================================================================
# RAW (UNCENTERED) PARALLEL INTERACTION MODELS
# =============================================================================
# For each centered interaction model fit above (m5-m20 for both waves),
# build a sibling `_raw` model that replaces every centered (_c) variable
# with its raw counterpart. The raw versions sit alongside the centered
# ones in memory and are exported to a separate output folder by 07 so
# coefficients can be compared head-to-head.

.raw_var_swaps <- c(
  "B3TEFZ1_c"        = "B3TEFZ1",
  "C3TEF_c"          = "C3TEF",
  "B1SJCSS_c"        = "B1SJCSS_mean",
  "C1SJCSS_c"        = "C1SJCSS_mean",
  "B1SJCCS_c"        = "B1SJCCS_mean",
  "C1SJCCS_c"        = "C1SJCCS_mean",
  "B1SSPEMP_c"       = "B1SSPEMP_mean",
  "C1SSPEMP_c"       = "C1SSPEMP_mean",
  "B1SJCDS_c"        = "B1SJCDS_mean",
  "C1SJCDS_c"        = "C1SJCDS_mean",
  "FAM_DEMANDS_M2_c" = "FAM_DEMANDS_M2",
  "FAM_DEMANDS_M3_c" = "FAM_DEMANDS_M3"
)

make_raw <- function(centered_model) {
  f_text <- paste(deparse(formula(centered_model)), collapse = " ")
  for (k in names(.raw_var_swaps)) {
    f_text <- gsub(paste0("\\b", k, "\\b"), .raw_var_swaps[[k]], f_text)
  }
  update(centered_model, as.formula(f_text))
}

.interaction_models <- c(
  paste0(c("m5_gender_negfw","m6_gender_negwf","m7_gender_posfw","m8_gender_poswf",
           "m9_super_negfw", "m10_super_negwf","m11_super_posfw","m12_super_poswf",
           "m13_cowork_negfw","m14_cowork_negwf","m15_cowork_posfw","m16_cowork_poswf",
           "m17_spouse_negfw","m18_spouse_negwf","m19_spouse_posfw","m20_spouse_poswf"),
         "_m2"),
  paste0(c("m5_gender_negfw","m6_gender_negwf","m7_gender_posfw","m8_gender_poswf",
           "m9_super_negfw", "m10_super_negwf","m11_super_posfw","m12_super_poswf",
           "m13_cowork_negfw","m14_cowork_negwf","m15_cowork_posfw","m16_cowork_poswf",
           "m17_spouse_negfw","m18_spouse_negwf","m19_spouse_posfw","m20_spouse_poswf"),
         "_m3")
)
for (.nm in .interaction_models) {
  if (exists(.nm)) {
    assign(paste0(.nm, "_raw"), make_raw(get(.nm)))
  }
}
cat("Built ", length(.interaction_models), " uncentered (_raw) interaction models.\n",
    sep = "")

# =============================================================================
# GENERATE APA TABLES
# =============================================================================

# Table 1: MIDUS 2 Main Effects (H1-H4)
table1_m2 <- apa_table_multi(
  models = list(m1_negwf_m2, m2_negfw_m2, m3_poswf_m2, m4_posfw_m2),
  model_names = c("Neg W→F", "Neg F→W", "Pos W→F", "Pos F→W"),
  title = "Table 1\nMain Effects of Executive Functioning on Work-Family Spillover (MIDUS 2)"
)

# Table 2: MIDUS 3 Main Effects (H1-H4)
table2_m3 <- apa_table_multi(
  models = list(m1_negwf_m3, m2_negfw_m3, m3_poswf_m3, m4_posfw_m3),
  model_names = c("Neg W→F", "Neg F→W", "Pos W→F", "Pos F→W"),
  title = "Table 2\nMain Effects of Executive Functioning on Work-Family Spillover (MIDUS 3)"
)

# Table 3: Gender Moderation - MIDUS 2
table3_gender_m2 <- apa_table_multi(
  models = list(m5_gender_negfw_m2, m6_gender_negwf_m2, m7_gender_posfw_m2, m8_gender_poswf_m2),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 3\nGender × Executive Functioning Interactions (MIDUS 2)"
)

# Table 4: Gender Moderation - MIDUS 3
table4_gender_m3 <- apa_table_multi(
  models = list(m5_gender_negfw_m3, m6_gender_negwf_m3, m7_gender_posfw_m3, m8_gender_poswf_m3),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 4\nGender × Executive Functioning Interactions (MIDUS 3)"
)

# Table 5: Supervisor Support Moderation - MIDUS 2
table5_super_m2 <- apa_table_multi(
  models = list(m9_super_negfw_m2, m10_super_negwf_m2, m11_super_posfw_m2, m12_super_poswf_m2),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 5\nSupervisor Support × Executive Functioning Interactions (MIDUS 2)"
)

# Table 6: Supervisor Support Moderation - MIDUS 3
table6_super_m3 <- apa_table_multi(
  models = list(m9_super_negfw_m3, m10_super_negwf_m3, m11_super_posfw_m3, m12_super_poswf_m3),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 6\nSupervisor Support × Executive Functioning Interactions (MIDUS 3)"
)

# Table 7: Coworker Support Moderation - MIDUS 2
table7_cowork_m2 <- apa_table_multi(
  models = list(m13_cowork_negfw_m2, m14_cowork_negwf_m2, m15_cowork_posfw_m2, m16_cowork_poswf_m2),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 7\nCoworker Support × Executive Functioning Interactions (MIDUS 2)"
)

# Table 8: Coworker Support Moderation - MIDUS 3
table8_cowork_m3 <- apa_table_multi(
  models = list(m13_cowork_negfw_m3, m14_cowork_negwf_m3, m15_cowork_posfw_m3, m16_cowork_poswf_m3),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 8\nCoworker Support × Executive Functioning Interactions (MIDUS 3)"
)

# Table 9: Spousal Support Moderation - MIDUS 2
table9_spouse_m2 <- apa_table_multi(
  models = list(m17_spouse_negfw_m2, m18_spouse_negwf_m2, m19_spouse_posfw_m2, m20_spouse_poswf_m2),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 9\nSpousal Support × Executive Functioning Interactions (MIDUS 2)"
)

# Table 10: Spousal Support Moderation - MIDUS 3
table10_spouse_m3 <- apa_table_multi(
  models = list(m17_spouse_negfw_m3, m18_spouse_negwf_m3, m19_spouse_posfw_m3, m20_spouse_poswf_m3),
  model_names = c("Neg F→W", "Neg W→F", "Pos F→W", "Pos W→F"),
  title = "Table 10\nSpousal Support × Executive Functioning Interactions (MIDUS 3)"
)

# =============================================================================
# TABLES BUILT (not rendered)
# =============================================================================
# The kable objects above are kept in memory for downstream use, but are
# intentionally NOT printed to the RStudio Viewer. (Previously these were
# print()-ed here, which opened each HTML table in the Viewer pane.)

# =============================================================================
# CROSS-WAVE REPLICABILITY (OSC, 2015)
# =============================================================================
# Compare EF-related coefficients across MIDUS 2 (W1) and MIDUS 3 (W2)
# using the Open Science Collaboration (2015) three-step criteria.

model_pairs <- list(
  list(id = "m1: Neg W-to-F (Main)",           m2 = m1_negwf_m2,          m3 = m1_negwf_m3),
  list(id = "m2: Neg F-to-W (Main)",           m2 = m2_negfw_m2,          m3 = m2_negfw_m3),
  list(id = "m3: Pos W-to-F (Main)",           m2 = m3_poswf_m2,          m3 = m3_poswf_m3),
  list(id = "m4: Pos F-to-W (Main)",           m2 = m4_posfw_m2,          m3 = m4_posfw_m3),
  list(id = "m5: Neg F-to-W (Gender)",         m2 = m5_gender_negfw_m2,   m3 = m5_gender_negfw_m3),
  list(id = "m6: Neg W-to-F (Gender)",         m2 = m6_gender_negwf_m2,   m3 = m6_gender_negwf_m3),
  list(id = "m7: Pos F-to-W (Gender)",         m2 = m7_gender_posfw_m2,   m3 = m7_gender_posfw_m3),
  list(id = "m8: Pos W-to-F (Gender)",         m2 = m8_gender_poswf_m2,   m3 = m8_gender_poswf_m3),
  list(id = "m9: Neg F-to-W (Supervisor)",     m2 = m9_super_negfw_m2,    m3 = m9_super_negfw_m3),
  list(id = "m10: Neg W-to-F (Supervisor)",    m2 = m10_super_negwf_m2,   m3 = m10_super_negwf_m3),
  list(id = "m11: Pos W-to-F (Supervisor)",    m2 = m11_super_posfw_m2,   m3 = m11_super_posfw_m3),
  list(id = "m12: Pos W-to-F (Supervisor)",    m2 = m12_super_poswf_m2,   m3 = m12_super_poswf_m3),
  list(id = "m13: Neg F-to-W (Coworker)",      m2 = m13_cowork_negfw_m2,  m3 = m13_cowork_negfw_m3),
  list(id = "m14: Neg W-to-F (Coworker)",      m2 = m14_cowork_negwf_m2,  m3 = m14_cowork_negwf_m3),
  list(id = "m15: Pos F-to-W (Coworker)",      m2 = m15_cowork_posfw_m2,  m3 = m15_cowork_posfw_m3),
  list(id = "m16: Pos W-to-F (Coworker)",      m2 = m16_cowork_poswf_m2,  m3 = m16_cowork_poswf_m3),
  list(id = "m17: Neg F-to-W (Spouse)",        m2 = m17_spouse_negfw_m2,  m3 = m17_spouse_negfw_m3),
  list(id = "m18: Neg W-to-F (Spouse)",        m2 = m18_spouse_negwf_m2,  m3 = m18_spouse_negwf_m3),
  list(id = "m19: Pos F-to-W (Spouse)",        m2 = m19_spouse_posfw_m2,  m3 = m19_spouse_posfw_m3),
  list(id = "m20: Pos W-to-F (Spouse)",        m2 = m20_spouse_poswf_m2,  m3 = m20_spouse_poswf_m3)
)

# Extract and pair EF terms across waves
reg_paired_df <- map_dfr(model_pairs, function(pair) {
  w1 <- tidy(pair$m2) %>%
    filter(str_detect(term, "B3TEFZ1")) %>%
    mutate(term_label = label_terms(term)) %>%
    select(term_label, term_w1 = term,
           est_w1 = estimate, se_w1 = std.error, p_w1 = p.value)

  w2 <- tidy(pair$m3) %>%
    filter(str_detect(term, "C3TEF")) %>%
    mutate(term_label = label_terms(term)) %>%
    select(term_label, term_w2 = term,
           est_w2 = estimate, se_w2 = std.error, p_w2 = p.value)

  inner_join(w1, w2, by = "term_label") %>%
    mutate(pair_id = pair$id)
})

# Compute OSC (2015) criteria
reg_paired_df <- reg_paired_df %>%
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

# Print summary
n_sig_w1   <- sum(reg_paired_df$sig_w1)
n_both_sig <- sum(reg_paired_df$sig_w1 & reg_paired_df$sig_w2)

cat("\n=== OSC (2015) Replicability: Primary Regressions ===\n")
if (n_sig_w1 > 0) {
  cat(sprintf("Step 1 - Same sign:          %d/%d (%.1f%%)\n",
              sum(reg_paired_df$replicated_sign), n_sig_w1,
              100 * sum(reg_paired_df$replicated_sign) / n_sig_w1))
  cat(sprintf("Step 2 - Sign + significant: %d/%d (%.1f%%)\n",
              sum(reg_paired_df$replicated_sig), n_sig_w1,
              100 * sum(reg_paired_df$replicated_sig) / n_sig_w1))
} else {
  cat("Steps 1-2: N/A (no W1 effects significant)\n")
}
if (n_both_sig > 0) {
  cat(sprintf("Step 3 - Overlapping CIs:    %d/%d (%.1f%%)\n",
              sum(reg_paired_df$ci_overlap), n_both_sig,
              100 * sum(reg_paired_df$ci_overlap) / n_both_sig))
} else {
  cat("Step 3: N/A (no effects significant in both waves)\n")
}

# Store for downstream APA table generation (07_midus_apa_tables.R)
reg_replicability <- reg_paired_df

# --- Sanity checks: models ---
stopifnot(
  # All 20 M2 models are lm objects
  "m1_negwf_m2 not lm"      = inherits(m1_negwf_m2, "lm"),
  "m20_spouse_poswf_m2 not lm" = inherits(m20_spouse_poswf_m2, "lm"),
  # All 20 M3 models are lm objects
  "m1_negwf_m3 not lm"      = inherits(m1_negwf_m3, "lm"),
  "m20_spouse_poswf_m3 not lm" = inherits(m20_spouse_poswf_m3, "lm"),
  # All models use the same N within each wave
  "M2 models have different N" = nobs(m1_negwf_m2) == nobs(m20_spouse_poswf_m2),
  "M3 models have different N" = nobs(m1_negwf_m3) == nobs(m20_spouse_poswf_m3),
  # EF term exists in main-effect models
  "EF missing from M2 model" = "B3TEFZ1" %in% names(coef(m1_negwf_m2)),
  "EF missing from M3 model" = "C3TEF" %in% names(coef(m1_negwf_m3))
)
cat("03_regressions model checks passed.\n")