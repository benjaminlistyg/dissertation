# =============================================================================
# MIDUS DISSERTATION - MULTIVERSE ANALYSIS
# Robustness Checks per Preregistration Amendment
# =============================================================================
# Decision points (arms):
# (a) Covariates: with/without; alternative operationalizations
# (b) Covariate specification: covariates as moderators
# (c) Inclusion criteria: alternative employment specifications
# (d) Sample specification: model-set specific vs. all-models consistent
# =============================================================================

library(tidyverse)
library(broom)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

extract_results <- function(model, outcome_name, specification) {
  # Get model summary stats first
  mod_summary <- summary(model)
  r_sq <- mod_summary$r.squared
  adj_r_sq <- mod_summary$adj.r.squared
  n_obs <- nobs(model)

  # Standardized betas: beta_i = b_i * sd(x_i) / sd(y)
  b <- coef(model)[-1]
  sx <- apply(model.matrix(model)[, -1, drop = FALSE], 2, sd)
  sy <- sd(model$model[[1]])
  std_beta <- b * sx / sy

  # Extract EF coefficients
  tidy(model) %>%
    filter(str_detect(term, "TEF|EF")) %>%
    mutate(
      outcome = outcome_name,
      specification = specification,
      r_squared = r_sq,
      adj_r_squared = adj_r_sq,
      n = n_obs,
      beta = std_beta[term]
    )
}

# =============================================================================
# ARM (D): MODEL-SET SPECIFIC SAMPLES
# =============================================================================
# Alternative to using df_m2_all/df_m3_all (consistent across ALL models)
# These samples are consistent only within each model set

# --- Base variables ---
base_vars_m2 <- c("B1SNEGWF", "B1SNEGFW", "B1SPOSWF", "B1SPOSFW",
                  "B3TEFZ1", "B1SJCDS_sum", "FAM_DEMANDS_M2",
                  "B1STINC1", "B1PHHSIZE_oth", "B1PAGE_M2", "B1PRSEX", "B1PB1")

base_vars_m3 <- c("C1SNEGWF", "C1SNEGFW", "C1SPOSWF", "C1SPOSFW",
                  "C3TEF", "C1SJCDS_sum", "FAM_DEMANDS_M3",
                  "C1STINC", "C1PHHSIZE_oth", "C1PRAGE", "C1PRSEX", "C1PB1")

# Main effects & Gender moderation (same variables)
df_m2_main <- df_m2_analytic %>% filter(complete.cases(across(all_of(base_vars_m2))))
df_m3_main <- df_m3_analytic %>% filter(complete.cases(across(all_of(base_vars_m3))))

# Supervisor support moderation
df_m2_super <- df_m2_analytic %>% filter(complete.cases(across(all_of(c(base_vars_m2, "B1SJCSS_sum")))))
df_m3_super <- df_m3_analytic %>% filter(complete.cases(across(all_of(c(base_vars_m3, "C1SJCSS_sum")))))

# Coworker support moderation
df_m2_cowork <- df_m2_analytic %>% filter(complete.cases(across(all_of(c(base_vars_m2, "B1SJCCS_sum")))))
df_m3_cowork <- df_m3_analytic %>% filter(complete.cases(across(all_of(c(base_vars_m3, "C1SJCCS_sum")))))

# Spousal support moderation
df_m2_spouse <- df_m2_analytic %>% filter(complete.cases(across(all_of(c(base_vars_m2, "B1SSPEMP_sum")))))
df_m3_spouse <- df_m3_analytic %>% filter(complete.cases(across(all_of(c(base_vars_m3, "C1SSPEMP_sum")))))

cat("=== Model-Set Specific Sample Sizes ===\n")
cat("MIDUS 2 - Main/Gender:", nrow(df_m2_main), "\n")
cat("MIDUS 2 - Supervisor:", nrow(df_m2_super), "\n")
cat("MIDUS 2 - Coworker:", nrow(df_m2_cowork), "\n")
cat("MIDUS 2 - Spousal:", nrow(df_m2_spouse), "\n")
cat("MIDUS 3 - Main/Gender:", nrow(df_m3_main), "\n")
cat("MIDUS 3 - Supervisor:", nrow(df_m3_super), "\n")
cat("MIDUS 3 - Coworker:", nrow(df_m3_cowork), "\n")
cat("MIDUS 3 - Spousal:", nrow(df_m3_spouse), "\n")

# -----------------------------------------------------------------------------
# D1: Main Effects with model-set specific sample - MIDUS 2
# -----------------------------------------------------------------------------
m1_main_setspec_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_main)
m2_main_setspec_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_main)
m3_main_setspec_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_main)
m4_main_setspec_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                           B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_main)

# -----------------------------------------------------------------------------
# D2: Gender Moderation with model-set specific sample - MIDUS 2
# -----------------------------------------------------------------------------
m5_gender_setspec_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                             B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_main)
m6_gender_setspec_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                             B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_main)
m7_gender_setspec_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                             B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_main)
m8_gender_setspec_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                             B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_main)

# -----------------------------------------------------------------------------
# D3: Supervisor Support Moderation with model-set specific sample - MIDUS 2
# -----------------------------------------------------------------------------
m9_super_setspec_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                            B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_super)
m10_super_setspec_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                             B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_super)
m11_super_setspec_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                             B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_super)
m12_super_setspec_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                             B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_super)

# -----------------------------------------------------------------------------
# D4: Coworker Support Moderation with model-set specific sample - MIDUS 2
# -----------------------------------------------------------------------------
m13_cowork_setspec_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_cowork)
m14_cowork_setspec_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_cowork)
m15_cowork_setspec_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_cowork)
m16_cowork_setspec_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_cowork)

# -----------------------------------------------------------------------------
# D5: Spousal Support Moderation with model-set specific sample - MIDUS 2
# -----------------------------------------------------------------------------
m17_spouse_setspec_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_spouse)
m18_spouse_setspec_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_spouse)
m19_spouse_setspec_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_spouse)
m20_spouse_setspec_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                              B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_spouse)

# -----------------------------------------------------------------------------
# D1: Main Effects with model-set specific sample - MIDUS 3
# -----------------------------------------------------------------------------
m1_main_setspec_m3 <- lm(C1SNEGWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_main)
m2_main_setspec_m3 <- lm(C1SNEGFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_main)
m3_main_setspec_m3 <- lm(C1SPOSWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_main)
m4_main_setspec_m3 <- lm(C1SPOSFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                           C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_main)

# -----------------------------------------------------------------------------
# D2: Gender Moderation with model-set specific sample - MIDUS 3
# -----------------------------------------------------------------------------
m5_gender_setspec_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                             C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_main)
m6_gender_setspec_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                             C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_main)
m7_gender_setspec_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                             C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_main)
m8_gender_setspec_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                             C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_main)

# -----------------------------------------------------------------------------
# D3: Supervisor Support Moderation with model-set specific sample - MIDUS 3
# -----------------------------------------------------------------------------
m9_super_setspec_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                            C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_super)
m10_super_setspec_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                             C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_super)
m11_super_setspec_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                             C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_super)
m12_super_setspec_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                             C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_super)

# -----------------------------------------------------------------------------
# D4: Coworker Support Moderation with model-set specific sample - MIDUS 3
# -----------------------------------------------------------------------------
m13_cowork_setspec_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_cowork)
m14_cowork_setspec_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_cowork)
m15_cowork_setspec_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_cowork)
m16_cowork_setspec_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_cowork)

# -----------------------------------------------------------------------------
# D5: Spousal Support Moderation with model-set specific sample - MIDUS 3
# -----------------------------------------------------------------------------
m17_spouse_setspec_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_spouse)
m18_spouse_setspec_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_spouse)
m19_spouse_setspec_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_spouse)
m20_spouse_setspec_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 + 
                              C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_spouse)

# =============================================================================
# ARM (A): COVARIATE VARIATIONS - MIDUS 2
# =============================================================================

# A1: No covariates
m1_nocov_m2 <- lm(B1SNEGWF ~ B3TEFZ1, data = df_m2_analytic)
m2_nocov_m2 <- lm(B1SNEGFW ~ B3TEFZ1, data = df_m2_analytic)
m3_nocov_m2 <- lm(B1SPOSWF ~ B3TEFZ1, data = df_m2_analytic)
m4_nocov_m2 <- lm(B1SPOSFW ~ B3TEFZ1, data = df_m2_analytic)

# A2: Demographics only (no job/family demands)
m1_demog_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_analytic)
m2_demog_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_analytic)
m3_demog_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_analytic)
m4_demog_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1,
                  data = df_m2_analytic)

# A3: Work hours instead of self-report job demands
# REMOVED - work hours variable not available in current data

# A4: Education only (no income) as SES
m1_edu_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                  B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m2_edu_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                  B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m3_edu_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                  B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m4_edu_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 + 
                  B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)


# =============================================================================
# ARM (A): COVARIATE VARIATIONS - MIDUS 3
# =============================================================================

# A1: No covariates
m1_nocov_m3 <- lm(C1SNEGWF ~ C3TEF, data = df_m3_analytic)
m2_nocov_m3 <- lm(C1SNEGFW ~ C3TEF, data = df_m3_analytic)
m3_nocov_m3 <- lm(C1SPOSWF ~ C3TEF, data = df_m3_analytic)
m4_nocov_m3 <- lm(C1SPOSFW ~ C3TEF, data = df_m3_analytic)

# A2: Demographics only
m1_demog_m3 <- lm(C1SNEGWF ~ C3TEF + C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1,
                  data = df_m3_analytic)
m2_demog_m3 <- lm(C1SNEGFW ~ C3TEF + C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1,
                  data = df_m3_analytic)
m3_demog_m3 <- lm(C1SPOSWF ~ C3TEF + C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1,
                  data = df_m3_analytic)
m4_demog_m3 <- lm(C1SPOSFW ~ C3TEF + C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1,
                  data = df_m3_analytic)

# A3: Work hours instead of self-report job demands
# REMOVED - work hours variable not available in current data

# A4: Education only (no income)
m1_edu_m3 <- lm(C1SNEGWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                  C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m2_edu_m3 <- lm(C1SNEGFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                  C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m3_edu_m3 <- lm(C1SPOSWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                  C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m4_edu_m3 <- lm(C1SPOSFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                  C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)


# =============================================================================
# ARM (A2): COVARIATE-GROUP DROP-OUT SERIES
# =============================================================================
# Three specifications per wave that omit one covariate group while keeping
# the rest. Designed to test whether any one covariate group is driving the
# null EF effects (overcontrol-bias concern).
#
#   "All covariates except job demands and family demands"            drops job demands + family demands
#   "All covariates except household variables (income, size)"          drops household income + household
#                                                                       size
#   "All covariates except respondent demographics (age, gender, education)"  drops age + gender + education

# --- MIDUS 2 ---

# Drop demands (job + family); keep household + respondent covariates
m1_nodem_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth +
                    B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m2_nodem_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth +
                    B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m3_nodem_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth +
                    B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m4_nodem_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1STINC1 + B1PHHSIZE_oth +
                    B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)

# Drop household (income + size + other household members); keep demands +
# respondent covariates
m1_nohh_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m2_nohh_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m3_nohh_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m4_nohh_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)

# Drop respondent covariates (age + gender + education); keep demands + household
m1_noresp_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth, data = df_m2_analytic)
m2_noresp_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth, data = df_m2_analytic)
m3_noresp_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth, data = df_m2_analytic)
m4_noresp_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth, data = df_m2_analytic)

# --- MIDUS 3 ---

# Drop demands
m1_nodem_m3 <- lm(C1SNEGWF ~ C3TEF + C1STINC + C1PHHSIZE_oth +
                    C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m2_nodem_m3 <- lm(C1SNEGFW ~ C3TEF + C1STINC + C1PHHSIZE_oth +
                    C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m3_nodem_m3 <- lm(C1SPOSWF ~ C3TEF + C1STINC + C1PHHSIZE_oth +
                    C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m4_nodem_m3 <- lm(C1SPOSFW ~ C3TEF + C1STINC + C1PHHSIZE_oth +
                    C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)

# Drop household (income + size + other household members)
m1_nohh_m3 <- lm(C1SNEGWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m2_nohh_m3 <- lm(C1SNEGFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m3_nohh_m3 <- lm(C1SPOSWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m4_nohh_m3 <- lm(C1SPOSFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)

# Drop respondent covariates
m1_noresp_m3 <- lm(C1SNEGWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth, data = df_m3_analytic)
m2_noresp_m3 <- lm(C1SNEGFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth, data = df_m3_analytic)
m3_noresp_m3 <- lm(C1SPOSWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth, data = df_m3_analytic)
m4_noresp_m3 <- lm(C1SPOSFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth, data = df_m3_analytic)


# =============================================================================
# ARM (B): COVARIATES AS MODERATORS - MIDUS 2
# =============================================================================

# B1: Age x EF
m1_age_mod_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1PAGE_M2 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PRSEX + B1PB1, data = df_m2_analytic)
m2_age_mod_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1PAGE_M2 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PRSEX + B1PB1, data = df_m2_analytic)
m3_age_mod_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1PAGE_M2 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PRSEX + B1PB1, data = df_m2_analytic)
m4_age_mod_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1PAGE_M2 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PRSEX + B1PB1, data = df_m2_analytic)

# B2: Education x EF
m1_edu_mod_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1PB1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX, data = df_m2_analytic)
m2_edu_mod_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1PB1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX, data = df_m2_analytic)
m3_edu_mod_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1PB1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX, data = df_m2_analytic)
m4_edu_mod_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1PB1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX, data = df_m2_analytic)

# B3: Income x EF
m1_inc_mod_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1STINC1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m2_inc_mod_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1STINC1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m3_inc_mod_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1STINC1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m4_inc_mod_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1STINC1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                      B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)

# B4: Job Demands x EF
m1_jd_mod_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SJCDS_c + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m2_jd_mod_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCDS_c + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m3_jd_mod_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SJCDS_c + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m4_jd_mod_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SJCDS_c + FAM_DEMANDS_M2 +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)

# B5: Family Demands x EF
m1_fd_mod_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * FAM_DEMANDS_M2_c + B1SJCDS_sum +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m2_fd_mod_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * FAM_DEMANDS_M2_c + B1SJCDS_sum +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m3_fd_mod_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * FAM_DEMANDS_M2_c + B1SJCDS_sum +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)
m4_fd_mod_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * FAM_DEMANDS_M2_c + B1SJCDS_sum +
                     B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_analytic)


# =============================================================================
# ARM (B): COVARIATES AS MODERATORS - MIDUS 3
# =============================================================================

# B1: Age x EF
m1_age_mod_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1PRAGE + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRSEX + C1PB1, data = df_m3_analytic)
m2_age_mod_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1PRAGE + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRSEX + C1PB1, data = df_m3_analytic)
m3_age_mod_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1PRAGE + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRSEX + C1PB1, data = df_m3_analytic)
m4_age_mod_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1PRAGE + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRSEX + C1PB1, data = df_m3_analytic)

# B2: Education x EF
m1_edu_mod_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1PB1 + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX, data = df_m3_analytic)
m2_edu_mod_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1PB1 + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX, data = df_m3_analytic)
m3_edu_mod_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1PB1 + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX, data = df_m3_analytic)
m4_edu_mod_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1PB1 + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX, data = df_m3_analytic)

# B3: Income x EF
m1_inc_mod_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1STINC + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m2_inc_mod_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1STINC + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m3_inc_mod_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1STINC + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m4_inc_mod_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1STINC + C1SJCDS_sum + FAM_DEMANDS_M3 +
                      C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)

# B4: Job Demands x EF
m1_jd_mod_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SJCDS_c + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m2_jd_mod_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SJCDS_c + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m3_jd_mod_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SJCDS_c + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m4_jd_mod_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SJCDS_c + FAM_DEMANDS_M3 +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)

# B5: Family Demands x EF
m1_fd_mod_m3 <- lm(C1SNEGWF ~ C3TEF_c * FAM_DEMANDS_M3_c + C1SJCDS_sum +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m2_fd_mod_m3 <- lm(C1SNEGFW ~ C3TEF_c * FAM_DEMANDS_M3_c + C1SJCDS_sum +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m3_fd_mod_m3 <- lm(C1SPOSWF ~ C3TEF_c * FAM_DEMANDS_M3_c + C1SJCDS_sum +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)
m4_fd_mod_m3 <- lm(C1SPOSFW ~ C3TEF_c * FAM_DEMANDS_M3_c + C1SJCDS_sum +
                     C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_analytic)


# =============================================================================
# ARM (C): ALTERNATIVE INCLUSION CRITERIA — DUAL-EARNER COUPLES
# =============================================================================
# Main sample is now respondent-employed only.
# This arm tests robustness using dual-earner couples (respondent + spouse employed).
# df_m2_dual_earner / df_m3_dual_earner created in 02_midus_scoring.R.

# --- Consistent sample for dual-earner arm (complete cases on all regression vars) ---
all_vars_m2 <- c("B1SNEGWF", "B1SNEGFW", "B1SPOSWF", "B1SPOSFW",
                 "B3TEFZ1", "B1SJCDS_sum", "FAM_DEMANDS_M2",
                 "B1STINC1", "B1PHHSIZE_oth", "B1PAGE_M2", "B1PRSEX", "B1PB1",
                 "B1SJCSS_sum", "B1SJCCS_sum", "B1SSPEMP_sum")
all_vars_m3 <- c("C1SNEGWF", "C1SNEGFW", "C1SPOSWF", "C1SPOSFW",
                 "C3TEF", "C1SJCDS_sum", "FAM_DEMANDS_M3",
                 "C1STINC", "C1PHHSIZE_oth", "C1PRAGE", "C1PRSEX", "C1PB1",
                 "C1SJCSS_sum", "C1SJCCS_sum", "C1SSPEMP_sum")

df_m2_dual_all <- df_m2_dual_earner %>% filter(complete.cases(across(all_of(all_vars_m2))))
df_m3_dual_all <- df_m3_dual_earner %>% filter(complete.cases(across(all_of(all_vars_m3))))

cat("=== Dual-Earner Arm C Sample Sizes ===\n")
cat("MIDUS 2 dual-earner (complete):", nrow(df_m2_dual_all), "\n")
cat("MIDUS 3 dual-earner (complete):", nrow(df_m3_dual_all), "\n")

# --- C: Main Effects (4 models) — MIDUS 2 ---
m1_dual_m2 <- lm(B1SNEGWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_dual_all)
m2_dual_m2 <- lm(B1SNEGFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_dual_all)
m3_dual_m2 <- lm(B1SPOSWF ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_dual_all)
m4_dual_m2 <- lm(B1SPOSFW ~ B3TEFZ1 + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PRSEX + B1PB1, data = df_m2_dual_all)

# --- C: Gender Moderation (4 models) — MIDUS 2 ---
m5_dual_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m6_dual_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m7_dual_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m8_dual_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                   B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)

# --- C: Supervisor Support Moderation (4 models) — MIDUS 2 ---
m9_dual_m2  <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m10_dual_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m11_dual_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m12_dual_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SJCSS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)

# --- C: Coworker Support Moderation (4 models) — MIDUS 2 ---
m13_dual_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m14_dual_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m15_dual_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m16_dual_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SJCCS_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)

# --- C: Spousal Support Moderation (4 models) — MIDUS 2 ---
m17_dual_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m18_dual_m2 <- lm(B1SNEGWF ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m19_dual_m2 <- lm(B1SPOSFW ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)
m20_dual_m2 <- lm(B1SPOSWF ~ B3TEFZ1_c * B1SSPEMP_c + B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                    B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1, data = df_m2_dual_all)

# --- C: Main Effects (4 models) — MIDUS 3 ---
m1_dual_m3 <- lm(C1SNEGWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_dual_all)
m2_dual_m3 <- lm(C1SNEGFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_dual_all)
m3_dual_m3 <- lm(C1SPOSWF ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_dual_all)
m4_dual_m3 <- lm(C1SPOSFW ~ C3TEF + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PRSEX + C1PB1, data = df_m3_dual_all)

# --- C: Gender Moderation (4 models) — MIDUS 3 ---
m5_dual_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m6_dual_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m7_dual_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m8_dual_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                   C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)

# --- C: Supervisor Support Moderation (4 models) — MIDUS 3 ---
m9_dual_m3  <- lm(C1SNEGFW ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m10_dual_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m11_dual_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m12_dual_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SJCSS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)

# --- C: Coworker Support Moderation (4 models) — MIDUS 3 ---
m13_dual_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m14_dual_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m15_dual_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m16_dual_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SJCCS_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)

# --- C: Spousal Support Moderation (4 models) — MIDUS 3 ---
m17_dual_m3 <- lm(C1SNEGFW ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m18_dual_m3 <- lm(C1SNEGWF ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m19_dual_m3 <- lm(C1SPOSFW ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)
m20_dual_m3 <- lm(C1SPOSWF ~ C3TEF_c * C1SSPEMP_c + C1PRSEX + C1SJCDS_sum + FAM_DEMANDS_M3 +
                    C1STINC + C1PHHSIZE_oth + C1PRAGE + C1PB1, data = df_m3_dual_all)

# --- Sanity checks: ARM C ---
stopifnot(
  # Dual-earner samples are strict subsets of analytic
  "dual_all not < analytic M2"  = nrow(df_m2_dual_all) < nrow(df_m2_analytic),
  "dual_all not < analytic M3"  = nrow(df_m3_dual_all) < nrow(df_m3_analytic),
  # Dual-earner models are lm objects and use the dual-earner N
  "m1_dual_m2 not lm"          = inherits(m1_dual_m2, "lm"),
  "m20_dual_m3 not lm"         = inherits(m20_dual_m3, "lm"),
  "ARM C M2 N mismatch"        = nobs(m1_dual_m2) == nobs(m20_dual_m2),
  "ARM C M3 N mismatch"        = nobs(m1_dual_m3) == nobs(m20_dual_m3),
  # Model-set specific samples are >= all-models consistent sample
  "setspec main < all M2"      = nrow(df_m2_main) >= nrow(df_m2_all),
  "setspec main < all M3"      = nrow(df_m3_main) >= nrow(df_m3_all)
)
cat("04_multiverse ARM checks passed.\n")

# =============================================================================
# RESULTS EXTRACTION
# =============================================================================

# MIDUS 2 Results
results_m2 <- bind_rows(
  # Arm A: Covariate variations
  extract_results(m1_nocov_m2, "NEGWF", "No covariates (EF only)"),
  extract_results(m2_nocov_m2, "NEGFW", "No covariates (EF only)"),
  extract_results(m3_nocov_m2, "POSWF", "No covariates (EF only)"),
  extract_results(m4_nocov_m2, "POSFW", "No covariates (EF only)"),
  
  extract_results(m1_demog_m2, "NEGWF", "Demographic covariates only (no work or family demand variables)"),
  extract_results(m2_demog_m2, "NEGFW", "Demographic covariates only (no work or family demand variables)"),
  extract_results(m3_demog_m2, "POSWF", "Demographic covariates only (no work or family demand variables)"),
  extract_results(m4_demog_m2, "POSFW", "Demographic covariates only (no work or family demand variables)"),
  
  extract_results(m1_edu_m2, "NEGWF", "Education-only SES indicator (income excluded)"),
  extract_results(m2_edu_m2, "NEGFW", "Education-only SES indicator (income excluded)"),
  extract_results(m3_edu_m2, "POSWF", "Education-only SES indicator (income excluded)"),
  extract_results(m4_edu_m2, "POSFW", "Education-only SES indicator (income excluded)"),

  # Arm A2: Covariate-group drop-out series
  extract_results(m1_nodem_m2, "NEGWF", "All covariates except job demands and family demands"),
  extract_results(m2_nodem_m2, "NEGFW", "All covariates except job demands and family demands"),
  extract_results(m3_nodem_m2, "POSWF", "All covariates except job demands and family demands"),
  extract_results(m4_nodem_m2, "POSFW", "All covariates except job demands and family demands"),

  extract_results(m1_nohh_m2, "NEGWF", "All covariates except household variables (income, size)"),
  extract_results(m2_nohh_m2, "NEGFW", "All covariates except household variables (income, size)"),
  extract_results(m3_nohh_m2, "POSWF", "All covariates except household variables (income, size)"),
  extract_results(m4_nohh_m2, "POSFW", "All covariates except household variables (income, size)"),

  extract_results(m1_noresp_m2, "NEGWF", "All covariates except respondent demographics (age, gender, education)"),
  extract_results(m2_noresp_m2, "NEGFW", "All covariates except respondent demographics (age, gender, education)"),
  extract_results(m3_noresp_m2, "POSWF", "All covariates except respondent demographics (age, gender, education)"),
  extract_results(m4_noresp_m2, "POSFW", "All covariates except respondent demographics (age, gender, education)"),

  # Arm B: Covariate moderators
  extract_results(m1_age_mod_m2, "NEGWF", "Full covariate model with EF × Age interaction"),
  extract_results(m2_age_mod_m2, "NEGFW", "Full covariate model with EF × Age interaction"),
  extract_results(m3_age_mod_m2, "POSWF", "Full covariate model with EF × Age interaction"),
  extract_results(m4_age_mod_m2, "POSFW", "Full covariate model with EF × Age interaction"),

  extract_results(m1_edu_mod_m2, "NEGWF", "Full covariate model with EF × Education interaction"),
  extract_results(m2_edu_mod_m2, "NEGFW", "Full covariate model with EF × Education interaction"),
  extract_results(m3_edu_mod_m2, "POSWF", "Full covariate model with EF × Education interaction"),
  extract_results(m4_edu_mod_m2, "POSFW", "Full covariate model with EF × Education interaction"),

  extract_results(m1_inc_mod_m2, "NEGWF", "Full covariate model with EF × Income interaction"),
  extract_results(m2_inc_mod_m2, "NEGFW", "Full covariate model with EF × Income interaction"),
  extract_results(m3_inc_mod_m2, "POSWF", "Full covariate model with EF × Income interaction"),
  extract_results(m4_inc_mod_m2, "POSFW", "Full covariate model with EF × Income interaction"),

  extract_results(m1_jd_mod_m2, "NEGWF", "Full covariate model with EF × Job Demands interaction"),
  extract_results(m2_jd_mod_m2, "NEGFW", "Full covariate model with EF × Job Demands interaction"),
  extract_results(m3_jd_mod_m2, "POSWF", "Full covariate model with EF × Job Demands interaction"),
  extract_results(m4_jd_mod_m2, "POSFW", "Full covariate model with EF × Job Demands interaction"),

  extract_results(m1_fd_mod_m2, "NEGWF", "Full covariate model with EF × Family Demands interaction"),
  extract_results(m2_fd_mod_m2, "NEGFW", "Full covariate model with EF × Family Demands interaction"),
  extract_results(m3_fd_mod_m2, "POSWF", "Full covariate model with EF × Family Demands interaction"),
  extract_results(m4_fd_mod_m2, "POSFW", "Full covariate model with EF × Family Demands interaction"),
  
  # Arm C: Dual-earner inclusion criteria (all 20 models)
  extract_results(m1_dual_m2, "NEGWF", "Dual-earner subsample: main effects"),
  extract_results(m2_dual_m2, "NEGFW", "Dual-earner subsample: main effects"),
  extract_results(m3_dual_m2, "POSWF", "Dual-earner subsample: main effects"),
  extract_results(m4_dual_m2, "POSFW", "Dual-earner subsample: main effects"),

  extract_results(m5_dual_m2, "NEGFW", "Dual-earner subsample: EF × Gender moderation"),
  extract_results(m6_dual_m2, "NEGWF", "Dual-earner subsample: EF × Gender moderation"),
  extract_results(m7_dual_m2, "POSFW", "Dual-earner subsample: EF × Gender moderation"),
  extract_results(m8_dual_m2, "POSWF", "Dual-earner subsample: EF × Gender moderation"),

  extract_results(m9_dual_m2,  "NEGFW", "Dual-earner subsample: EF × Supervisor support moderation"),
  extract_results(m10_dual_m2, "NEGWF", "Dual-earner subsample: EF × Supervisor support moderation"),
  extract_results(m11_dual_m2, "POSFW", "Dual-earner subsample: EF × Supervisor support moderation"),
  extract_results(m12_dual_m2, "POSWF", "Dual-earner subsample: EF × Supervisor support moderation"),

  extract_results(m13_dual_m2, "NEGFW", "Dual-earner subsample: EF × Coworker support moderation"),
  extract_results(m14_dual_m2, "NEGWF", "Dual-earner subsample: EF × Coworker support moderation"),
  extract_results(m15_dual_m2, "POSFW", "Dual-earner subsample: EF × Coworker support moderation"),
  extract_results(m16_dual_m2, "POSWF", "Dual-earner subsample: EF × Coworker support moderation"),

  extract_results(m17_dual_m2, "NEGFW", "Dual-earner subsample: EF × Spousal support moderation"),
  extract_results(m18_dual_m2, "NEGWF", "Dual-earner subsample: EF × Spousal support moderation"),
  extract_results(m19_dual_m2, "POSFW", "Dual-earner subsample: EF × Spousal support moderation"),
  extract_results(m20_dual_m2, "POSWF", "Dual-earner subsample: EF × Spousal support moderation")
) %>%
  mutate(wave = "MIDUS 2")

# MIDUS 3 Results
results_m3 <- bind_rows(
  # Arm A: Covariate variations
  extract_results(m1_nocov_m3, "NEGWF", "No covariates (EF only)"),
  extract_results(m2_nocov_m3, "NEGFW", "No covariates (EF only)"),
  extract_results(m3_nocov_m3, "POSWF", "No covariates (EF only)"),
  extract_results(m4_nocov_m3, "POSFW", "No covariates (EF only)"),
  
  extract_results(m1_demog_m3, "NEGWF", "Demographic covariates only (no work or family demand variables)"),
  extract_results(m2_demog_m3, "NEGFW", "Demographic covariates only (no work or family demand variables)"),
  extract_results(m3_demog_m3, "POSWF", "Demographic covariates only (no work or family demand variables)"),
  extract_results(m4_demog_m3, "POSFW", "Demographic covariates only (no work or family demand variables)"),
  
  extract_results(m1_edu_m3, "NEGWF", "Education-only SES indicator (income excluded)"),
  extract_results(m2_edu_m3, "NEGFW", "Education-only SES indicator (income excluded)"),
  extract_results(m3_edu_m3, "POSWF", "Education-only SES indicator (income excluded)"),
  extract_results(m4_edu_m3, "POSFW", "Education-only SES indicator (income excluded)"),

  # Arm A2: Covariate-group drop-out series
  extract_results(m1_nodem_m3, "NEGWF", "All covariates except job demands and family demands"),
  extract_results(m2_nodem_m3, "NEGFW", "All covariates except job demands and family demands"),
  extract_results(m3_nodem_m3, "POSWF", "All covariates except job demands and family demands"),
  extract_results(m4_nodem_m3, "POSFW", "All covariates except job demands and family demands"),

  extract_results(m1_nohh_m3, "NEGWF", "All covariates except household variables (income, size)"),
  extract_results(m2_nohh_m3, "NEGFW", "All covariates except household variables (income, size)"),
  extract_results(m3_nohh_m3, "POSWF", "All covariates except household variables (income, size)"),
  extract_results(m4_nohh_m3, "POSFW", "All covariates except household variables (income, size)"),

  extract_results(m1_noresp_m3, "NEGWF", "All covariates except respondent demographics (age, gender, education)"),
  extract_results(m2_noresp_m3, "NEGFW", "All covariates except respondent demographics (age, gender, education)"),
  extract_results(m3_noresp_m3, "POSWF", "All covariates except respondent demographics (age, gender, education)"),
  extract_results(m4_noresp_m3, "POSFW", "All covariates except respondent demographics (age, gender, education)"),

  # Arm B: Covariate moderators
  extract_results(m1_age_mod_m3, "NEGWF", "Full covariate model with EF × Age interaction"),
  extract_results(m2_age_mod_m3, "NEGFW", "Full covariate model with EF × Age interaction"),
  extract_results(m3_age_mod_m3, "POSWF", "Full covariate model with EF × Age interaction"),
  extract_results(m4_age_mod_m3, "POSFW", "Full covariate model with EF × Age interaction"),

  extract_results(m1_edu_mod_m3, "NEGWF", "Full covariate model with EF × Education interaction"),
  extract_results(m2_edu_mod_m3, "NEGFW", "Full covariate model with EF × Education interaction"),
  extract_results(m3_edu_mod_m3, "POSWF", "Full covariate model with EF × Education interaction"),
  extract_results(m4_edu_mod_m3, "POSFW", "Full covariate model with EF × Education interaction"),

  extract_results(m1_inc_mod_m3, "NEGWF", "Full covariate model with EF × Income interaction"),
  extract_results(m2_inc_mod_m3, "NEGFW", "Full covariate model with EF × Income interaction"),
  extract_results(m3_inc_mod_m3, "POSWF", "Full covariate model with EF × Income interaction"),
  extract_results(m4_inc_mod_m3, "POSFW", "Full covariate model with EF × Income interaction"),

  extract_results(m1_jd_mod_m3, "NEGWF", "Full covariate model with EF × Job Demands interaction"),
  extract_results(m2_jd_mod_m3, "NEGFW", "Full covariate model with EF × Job Demands interaction"),
  extract_results(m3_jd_mod_m3, "POSWF", "Full covariate model with EF × Job Demands interaction"),
  extract_results(m4_jd_mod_m3, "POSFW", "Full covariate model with EF × Job Demands interaction"),

  extract_results(m1_fd_mod_m3, "NEGWF", "Full covariate model with EF × Family Demands interaction"),
  extract_results(m2_fd_mod_m3, "NEGFW", "Full covariate model with EF × Family Demands interaction"),
  extract_results(m3_fd_mod_m3, "POSWF", "Full covariate model with EF × Family Demands interaction"),
  extract_results(m4_fd_mod_m3, "POSFW", "Full covariate model with EF × Family Demands interaction"),
  
  # Arm C: Dual-earner inclusion criteria (all 20 models)
  extract_results(m1_dual_m3, "NEGWF", "Dual-earner subsample: main effects"),
  extract_results(m2_dual_m3, "NEGFW", "Dual-earner subsample: main effects"),
  extract_results(m3_dual_m3, "POSWF", "Dual-earner subsample: main effects"),
  extract_results(m4_dual_m3, "POSFW", "Dual-earner subsample: main effects"),

  extract_results(m5_dual_m3, "NEGFW", "Dual-earner subsample: EF × Gender moderation"),
  extract_results(m6_dual_m3, "NEGWF", "Dual-earner subsample: EF × Gender moderation"),
  extract_results(m7_dual_m3, "POSFW", "Dual-earner subsample: EF × Gender moderation"),
  extract_results(m8_dual_m3, "POSWF", "Dual-earner subsample: EF × Gender moderation"),

  extract_results(m9_dual_m3,  "NEGFW", "Dual-earner subsample: EF × Supervisor support moderation"),
  extract_results(m10_dual_m3, "NEGWF", "Dual-earner subsample: EF × Supervisor support moderation"),
  extract_results(m11_dual_m3, "POSFW", "Dual-earner subsample: EF × Supervisor support moderation"),
  extract_results(m12_dual_m3, "POSWF", "Dual-earner subsample: EF × Supervisor support moderation"),

  extract_results(m13_dual_m3, "NEGFW", "Dual-earner subsample: EF × Coworker support moderation"),
  extract_results(m14_dual_m3, "NEGWF", "Dual-earner subsample: EF × Coworker support moderation"),
  extract_results(m15_dual_m3, "POSFW", "Dual-earner subsample: EF × Coworker support moderation"),
  extract_results(m16_dual_m3, "POSWF", "Dual-earner subsample: EF × Coworker support moderation"),

  extract_results(m17_dual_m3, "NEGFW", "Dual-earner subsample: EF × Spousal support moderation"),
  extract_results(m18_dual_m3, "NEGWF", "Dual-earner subsample: EF × Spousal support moderation"),
  extract_results(m19_dual_m3, "POSFW", "Dual-earner subsample: EF × Spousal support moderation"),
  extract_results(m20_dual_m3, "POSWF", "Dual-earner subsample: EF × Spousal support moderation")
) %>%
  mutate(wave = "MIDUS 3")

# Combined results
results_all <- bind_rows(results_m2, results_m3)

# --- Sanity checks: results extraction ---
stopifnot(
  "results_m2 is empty"          = nrow(results_m2) > 0,
  "results_m3 is empty"          = nrow(results_m3) > 0,
  # Dual-earner specs present in extraction
  "no Dual-earner specs in M2"   = any(grepl("Dual-earner subsample: main effects", results_m2$specification)),
  "no Dual-earner specs in M3"   = any(grepl("Dual-earner subsample: main effects", results_m3$specification)),
  # Required columns exist
  "missing estimate col"         = "estimate" %in% names(results_all),
  "missing specification col"    = "specification" %in% names(results_all),
  "missing wave col"             = "wave" %in% names(results_all),
  # Both waves present
  "only one wave in results"     = length(unique(results_all$wave)) == 2
)
cat("04_multiverse results checks passed.\n")

# =============================================================================
# SPECIFICATION CURVE VISUALIZATION
# =============================================================================

create_spec_curve <- function(results_df) {
  ggplot(results_df, aes(x = reorder(specification, estimate), y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    facet_wrap(~outcome) +
    labs(
      title = "Multiverse Analysis: EF Effect Across Specifications",
      x = "Specification",
      y = "Standardized Coefficient (95% CI)"
    ) +
    theme_minimal()
}

# =============================================================================
# CREATE PLOTS
# =============================================================================

# MIDUS 2 specification curve
p_m2 <- create_spec_curve(results_m2) + 
  ggtitle("MIDUS 2: EF Effect Across Specifications")

# MIDUS 3 specification curve  
p_m3 <- create_spec_curve(results_m3) + 
  ggtitle("MIDUS 3: EF Effect Across Specifications")

# Combined plot by wave
p_combined <- ggplot(results_all, aes(x = reorder(specification, estimate), y = estimate, color = wave)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  facet_wrap(~outcome) +
  labs(
    title = "Multiverse Analysis: EF Effect Across Specifications",
    x = "Specification",
    y = "Standardized Coefficient (95% CI)",
    color = "Wave"
  ) +
  theme_minimal()

# Print plots
print(p_m2)
print(p_m3)
print(p_combined)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

# Count significant effects by outcome
sig_summary <- results_all %>%
  mutate(significant = p.value < .05) %>%
  group_by(wave, outcome) %>%
  summarise(
    n_specs = n(),
    n_sig = sum(significant),
    pct_sig = mean(significant) * 100,
    median_est = median(estimate),
    range_est = paste0("[", round(min(estimate), 3), ", ", round(max(estimate), 3), "]"),
    .groups = "drop"
  )

print(sig_summary)

# =============================================================================
# CROSS-WAVE REPLICABILITY (OSC, 2015)
# =============================================================================
# Compare EF-related coefficients across MIDUS 2 (W1) and MIDUS 3 (W2)
# using the Open Science Collaboration (2015) three-step criteria.

# Supplementary term labels for Arm B interaction terms not in 03's var_labels
mv_extra_labels <- c(
  "B3TEFZ1:B1PAGE_M2"     = "EF x Age",
  "B3TEFZ1:B1PB1"         = "EF x Education",
  "B3TEFZ1:B1STINC1"      = "EF x Income",
  "B3TEFZ1:B1SJCDS_sum"   = "EF x Job Demands",
  "B3TEFZ1:FAM_DEMANDS_M2" = "EF x Family Demands",
  "C3TEF:C1PRAGE"          = "EF x Age",
  "C3TEF:C1PB1"            = "EF x Education",
  "C3TEF:C1STINC"          = "EF x Income",
  "C3TEF:C1SJCDS_sum"      = "EF x Job Demands",
  "C3TEF:FAM_DEMANDS_M3"   = "EF x Family Demands"
)

label_terms_mv <- function(term) {
  ifelse(term %in% names(mv_extra_labels),
         mv_extra_labels[term],
         label_terms(term))
}

# Normalize terms and pair across waves
mv_w1 <- results_m2 %>%
  mutate(term_label = label_terms_mv(term)) %>%
  select(specification, outcome, term_label, term_w1 = term,
         est_w1 = estimate, se_w1 = std.error, p_w1 = p.value)

mv_w2 <- results_m3 %>%
  mutate(term_label = label_terms_mv(term)) %>%
  select(specification, outcome, term_label, term_w2 = term,
         est_w2 = estimate, se_w2 = std.error, p_w2 = p.value)

mv_paired_df <- inner_join(mv_w1, mv_w2,
                           by = c("specification", "outcome", "term_label")) %>%
  mutate(pair_id = paste0(specification, " / ", outcome))

# Compute OSC (2015) criteria
mv_paired_df <- mv_paired_df %>%
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
n_sig_w1_mv   <- sum(mv_paired_df$sig_w1)
n_both_sig_mv <- sum(mv_paired_df$sig_w1 & mv_paired_df$sig_w2)

cat("\n=== OSC (2015) Replicability: Multiverse ===\n")
if (n_sig_w1_mv > 0) {
  cat(sprintf("Step 1 - Same sign:          %d/%d (%.1f%%)\n",
              sum(mv_paired_df$replicated_sign), n_sig_w1_mv,
              100 * sum(mv_paired_df$replicated_sign) / n_sig_w1_mv))
  cat(sprintf("Step 2 - Sign + significant: %d/%d (%.1f%%)\n",
              sum(mv_paired_df$replicated_sig), n_sig_w1_mv,
              100 * sum(mv_paired_df$replicated_sig) / n_sig_w1_mv))
} else {
  cat("Steps 1-2: N/A (no W1 effects significant)\n")
}
if (n_both_sig_mv > 0) {
  cat(sprintf("Step 3 - Overlapping CIs:    %d/%d (%.1f%%)\n",
              sum(mv_paired_df$ci_overlap), n_both_sig_mv,
              100 * sum(mv_paired_df$ci_overlap) / n_both_sig_mv))
} else {
  cat("Step 3: N/A (no effects significant in both waves)\n")
}

# Store for downstream APA table generation (07_midus_apa_tables.R)
mv_replicability <- mv_paired_df