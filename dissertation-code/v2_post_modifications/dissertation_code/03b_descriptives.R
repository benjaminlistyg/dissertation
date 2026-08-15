# =============================================================================
# MIDUS DISSERTATION - DESCRIPTIVE STATISTICS & CORRELATION MATRICES
# Runs after 03_regressions.R (requires df_m2_all, df_m3_all)
# Output: stores descriptives_data for APA table generation in 07
# =============================================================================

# =============================================================================
# VARIABLE LISTS & LABELS
# =============================================================================

# Analysis variables in presentation order:
#   DVs -> IV -> Covariates -> Moderators -> Demographics

desc_vars_m2 <- c(
  # Spillover DVs
  "B1SNEGWF", "B1SNEGFW", "B1SPOSWF", "B1SPOSFW",
  # Executive Functioning
  "B3TEFZ1",
  # Covariates
  "B1SJCDS_mean", "FAM_DEMANDS_M2", "B1STINC1", "B1PHHSIZE_oth",
  # Moderators
  "B1SJCSS_mean", "B1SJCCS_mean", "B1SSPEMP_mean",
  # Demographics
  "B1PAGE_M2", "B1PRSEX", "B1PB1"
)

desc_vars_m3 <- c(
  "C1SNEGWF", "C1SNEGFW", "C1SPOSWF", "C1SPOSFW",
  "C3TEF",
  "C1SJCDS_mean", "FAM_DEMANDS_M3", "C1STINC", "C1PHHSIZE_oth",
  "C1SJCSS_mean", "C1SJCCS_mean", "C1SSPEMP_mean",
  "C1PRAGE", "C1PRSEX", "C1PB1"
)

# Shared readable labels (same order as desc_vars above)
desc_labels <- c(
  "Neg Work-to-Family Spillover",
  "Neg Family-to-Work Spillover",
  "Pos Work-to-Family Spillover",
  "Pos Family-to-Work Spillover",
  "Executive Functioning",
  "Job Demands",
  "Family Demands",
  "Household Income",
  "Household Size",
  "Supervisor Support",
  "Coworker Support",
  "Spousal Support",
  "Age",
  "Gender",
  "Education"
)

# Item maps for multi-item scales (used to compute Cronbach's alpha).
# Single-item / count / demographic variables are absent (alpha N/A).
# psych::alpha(check.keys = TRUE) handles reverse-keyed items.
scale_items_m2 <- list(
  B1SNEGWF       = c("B1SF27A", "B1SF27B", "B1SF27C", "B1SF27D"),
  B1SNEGFW       = c("B1SF27I", "B1SF27J", "B1SF27K", "B1SF27L"),
  B1SPOSWF       = c("B1SF27E", "B1SF27F", "B1SF27G", "B1SF27H"),
  B1SPOSFW       = c("B1SF27M", "B1SF27N", "B1SF27O", "B1SF27P"),
  B1SJCDS_mean    = c("B1SF28A", "B1SF28J", "B1SF29A", "B1SF29C", "B1SF29D"),
  FAM_DEMANDS_M2 = c("B1SF33A_pomp", "B1SF33C_pomp", "B1SF33D_pomp",
                     "B1SJ2G_pomp",  "B1SL11G_pomp"),
  B1SJCSS_mean    = c("B1SF30C", "B1SF30D", "B1SF30E"),
  B1SJCCS_mean    = c("B1SF30A", "B1SF30B"),
  B1SSPEMP_mean   = c("B1SL11A", "B1SL11B", "B1SL11C",
                     "B1SL11D", "B1SL11E", "B1SL11F")
)

scale_items_m3 <- list(
  C1SNEGWF       = c("C1SF40A", "C1SF40B", "C1SF40C", "C1SF40D"),
  C1SNEGFW       = c("C1SF40I", "C1SF40J", "C1SF40K", "C1SF40L"),
  C1SPOSWF       = c("C1SF40E", "C1SF40F", "C1SF40G", "C1SF40H"),
  C1SPOSFW       = c("C1SF40M", "C1SF40N", "C1SF40O", "C1SF40P"),
  C1SJCDS_mean    = c("C1SF36A", "C1SF36J", "C1SF42A", "C1SF42C", "C1SF42D"),
  FAM_DEMANDS_M3 = c("C1SF43A_pomp", "C1SF43C_pomp", "C1SF43D_pomp",
                     "C1SJ4G_pomp",  "C1SL12G_pomp"),
  C1SJCSS_mean    = c("C1SF37C", "C1SF37D", "C1SF37E"),
  C1SJCCS_mean    = c("C1SF37A", "C1SF37B"),
  C1SSPEMP_mean   = c("C1SL12A", "C1SL12B", "C1SL12C",
                     "C1SL12D", "C1SL12E", "C1SL12F")
)

# =============================================================================
# STORE FOR APA TABLE GENERATION (07_midus_apa_tables.R)
# =============================================================================
# These objects are picked up by the exists()-gated block in 07.
#
# We pull the raw (pre-listwise) item columns from df_m{2,3}_analytic and
# then restrict by the regression-sample IDs captured in 03_regressions.R.
# That way the reliability sample is, by construction, the same respondents
# the lm() models used -- not a smaller listwise-on-raw-items subset.

stopifnot(
  "reg_ids_m2 missing from 03_regressions" = exists("reg_ids_m2"),
  "reg_ids_m3 missing from 03_regressions" = exists("reg_ids_m3"),
  "M2ID missing from df_m2_analytic" = "M2ID" %in% names(df_m2_analytic),
  "M2ID missing from df_m3_analytic" = "M2ID" %in% names(df_m3_analytic)
)

desc_data_m2 <- df_m2_analytic %>% filter(M2ID %in% reg_ids_m2)
desc_data_m3 <- df_m3_analytic %>% filter(M2ID %in% reg_ids_m3)

stopifnot(
  "desc_data_m2 size != regression sample" =
    nrow(desc_data_m2) == length(reg_ids_m2),
  "desc_data_m3 size != regression sample" =
    nrow(desc_data_m3) == length(reg_ids_m3),
  "desc_data_m2 IDs != regression IDs" =
    setequal(desc_data_m2$M2ID, reg_ids_m2),
  "desc_data_m3 IDs != regression IDs" =
    setequal(desc_data_m3$M2ID, reg_ids_m3)
)

descriptives_data <- list(
  m2 = list(data = desc_data_m2, ids = reg_ids_m2,
            vars = desc_vars_m2, labels = desc_labels,
            scale_items = scale_items_m2),
  m3 = list(data = desc_data_m3, ids = reg_ids_m3,
            vars = desc_vars_m3, labels = desc_labels,
            scale_items = scale_items_m3)
)
