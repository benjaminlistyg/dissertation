# =============================================================================
# MIDUS DISSERTATION ANALYSIS - SCORING & FILTERING
# Step 2: Score scales and apply sample restrictions
# =============================================================================

# POMP transform function
pomp <- function(x, min_val, max_val) {
  100 * (x - min_val) / (max_val - min_val)
}

# =============================================================================
# MIDUS 2 - SCORE SCALES
# =============================================================================

# Note: Using pre-scored MIDUS spillover scales (B1SPOSWF, B1SNEGWF, etc.)
# Individual items kept for CFA analyses

# -----------------------------------------------------------------------------
# JOB DEMANDS (Section F, Q28a, Q28j, Q29a, Q29c, Q29d)
# Coding: 1=All the time to 5=Never
# Reverse code (except Q29c which is already reverse-keyed)
# -----------------------------------------------------------------------------

df_m2$B1SJCDS_mean <- rowMeans(cbind(
  6 - df_m2$B1SF28A,   # reverse
  6 - df_m2$B1SF28J,   # reverse
  6 - df_m2$B1SF29A,   # reverse
  df_m2$B1SF29C,       # already reverse-keyed (R)
  6 - df_m2$B1SF29D    # reverse
), na.rm = FALSE)

# -----------------------------------------------------------------------------
# COWORKER SUPPORT (Section F, Q30a-b)
# Coding: 1=All the time to 5=Never
# Reverse code so higher = more support
# -----------------------------------------------------------------------------

df_m2$B1SJCCS_mean <- rowMeans(6 - df_m2[, c("B1SF30A", "B1SF30B")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SUPERVISOR SUPPORT (Section F, Q30c-e)
# Coding: 1=All the time to 5=Never
# Reverse code so higher = more support
# -----------------------------------------------------------------------------

df_m2$B1SJCSS_mean <- rowMeans(6 - df_m2[, c("B1SF30C", "B1SF30D", "B1SF30E")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SPOUSE/PARTNER SUPPORT (Section L, Q11a-f)
# Coding: 1=A lot to 4=Not at all
# Reverse code so higher = more support
# -----------------------------------------------------------------------------

df_m2$B1SSPEMP_mean <- rowMeans(5 - df_m2[, c("B1SL11A", "B1SL11B", "B1SL11C", 
                                             "B1SL11D", "B1SL11E", "B1SL11F")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# FAMILY DEMANDS (Composite - requires POMP transformation)
# Home demands: F33a, F33c (R), F33d (5-point scale)
# Family demands: J2g, L11g (4-point scale)
# -----------------------------------------------------------------------------

# Home demands items (reverse code so higher = more demands)
df_m2$B1SF33A_pomp <- pomp(6 - df_m2$B1SF33A, 1, 5)  # reverse
df_m2$B1SF33C_pomp <- pomp(df_m2$B1SF33C, 1, 5)       # already reverse-keyed
df_m2$B1SF33D_pomp <- pomp(6 - df_m2$B1SF33D, 1, 5)  # reverse

# Family demands items (reverse code so higher = more demands)
df_m2$B1SJ2G_pomp  <- pomp(5 - df_m2$B1SJ2G, 1, 4)   # reverse
df_m2$B1SL11G_pomp <- pomp(5 - df_m2$B1SL11G, 1, 4)  # reverse

# Composite family demands
df_m2$FAM_DEMANDS_M2 <- rowMeans(df_m2[, c("B1SF33A_pomp", "B1SF33C_pomp", "B1SF33D_pomp",
                                            "B1SJ2G_pomp", "B1SL11G_pomp")], na.rm = FALSE)


# =============================================================================
# MIDUS 3 - SCORE SCALES
# =============================================================================

# Note: Using pre-scored MIDUS spillover scales (C1SPOSWF, C1SNEGWF, etc.)

# -----------------------------------------------------------------------------
# JOB DEMANDS (Section F, Q36a, Q36j, Q42a, Q42c, Q42d)
# -----------------------------------------------------------------------------

df_m3$C1SJCDS_mean <- rowMeans(cbind(
  6 - df_m3$C1SF36A,   # reverse
  6 - df_m3$C1SF36J,   # reverse
  6 - df_m3$C1SF42A,   # reverse
  df_m3$C1SF42C,       # already reverse-keyed (R)
  6 - df_m3$C1SF42D    # reverse
), na.rm = FALSE)

# -----------------------------------------------------------------------------
# COWORKER SUPPORT (Section F, Q37a-b)
# -----------------------------------------------------------------------------

df_m3$C1SJCCS_mean <- rowMeans(6 - df_m3[, c("C1SF37A", "C1SF37B")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SUPERVISOR SUPPORT (Section F, Q37c-e)
# -----------------------------------------------------------------------------

df_m3$C1SJCSS_mean <- rowMeans(6 - df_m3[, c("C1SF37C", "C1SF37D", "C1SF37E")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SPOUSE/PARTNER SUPPORT (Section L, Q12a-f)
# -----------------------------------------------------------------------------

df_m3$C1SSPEMP_mean <- rowMeans(5 - df_m3[, c("C1SL12A", "C1SL12B", "C1SL12C", 
                                             "C1SL12D", "C1SL12E", "C1SL12F")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# FAMILY DEMANDS (Composite - POMP transformation)
# Home demands: F43a, F43c (R), F43d
# Family demands: J4g, L12g
# -----------------------------------------------------------------------------

df_m3$C1SF43A_pomp <- pomp(6 - df_m3$C1SF43A, 1, 5)
df_m3$C1SF43C_pomp <- pomp(df_m3$C1SF43C, 1, 5)
df_m3$C1SF43D_pomp <- pomp(6 - df_m3$C1SF43D, 1, 5)
df_m3$C1SJ4G_pomp  <- pomp(5 - df_m3$C1SJ4G, 1, 4)
df_m3$C1SL12G_pomp <- pomp(5 - df_m3$C1SL12G, 1, 4)

df_m3$FAM_DEMANDS_M3 <- rowMeans(df_m3[, c("C1SF43A_pomp", "C1SF43C_pomp", "C1SF43D_pomp",
                                            "C1SJ4G_pomp", "C1SL12G_pomp")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# RESCALE PRE-SCORED SPILLOVER OUTCOMES TO PER-ITEM MEANS
# -----------------------------------------------------------------------------
# MIDUS provides the four spillover scales as 4-item SUMS (range 4-20).
# We divide by 4 here so all outcomes and predictors share a comparable
# per-item Likert scale (~1-5). Standardized betas / p-values / R^2 are
# invariant under this rescaling; only raw B and the intercept change.
df_m2$B1SNEGWF <- df_m2$B1SNEGWF / 4
df_m2$B1SNEGFW <- df_m2$B1SNEGFW / 4
df_m2$B1SPOSWF <- df_m2$B1SPOSWF / 4
df_m2$B1SPOSFW <- df_m2$B1SPOSFW / 4
df_m3$C1SNEGWF <- df_m3$C1SNEGWF / 4
df_m3$C1SNEGFW <- df_m3$C1SNEGFW / 4
df_m3$C1SPOSWF <- df_m3$C1SPOSWF / 4
df_m3$C1SPOSFW <- df_m3$C1SPOSFW / 4
# =============================================================================
# HOUSEHOLD-SIZE-EXCLUDING-RESPONDENT
# =============================================================================
# B1PHHSIZE = B1PC4 (other adults) + B1PKIDHH (kids in HH) + 1 (respondent).
# Carrying both PHHSIZE and PC4 in regressions induces near-perfect collinearity
# (they're linearly related through PKIDHH). We replace that pair with a
# single derived variable, PHHSIZE - 1 = the count of household members
# other than the respondent (adults + kids combined).
df_m2$B1PHHSIZE_oth <- df_m2$B1PHHSIZE - 1
df_m3$C1PHHSIZE_oth <- df_m3$C1PHHSIZE - 1

# =============================================================================
# RECODE GENDER AS FACTOR (Male = reference / "0", Female = "1" contrast)
# MIDUS originals: 1 = Male, 2 = Female. After recode, the regression
# coefficient is named *PRSEXFemale and reflects the female-vs-male contrast.
# =============================================================================
df_m2$B1PRSEX <- factor(df_m2$B1PRSEX, levels = c(1, 2), labels = c("Male", "Female"))
df_m3$C1PRSEX <- factor(df_m3$C1PRSEX, levels = c(1, 2), labels = c("Male", "Female"))

# =============================================================================
# SAMPLE RESTRICTIONS (RESPONDENT EMPLOYED)
# =============================================================================

# MIDUS 2: Respondent employed
df_m2_analytic <- df_m2 %>%
  filter(B1SF13 == 1)

# MIDUS 3: Respondent employed
df_m3_analytic <- df_m3 %>%
  filter(C1SF24 == 1)

cat("MIDUS 2 analytic sample (respondent employed):", nrow(df_m2_analytic), "\n")
cat("MIDUS 3 analytic sample (respondent employed):", nrow(df_m3_analytic), "\n")

# Dual-earner subsets (respondent + spouse employed) for multiverse ARM C
df_m2_dual_earner <- df_m2 %>% filter(B1SF13 == 1 & B1PB34WK %in% c(1, 2))
df_m3_dual_earner <- df_m3 %>% filter(C1SF24 == 1 & C1PB34WK %in% c(1, 2))

cat("MIDUS 2 dual-earner subset:", nrow(df_m2_dual_earner), "\n")
cat("MIDUS 3 dual-earner subset:", nrow(df_m3_dual_earner), "\n")

# =============================================================================
# MEAN-CENTER CONTINUOUS PREDICTORS (for interactions)
# =============================================================================

# MIDUS 2
df_m2_analytic$B3TEFZ1_c <- scale(df_m2_analytic$B3TEFZ1, center = TRUE, scale = FALSE)
df_m2_analytic$B1SJCSS_c <- scale(df_m2_analytic$B1SJCSS_mean, center = TRUE, scale = FALSE)
df_m2_analytic$B1SJCCS_c <- scale(df_m2_analytic$B1SJCCS_mean, center = TRUE, scale = FALSE)
df_m2_analytic$B1SSPEMP_c <- scale(df_m2_analytic$B1SSPEMP_mean, center = TRUE, scale = FALSE)

# MIDUS 3
df_m3_analytic$C3TEF_c <- scale(df_m3_analytic$C3TEF, center = TRUE, scale = FALSE)
df_m3_analytic$C1SJCSS_c <- scale(df_m3_analytic$C1SJCSS_mean, center = TRUE, scale = FALSE)
df_m3_analytic$C1SJCCS_c <- scale(df_m3_analytic$C1SJCCS_mean, center = TRUE, scale = FALSE)
df_m3_analytic$C1SSPEMP_c <- scale(df_m3_analytic$C1SSPEMP_mean, center = TRUE, scale = FALSE)

# Mean-center dual-earner subsets (same variables)
df_m2_dual_earner$B3TEFZ1_c <- scale(df_m2_dual_earner$B3TEFZ1, center = TRUE, scale = FALSE)
df_m2_dual_earner$B1SJCSS_c <- scale(df_m2_dual_earner$B1SJCSS_mean, center = TRUE, scale = FALSE)
df_m2_dual_earner$B1SJCCS_c <- scale(df_m2_dual_earner$B1SJCCS_mean, center = TRUE, scale = FALSE)
df_m2_dual_earner$B1SSPEMP_c <- scale(df_m2_dual_earner$B1SSPEMP_mean, center = TRUE, scale = FALSE)

df_m3_dual_earner$C3TEF_c <- scale(df_m3_dual_earner$C3TEF, center = TRUE, scale = FALSE)
df_m3_dual_earner$C1SJCSS_c <- scale(df_m3_dual_earner$C1SJCSS_mean, center = TRUE, scale = FALSE)
df_m3_dual_earner$C1SJCCS_c <- scale(df_m3_dual_earner$C1SJCCS_mean, center = TRUE, scale = FALSE)
df_m3_dual_earner$C1SSPEMP_c <- scale(df_m3_dual_earner$C1SSPEMP_mean, center = TRUE, scale = FALSE)

# --- Additional centered moderators ---
# Only Job Demands and Family Demands are added here as moderators.
# Age, Education, Income, Household Size, and Other Household Members stay
# uncentered (they are not in the user-specified centering set). Gender
# stays uncentered because it is binary (0 = male, 1 = female).
df_m2_analytic$B1SJCDS_c         <- scale(df_m2_analytic$B1SJCDS_mean,    center = TRUE, scale = FALSE)
df_m2_analytic$FAM_DEMANDS_M2_c  <- scale(df_m2_analytic$FAM_DEMANDS_M2, center = TRUE, scale = FALSE)

df_m3_analytic$C1SJCDS_c         <- scale(df_m3_analytic$C1SJCDS_mean,    center = TRUE, scale = FALSE)
df_m3_analytic$FAM_DEMANDS_M3_c  <- scale(df_m3_analytic$FAM_DEMANDS_M3, center = TRUE, scale = FALSE)

df_m2_dual_earner$B1SJCDS_c        <- scale(df_m2_dual_earner$B1SJCDS_mean,    center = TRUE, scale = FALSE)
df_m2_dual_earner$FAM_DEMANDS_M2_c <- scale(df_m2_dual_earner$FAM_DEMANDS_M2, center = TRUE, scale = FALSE)

df_m3_dual_earner$C1SJCDS_c        <- scale(df_m3_dual_earner$C1SJCDS_mean,    center = TRUE, scale = FALSE)
df_m3_dual_earner$FAM_DEMANDS_M3_c <- scale(df_m3_dual_earner$FAM_DEMANDS_M3, center = TRUE, scale = FALSE)

# --- Sanity checks ---
stopifnot(
  # Subset relationships hold
  "analytic not subset of merged (M2)"    = nrow(df_m2_analytic) < nrow(df_m2),
  "analytic not subset of merged (M3)"    = nrow(df_m3_analytic) < nrow(df_m3),
  "dual-earner not subset of analytic M2" = nrow(df_m2_dual_earner) <= nrow(df_m2_analytic),
  "dual-earner not subset of analytic M3" = nrow(df_m3_dual_earner) <= nrow(df_m3_analytic),
  # Employment filter actually worked
  "M2 analytic contains non-employed" = all(df_m2_analytic$B1SF13 == 1, na.rm = TRUE),
  "M3 analytic contains non-employed" = all(df_m3_analytic$C1SF24 == 1, na.rm = TRUE),
  # Scored scales exist and are not all NA
  "M2 Job Demands all NA" = !all(is.na(df_m2$B1SJCDS_mean)),
  "M3 Job Demands all NA" = !all(is.na(df_m3$C1SJCDS_mean)),
  "M2 Fam Demands all NA" = !all(is.na(df_m2$FAM_DEMANDS_M2)),
  "M3 Fam Demands all NA" = !all(is.na(df_m3$FAM_DEMANDS_M3)),
  # Mean-centered variables have mean near zero (within analytic sample)
  "M2 EF_c not centered"  = abs(mean(df_m2_analytic$B3TEFZ1_c, na.rm = TRUE)) < 0.01,
  "M3 EF_c not centered"  = abs(mean(df_m3_analytic$C3TEF_c, na.rm = TRUE)) < 0.01
)
cat("02_scoring checks passed.\n")
