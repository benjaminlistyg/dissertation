# =============================================================================
# MIDUS 2 & 3 - SUM-SCORED SCALES FROM INDIVIDUAL ITEMS
# Corrected Variable Names Based on Codebook Verification
# =============================================================================

# POMP transform function
pomp <- function(x, min_val, max_val) {
  100 * (x - min_val) / (max_val - min_val)
}

# =============================================================================
# MIDUS 2 (M2P1) - SCORING
# =============================================================================

# -----------------------------------------------------------------------------
# WORK-FAMILY SPILLOVER - Using pre-scored scales from MIDUS
# B1SPOSWF: Positive Work-to-Family (Items: F27e-h)
# B1SNEGWF: Negative Work-to-Family (Items: F27a-d)
# B1SPOSFW: Positive Family-to-Work (Items: I27m-p)
# B1SNEGFW: Negative Family-to-Work (Items: I27i-l)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# JOB DEMANDS (Section F, Q28a, Q28j, Q29a, Q29c, Q29d)
# Coding: 1=All the time to 5=Never
# Reverse code (except Q29c which is already reverse-keyed)
# -----------------------------------------------------------------------------

df_m2$B1SJCDS_sum <- rowSums(cbind(
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

df_m2$B1SJCCS_sum <- rowSums(6 - df_m2[, c("B1SF30A", "B1SF30B")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SUPERVISOR SUPPORT (Section F, Q30c-e)
# Coding: 1=All the time to 5=Never
# Reverse code so higher = more support
# -----------------------------------------------------------------------------

df_m2$B1SJCSS_sum <- rowSums(6 - df_m2[, c("B1SF30C", "B1SF30D", "B1SF30E")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SPOUSE/PARTNER SUPPORT (Section L, Q11a-f)
# Coding: 1=A lot to 4=Not at all
# Reverse code so higher = more support
# -----------------------------------------------------------------------------

df_m2$B1SSPEMP_sum <- rowSums(5 - df_m2[, c("B1SL11A", "B1SL11B", "B1SL11C", 
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
# MIDUS 3 (M3P1) - SCORING
# =============================================================================

# -----------------------------------------------------------------------------
# WORK-FAMILY SPILLOVER - Using pre-scored scales from MIDUS
# C1SPOSWF: Positive Work-to-Family (Items: F40e-h)
# C1SNEGWF: Negative Work-to-Family (Items: F40a-d)
# C1SPOSFW: Positive Family-to-Work (Items: F40m-p)
# C1SNEGFW: Negative Family-to-Work (Items: F40i-l)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# JOB DEMANDS (Section F, Q36a, Q36j, Q42a, Q42c, Q42d)
# -----------------------------------------------------------------------------

df_m3$C1SJCDS_sum <- rowSums(cbind(
  6 - df_m3$C1SF36A,   # reverse
  6 - df_m3$C1SF36J,   # reverse
  6 - df_m3$C1SF42A,   # reverse
  df_m3$C1SF42C,       # already reverse-keyed (R)
  6 - df_m3$C1SF42D    # reverse
), na.rm = FALSE)

# -----------------------------------------------------------------------------
# COWORKER SUPPORT (Section F, Q37a-b)
# -----------------------------------------------------------------------------

df_m3$C1SJCCS_sum <- rowSums(6 - df_m3[, c("C1SF37A", "C1SF37B")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SUPERVISOR SUPPORT (Section F, Q37c-e)
# -----------------------------------------------------------------------------

df_m3$C1SJCSS_sum <- rowSums(6 - df_m3[, c("C1SF37C", "C1SF37D", "C1SF37E")], na.rm = FALSE)

# -----------------------------------------------------------------------------
# SPOUSE/PARTNER SUPPORT (Section L, Q12a-f)
# -----------------------------------------------------------------------------

df_m3$C1SSPEMP_sum <- rowSums(5 - df_m3[, c("C1SL12A", "C1SL12B", "C1SL12C", 
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