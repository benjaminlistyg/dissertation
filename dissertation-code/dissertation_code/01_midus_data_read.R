# =============================================================================
# MIDUS DISSERTATION ANALYSIS - DATA READING
# Step 1: Read in all variables needed for scoring and regressions
# =============================================================================

library(haven)
library(tidyr)
library(dplyr)
library(purrr)
library(tidylog)

# =============================================================================
# READ RAW DATA FILES
# =============================================================================

M2 <- haven::read_sav('M2_P1_SURVEY_N4963_20200720.sav')
M2_BTACT <- haven::read_sav('M2_P3_BTACT_N4512_20211123.sav')
M3 <- haven::read_sav('M3_P1_SURVEY_N3294_20181219.sav')
M3_BTACT <- haven::read_sav('M3_P3_BTACT_N3291_20210922.sav')

# =============================================================================
# MIDUS 2 - SELECT ALL REQUIRED VARIABLES
# =============================================================================

M2_selected <- M2 %>% 
  dplyr::select(
    M2ID,
    
    # --- Employment filters ---
    B1SF13,        # Respondent currently working for pay
    B1PB34WK,      # Spouse/partner currently employed
    
    # --- Demographics & Controls ---
    B1PAGE_M2,     # Age
    B1PRSEX,       # Gender
    B1PB1,         # Education
    B1STINC1,      # Household income
    B1PHHSIZE,     # Total household size
    B1PC4,         # Household members (excl. children/self)
    B1PKIDHH,      # Number of children in household
    
    # --- Work-Family Spillover (Pre-scored scales) ---
    # Positive Work-to-Family (Items: F27e-h)
    B1SPOSWF,
    # Negative Work-to-Family (Items: F27a-d)
    B1SNEGWF,
    # Positive Family-to-Work (Items: I27m-p)
    B1SPOSFW,
    # Negative Family-to-Work (Items: I27i-l)
    B1SNEGFW,

    # --- Work-Family Spillover Items (for CFA) ---
    B1SF27A, B1SF27B, B1SF27C, B1SF27D,  # Neg W-to-F items
    B1SF27E, B1SF27F, B1SF27G, B1SF27H,  # Pos W-to-F items
    B1SF27I, B1SF27J, B1SF27K, B1SF27L,  # Neg F-to-W items
    B1SF27M, B1SF27N, B1SF27O, B1SF27P,  # Pos F-to-W items
    
    # --- Job Demands Items (Section F Q28a, Q28j, Q29a, Q29c, Q29d) ---
    B1SF28A,       # Work intensively
    B1SF28J,       # Work demands hard to combine
    B1SF29A,       # Time to get everything done at work
    B1SF29C,       # Too many demands at work (R)
    B1SF29D,       # Lot of interruptions at work
    
    # --- Coworker Support Items (Section F Q30a-b) ---
    B1SF30A, B1SF30B,
    
    # --- Supervisor Support Items (Section F Q30c-e) ---
    B1SF30C, B1SF30D, B1SF30E,
    
    # --- Spouse/Partner Support Items (Section L Q11a-f) ---
    B1SL11A, B1SL11B, B1SL11C, B1SL11D, B1SL11E, B1SL11F,
    
    # --- Family Demands Items ---
    # Home demands (F33a, F33c, F33d)
    B1SF33A, B1SF33C, B1SF33D,
    # Family demands (J2g, L11g)
    B1SJ2G, B1SL11G
  )

M2_BTACT_selected <- M2_BTACT %>%
  dplyr::select(
    M2ID,
    B3TEFZ1        # EF Composite z-score (national sample)
  )

# =============================================================================
# MIDUS 3 - SELECT ALL REQUIRED VARIABLES
# =============================================================================

M3_selected <- M3 %>% 
  dplyr::select(
    M2ID,
    
    # --- Employment filters ---
    C1SF24,        # Respondent currently working for pay
    C1PB34WK,      # Spouse/partner currently employed
    
    # --- Demographics & Controls ---
    C1PRAGE,     # Age
    C1PRSEX,       # Gender
    C1PB1,         # Education
    C1STINC,       # Household income
    C1PHHSIZE,     # Total household size
    C1PC4,         # Household members (excl. children/self)
    C1PKIDHH,      # Number of children in household
    
    # --- Work-Family Spillover (Pre-scored scales) ---
    # Positive Work-to-Family (Items: F40e-h)
    C1SPOSWF,
    # Negative Work-to-Family (Items: F40a-d)
    C1SNEGWF,
    # Positive Family-to-Work (Items: F40m-p)
    C1SPOSFW,
    # Negative Family-to-Work (Items: F40i-l)
    C1SNEGFW,

    # --- Work-Family Spillover Items (for CFA) ---
    C1SF40A, C1SF40B, C1SF40C, C1SF40D,  # Neg W-to-F items
    C1SF40E, C1SF40F, C1SF40G, C1SF40H,  # Pos W-to-F items
    C1SF40I, C1SF40J, C1SF40K, C1SF40L,  # Neg F-to-W items
    C1SF40M, C1SF40N, C1SF40O, C1SF40P,  # Pos F-to-W items
    
    # --- Job Demands Items (Section F Q36a, Q36j, Q42a, Q42c, Q42d) ---
    C1SF36A,       # Work intensively
    C1SF36J,       # Work demands hard to combine
    C1SF42A,       # Time to get everything done at work
    C1SF42C,       # Too many demands at work (R)
    C1SF42D,       # Lot of interruptions at work
    
    # --- Coworker Support Items (Section F Q37a-b) ---
    C1SF37A, C1SF37B,
    
    # --- Supervisor Support Items (Section F Q37c-e) ---
    C1SF37C, C1SF37D, C1SF37E,
    
    # --- Spouse/Partner Support Items (Section L Q12a-f) ---
    C1SL12A, C1SL12B, C1SL12C, C1SL12D, C1SL12E, C1SL12F,
    
    # --- Family Demands Items ---
    # Home demands (F43a, F43c, F43d)
    C1SF43A, C1SF43C, C1SF43D,
    # Family demands (J4g, L12g)
    C1SJ4G, C1SL12G
  )

M3_BTACT_selected <- M3_BTACT %>%
  dplyr::select(
    M2ID,
    C3TEF          # EF Composite
  )

# =============================================================================
# MERGE SURVEY AND COGNITIVE DATA
# =============================================================================

df_m2 <- inner_join(M2_selected, M2_BTACT_selected, by = 'M2ID')
df_m3 <- inner_join(M3_selected, M3_BTACT_selected, by = 'M2ID')

# Print initial sample sizes
cat("MIDUS 2 merged sample:", nrow(df_m2), "\n")
cat("MIDUS 3 merged sample:", nrow(df_m3), "\n")

# --- Sanity checks ---
stopifnot("df_m2 has no rows"      = nrow(df_m2) > 0,
          "df_m3 has no rows"      = nrow(df_m3) > 0,
          "M2ID missing in df_m2"  = "M2ID" %in% names(df_m2),
          "EF missing in df_m2"    = "B3TEFZ1" %in% names(df_m2),
          "EF missing in df_m3"    = "C3TEF" %in% names(df_m3),
          "duplicate M2IDs in M2"  = !anyDuplicated(df_m2$M2ID),
          "duplicate M2IDs in M3"  = !anyDuplicated(df_m3$M2ID))
cat("01_data_read checks passed.\n")

