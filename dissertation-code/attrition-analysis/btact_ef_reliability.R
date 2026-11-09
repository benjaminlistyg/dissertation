# =============================================================================
# RELIABILITY OF THE BTACT EXECUTIVE FUNCTIONING COMPOSITE
# Internal consistency of the five EF indicators (Tun & Lachman, 2012 scoring):
#   - Digits Backward           (B3TDBS   / C3TDBS)
#   - Category Fluency          (B3TCTFLU / C3TCTFLU)
#   - Number Series             (B3TNSTOT / C3TNSTOT)
#   - Backward Counting, 30-SACT (B3TBKTOT / C3TBKTOT)
#   - SGST mixed-task latency, mean of switch & nonswitch trials
#     (B3TSMXBB / C3TSMXBB; reverse-scored so higher = better)
# Indicators are z-scored, latency reversed, then reliability is computed as
# Cronbach's alpha and McDonald's omega (single-factor CFA via lavaan) in
# (a) the full BTACT samples and (b) the main-analysis regression samples.
# Includes a verification step: the mean of the five z-scores should
# reproduce the MIDUS-provided composites (B3TEFZ1 / C3TEF).
# Run from the project root (dissertation-code/):
#   source("attrition-analysis/btact_ef_reliability.R")
# =============================================================================

library(tidyverse)
library(haven)
library(lavaan)

select <- dplyr::select
filter <- dplyr::filter

# =============================================================================
# READ EF INDICATORS
# =============================================================================

ef_vars_m2 <- c(dbs = "B3TDBS", flu = "B3TCTFLU", ns = "B3TNSTOT",
                bk = "B3TBKTOT", sgst = "B3TSMXBB")
ef_vars_m3 <- c(dbs = "C3TDBS", flu = "C3TCTFLU", ns = "C3TNSTOT",
                bk = "C3TBKTOT", sgst = "C3TSMXBB")

m2_raw <- read_sav("M2_P3_BTACT_N4512_20211123.sav",
                   col_select = all_of(c("M2ID", unname(ef_vars_m2), "B3TEFZ1")))
m3_raw <- read_sav("M3_P3_BTACT_N3291_20210922.sav",
                   col_select = all_of(c("M2ID", unname(ef_vars_m3), "C3TEF")))

prep_indicators <- function(df, ef_vars, composite_var) {
  out <- df %>%
    mutate(across(all_of(unname(ef_vars)), as.numeric)) %>%
    rename(all_of(ef_vars)) %>%
    filter(if_all(c(dbs, flu, ns, bk, sgst), ~ !is.na(.x)))
  out %>%
    mutate(
      across(c(dbs, flu, ns, bk), ~ as.numeric(scale(.x))),
      sgst = -as.numeric(scale(sgst)),   # latency: faster = better
      composite = as.numeric(.data[[composite_var]])
    )
}

m2_ind <- prep_indicators(m2_raw, ef_vars_m2, "B3TEFZ1")
m3_ind <- prep_indicators(m3_raw, ef_vars_m3, "C3TEF")

# --- Verification: mean of z-scored indicators vs MIDUS composite ---
r_m2 <- cor(rowMeans(m2_ind[c("dbs", "flu", "ns", "bk", "sgst")]),
            m2_ind$composite, use = "complete.obs")
r_m3 <- cor(rowMeans(m3_ind[c("dbs", "flu", "ns", "bk", "sgst")]),
            m3_ind$composite, use = "complete.obs")
cat(sprintf("Verification r with MIDUS composite: M2 = %.3f, M3 = %.3f\n", r_m2, r_m3))

# =============================================================================
# ANALYTIC SAMPLE IDS (mirrors 03_regressions.R complete-case filters)
# =============================================================================

if (!exists("df_m2_analytic") || !exists("df_m3_analytic")) {
  source("dissertation_code/01_midus_data_read.R")
  source("dissertation_code/02_midus_scoring.R")
}

all_vars_m2 <- c("B1SNEGWF", "B1SNEGFW", "B1SPOSWF", "B1SPOSFW",
                 "B3TEFZ1", "B1SJCDS_sum", "FAM_DEMANDS_M2",
                 "B1STINC1", "B1PHHSIZE_oth", "B1PAGE_M2", "B1PRSEX", "B1PB1",
                 "B1SJCSS_sum", "B1SJCCS_sum", "B1SSPEMP_sum")
all_vars_m3 <- c("C1SNEGWF", "C1SNEGFW", "C1SPOSWF", "C1SPOSFW",
                 "C3TEF", "C1SJCDS_sum", "FAM_DEMANDS_M3",
                 "C1STINC", "C1PHHSIZE_oth", "C1PRAGE", "C1PRSEX", "C1PB1",
                 "C1SJCSS_sum", "C1SJCCS_sum", "C1SSPEMP_sum")

ids_m2_analytic <- df_m2_analytic %>%
  filter(complete.cases(across(all_of(all_vars_m2)))) %>% pull(M2ID)
ids_m3_analytic <- df_m3_analytic %>%
  filter(complete.cases(across(all_of(all_vars_m3)))) %>% pull(M2ID)

# =============================================================================
# RELIABILITY: CRONBACH'S ALPHA + McDONALD'S OMEGA
# =============================================================================

cronbach_alpha <- function(items) {
  k <- ncol(items)
  (k / (k - 1)) * (1 - sum(apply(items, 2, var)) / var(rowSums(items)))
}

mcdonald_omega <- function(items) {
  fit <- cfa("ef =~ dbs + flu + ns + bk + sgst",
             data = items, std.lv = TRUE)
  est <- parameterEstimates(fit)
  l <- est$est[est$op == "=~"]
  theta <- est$est[est$op == "~~" & est$lhs == est$rhs & est$lhs != "ef"]
  sum(l)^2 / (sum(l)^2 + sum(theta))
}

reliability_row <- function(ind_df, sample_label, wave_label) {
  items <- ind_df[c("dbs", "flu", "ns", "bk", "sgst")]
  tibble(
    wave = wave_label,
    sample = sample_label,
    n = nrow(items),
    alpha = cronbach_alpha(items),
    omega = mcdonald_omega(items),
    mean_r = mean(cor(items)[lower.tri(cor(items))])
  )
}

rel_df <- bind_rows(
  reliability_row(m2_ind, "Full BTACT sample", "MIDUS 2"),
  reliability_row(m2_ind %>% filter(M2ID %in% ids_m2_analytic),
                  "Analytic sample", "MIDUS 2"),
  reliability_row(m3_ind, "Full BTACT sample", "MIDUS 3"),
  reliability_row(m3_ind %>% filter(M2ID %in% ids_m3_analytic),
                  "Analytic sample", "MIDUS 3")
)

cat("\n=== BTACT EF composite reliability (5 indicators) ===\n")
print(as.data.frame(rel_df), digits = 3)

# Inter-indicator correlations, per wave (full samples)
cat("\n--- Indicator correlations, MIDUS 2 (full BTACT sample) ---\n")
print(round(cor(m2_ind[c("dbs", "flu", "ns", "bk", "sgst")]), 3))
cat("\n--- Indicator correlations, MIDUS 3 (full BTACT sample) ---\n")
print(round(cor(m3_ind[c("dbs", "flu", "ns", "bk", "sgst")]), 3))

write_csv(rel_df, "attrition-analysis/btact_ef_reliability.csv")
cat("\nSaved: attrition-analysis/btact_ef_reliability.csv\n")

stopifnot(
  "M2 composite verification failed (r < .95)" = r_m2 > .95,
  "M3 composite verification failed (r < .95)" = r_m3 > .95,
  "alpha out of range" = all(rel_df$alpha > 0 & rel_df$alpha < 1),
  "omega out of range" = all(rel_df$omega > 0 & rel_df$omega < 1)
)
cat("btact_ef_reliability checks passed.\n")
