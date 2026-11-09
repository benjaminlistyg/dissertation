# =============================================================================
# EF x Big Five correlations in the analytic (regression) samples
#
# Uses the exact listwise-complete regression samples (df_m2_all, df_m3_all)
# built by 03_regressions.R, joins the MIDUS Big Five scale scores by M2ID,
# and reports Pearson correlations between executive functioning
# (B3TEFZ1 / C3TEF) and each trait. Big Five values missing within the
# regression sample are dropped pairwise (n reported per correlation).
#
# Run from the dissertation-code project root:
#   source("ef-personality-correlations/ef_big5_correlations.R")
# =============================================================================

source("dissertation_code/01_midus_data_read.R")
source("dissertation_code/02_midus_scoring.R")
source("dissertation_code/03_regressions.R")

big5_m2_vars <- c("B1SNEURO", "B1SEXTRA", "B1SOPEN", "B1SCONS1", "B1SAGREE")
big5_m3_vars <- c("C1SNEURO", "C1SEXTRA", "C1SOPEN", "C1SCONS1", "C1SAGREE")
trait_labels <- c("Neuroticism", "Extraversion", "Openness",
                  "Conscientiousness", "Agreeableness")

big5_m2 <- haven::read_sav("M2_P1_SURVEY_N4963_20200720.sav",
                           col_select = all_of(c("M2ID", big5_m2_vars)))
big5_m3 <- haven::read_sav("M3_P1_SURVEY_N3294_20181219.sav",
                           col_select = all_of(c("M2ID", big5_m3_vars)))

d_m2 <- left_join(df_m2_all, big5_m2, by = "M2ID")
d_m3 <- left_join(df_m3_all, big5_m3, by = "M2ID")

cor_row <- function(data, ef_var, trait_var, trait, wave) {
  ct <- cor.test(data[[ef_var]], data[[trait_var]], use = "complete.obs")
  tibble(
    Wave = wave, Trait = trait,
    n = sum(complete.cases(data[[ef_var]], data[[trait_var]])),
    r = unname(ct$estimate),
    ci_lower = ct$conf.int[1], ci_upper = ct$conf.int[2],
    p = ct$p.value
  )
}

ef_big5_cors <- bind_rows(
  map2_dfr(big5_m2_vars, trait_labels,
           ~cor_row(d_m2, "B3TEFZ1", .x, .y, "MIDUS 2")),
  map2_dfr(big5_m3_vars, trait_labels,
           ~cor_row(d_m3, "C3TEF", .x, .y, "MIDUS 3"))
)

out_dir <- "ef-personality-correlations"
readr::write_csv(ef_big5_cors, file.path(out_dir, "ef_big5_correlations.csv"))

fmt_p <- function(p) ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
tbl <- ef_big5_cors %>%
  mutate(across(c(r, ci_lower, ci_upper), ~sprintf("%.3f", .x)),
         `95% CI` = paste0("[", ci_lower, ", ", ci_upper, "]"),
         p = fmt_p(p)) %>%
  select(Wave, Trait, n, r, `95% CI`, p)

kbl(tbl, format = "html", escape = FALSE, caption = NULL,
    col.names = c("Wave", "Trait", "<i>n</i>", "<i>r</i>", "95% CI", "<i>p</i>"),
    align = c("l", "l", "r", "r", "c", "r")) %>%
  kable_classic(lightable_options = "basic", full_width = FALSE) %>%
  save_kable(file.path(out_dir, "ef_big5_correlations.html"))

print(as.data.frame(tbl), row.names = FALSE)
cat("\nSamples: MIDUS 2 n =", nrow(d_m2), "| MIDUS 3 n =", nrow(d_m3), "\n")
