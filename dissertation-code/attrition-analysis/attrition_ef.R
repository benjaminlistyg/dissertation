# =============================================================================
# ATTRITION ANALYSIS: M2 EF SCORES BY M2 -> M3 RETENTION STATUS
# Question: were participants who dropped out between MIDUS 2 and MIDUS 3
# lower on executive functioning?
# Two panels:
#   1. Full BTACT sample: all MIDUS 2 respondents with an EF composite
#      (retained = completed the MIDUS 3 BTACT with an EF composite).
#      Cleanest pure-attrition read.
#   2. Analytic sample: the main-analysis regression sample from
#      03_regressions.R. Non-retained respondents are decomposed into
#        - "Left the study": no MIDUS 3 survey record at all
#        - "In M3, filtered out": present in the MIDUS 3 survey but failed
#          the M3 analytic criteria (employment / complete-case filters)
# Run from the project root (dissertation-code/):
#   source("attrition-analysis/attrition_ef.R")
# =============================================================================

library(tidyverse)
library(haven)
library(cowplot)

select <- dplyr::select
filter <- dplyr::filter

# =============================================================================
# FULL BTACT SAMPLE
# =============================================================================

m2_btact <- read_sav("M2_P3_BTACT_N4512_20211123.sav",
                     col_select = c("M2ID", "B3TEFZ1"))
m3_btact <- read_sav("M3_P3_BTACT_N3291_20210922.sav",
                     col_select = c("M2ID", "C3TEF"))

full_m2 <- m2_btact %>% filter(!is.na(B3TEFZ1))
m3_btact_ids <- m3_btact %>% filter(!is.na(C3TEF)) %>% pull(M2ID)

full_df <- full_m2 %>%
  mutate(
    status = ifelse(M2ID %in% m3_btact_ids, "Retained at M3", "Dropped out"),
    ef = as.numeric(B3TEFZ1)
  ) %>%
  select(M2ID, status, ef)

# =============================================================================
# ANALYTIC SAMPLE (mirrors the complete-case filters in 03_regressions.R)
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

df_m2_all <- df_m2_analytic %>% filter(complete.cases(across(all_of(all_vars_m2))))
df_m3_all <- df_m3_analytic %>% filter(complete.cases(across(all_of(all_vars_m3))))

# Everyone with any MIDUS 3 survey record (M3 is a pure follow-up of M2)
m3_survey_ids <- read_sav("M3_P1_SURVEY_N3294_20181219.sav",
                          col_select = "M2ID") %>% pull(M2ID)

analytic_df <- df_m2_all %>%
  mutate(
    status = case_when(
      M2ID %in% df_m3_all$M2ID ~ "Retained at M3",
      M2ID %in% m3_survey_ids  ~ "In M3, filtered out",
      TRUE                     ~ "Left the study"
    ),
    ef = as.numeric(B3TEFZ1)
  ) %>%
  select(M2ID, status, ef)

# =============================================================================
# COMPARE GROUPS (retained vs each non-retained group)
# =============================================================================

cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  sp <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / sp
}

compare_groups <- function(df, sample_label, ref = "Retained at M3") {
  ref_ef <- df$ef[df$status == ref]
  map_dfr(setdiff(unique(df$status), ref), function(grp) {
    grp_ef <- df$ef[df$status == grp]
    tt <- t.test(ref_ef, grp_ef)
    tibble(
      sample = sample_label,
      comparison = paste(ref, "vs", grp),
      group = grp,
      n_retained = length(ref_ef),
      n_group = length(grp_ef),
      m_retained = mean(ref_ef),
      m_group = mean(grp_ef),
      sd_retained = sd(ref_ef),
      sd_group = sd(grp_ef),
      t = unname(tt$statistic),
      df = unname(tt$parameter),
      p = tt$p.value,
      d = cohens_d(ref_ef, grp_ef)
    )
  })
}

stats_df <- bind_rows(
  compare_groups(full_df, "Full BTACT sample"),
  compare_groups(analytic_df, "Analytic sample")
)

cat("=== M2 EF (B3TEFZ1) by M2 -> M3 retention status ===\n")
print(as.data.frame(stats_df %>% select(-sd_retained, -sd_group)), digits = 3)

# =============================================================================
# PLOT
# =============================================================================

fmt_p <- function(p) ifelse(p < .001, "< .001", sprintf("= %.3f", p))

make_panel <- function(df, stats, title, fills, linetypes, bracket_step = 0.13) {
  status_levels <- names(fills)
  df <- df %>% mutate(status = factor(status, levels = status_levels))

  peak <- max(map_dbl(split(df$ef, df$status), ~ max(density(.x)$y)))
  means <- df %>%
    group_by(status) %>%
    summarise(m = mean(ef), .groups = "drop")

  # One bracket per retained-vs-group comparison, stacked above the curves
  br <- stats %>%
    arrange(abs(m_retained - m_group)) %>%
    mutate(
      y = peak * (1.06 + bracket_step * (row_number() - 1)),
      x_mid = (m_retained + m_group) / 2,
      label = sprintf("d = %.2f, p %s", d, fmt_p(p))
    )

  ns <- df %>% count(status)
  legend_labs <- setNames(
    sprintf("%s (n = %s)", ns$status, format(ns$n, big.mark = ",", trim = TRUE)),
    as.character(ns$status)
  )

  ggplot(df, aes(x = ef, fill = status, linetype = status)) +
    geom_density(alpha = 0.45, color = "grey20", linewidth = 0.6) +
    # Group mean lines, capped below the brackets, linetype-matched to curves
    geom_segment(data = means,
                 aes(x = m, xend = m, y = 0, yend = peak * 1.06, linetype = status),
                 color = "grey20", linewidth = 0.5, inherit.aes = FALSE,
                 show.legend = FALSE) +
    geom_segment(data = br,
                 aes(x = m_group, xend = m_retained, y = y, yend = y),
                 arrow = arrow(ends = "both", length = unit(0.06, "in"), angle = 90),
                 color = "grey20", linewidth = 0.5, inherit.aes = FALSE) +
    geom_text(data = br,
              aes(x = x_mid, y = y, label = label),
              vjust = -0.55, size = 3.2, inherit.aes = FALSE) +
    scale_fill_manual(values = fills, labels = legend_labs, name = NULL) +
    scale_linetype_manual(values = linetypes, labels = legend_labs, name = NULL) +
    coord_cartesian(xlim = c(-4.5, 3.5)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.26))) +
    labs(title = title,
         x = "Executive Functioning (BTACT composite z-score, MIDUS 2)",
         y = "Density") +
    theme_minimal(base_size = 12, base_family = "Times New Roman") +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 8.5),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    guides(fill = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1))
}

p_full <- make_panel(
  full_df,
  stats_df %>% filter(sample == "Full BTACT sample"),
  "Full BTACT sample (pure attrition)",
  fills = c("Retained at M3" = "grey45", "Dropped out" = "grey85"),
  linetypes = c("Retained at M3" = "solid", "Dropped out" = "22")
)

p_analytic <- make_panel(
  analytic_df,
  stats_df %>% filter(sample == "Analytic sample"),
  "Analytic sample (non-retained split)",
  fills = c("Retained at M3" = "grey45",
            "In M3, filtered out" = "grey75",
            "Left the study" = "grey92"),
  linetypes = c("Retained at M3" = "solid",
                "In M3, filtered out" = "22",
                "Left the study" = "dotted")
)

# APA convention: the figure title lives in the caption, not in the image, so
# the two panels are labelled A / B and no overall title row is drawn.
p <- plot_grid(p_full, p_analytic, nrow = 1, align = "h", axis = "tb",
               labels = c("A", "B"), label_fontfamily = "Times New Roman",
               label_size = 13) +
  theme(plot.background = element_rect(fill = "white", color = NA))

out_file <- "attrition-analysis/attrition_ef_plot.png"
ggsave(out_file, p, width = 12, height = 6.0, dpi = 300, bg = "white")
write_csv(stats_df, "attrition-analysis/attrition_ef_stats.csv")
cat("\nSaved:", out_file, "and attrition-analysis/attrition_ef_stats.csv\n")

stopifnot(
  "full sample missing groups"   = n_distinct(full_df$status) == 2,
  "analytic sample not 3 groups" = n_distinct(analytic_df$status) == 3,
  "analytic N mismatch with 03"  = nrow(analytic_df) == nrow(df_m2_all),
  "decomposition does not sum"   =
    sum(analytic_df$status != "Retained at M3") ==
    sum(analytic_df$status == "In M3, filtered out") +
    sum(analytic_df$status == "Left the study")
)
cat("attrition_ef checks passed.\n")
