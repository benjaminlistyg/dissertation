# =============================================================================
# CFA AND MEASUREMENT INVARIANCE FOR SPILLOVER SCALES
# Tests 4-factor structure and measurement invariance across gender
# =============================================================================

library(lavaan)
library(semTools)
library(tidyverse)
library(knitr)
library(kableExtra)

# =============================================================================
# DEFINE SPILLOVER ITEMS BY WAVE
# =============================================================================

# MIDUS 2 items (B1 prefix)
m2_items <- list(
  neg_wtf = c("B1SF27A", "B1SF27B", "B1SF27C", "B1SF27D"),  # Negative Work-to-Family
  pos_wtf = c("B1SF27E", "B1SF27F", "B1SF27G", "B1SF27H"),  # Positive Work-to-Family
  neg_ftw = c("B1SF27I", "B1SF27J", "B1SF27K", "B1SF27L"),  # Negative Family-to-Work
  pos_ftw = c("B1SF27M", "B1SF27N", "B1SF27O", "B1SF27P")   # Positive Family-to-Work
)

# MIDUS 3 items (C1 prefix)
m3_items <- list(
  neg_wtf = c("C1SF40A", "C1SF40B", "C1SF40C", "C1SF40D"),
  pos_wtf = c("C1SF40E", "C1SF40F", "C1SF40G", "C1SF40H"),
  neg_ftw = c("C1SF40I", "C1SF40J", "C1SF40K", "C1SF40L"),
  pos_ftw = c("C1SF40M", "C1SF40N", "C1SF40O", "C1SF40P")
)

# =============================================================================
# CFA MODEL SPECIFICATION (4-FACTOR CORRELATED MODEL)
# =============================================================================

make_cfa_model <- function(items) {
  paste0(
    "# Measurement model\n",
    "neg_wtf =~ ", paste(items$neg_wtf, collapse = " + "), "\n",
    "pos_wtf =~ ", paste(items$pos_wtf, collapse = " + "), "\n",
    "neg_ftw =~ ", paste(items$neg_ftw, collapse = " + "), "\n",
    "pos_ftw =~ ", paste(items$pos_ftw, collapse = " + "), "\n"
  )
}

cfa_model_m2 <- make_cfa_model(m2_items)
cfa_model_m3 <- make_cfa_model(m3_items)

# =============================================================================
# FUNCTION: RUN CFA
# =============================================================================

run_cfa <- function(data, model, wave_name) {
  cat("\n", strrep("=", 70), "\n")
  cat("CFA RESULTS:", wave_name, "\n")
  cat(strrep("=", 70), "\n\n")
  
  fit <- cfa(model, data = data, ordered = FALSE, std.lv = TRUE)
  
  # Model fit
  fit_indices <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", 
                                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                                     "srmr"))
  cat("Model Fit Indices:\n")
  print(round(fit_indices, 3))
  
  # Standardized loadings
  cat("\nStandardized Factor Loadings:\n")
  std_loadings <- standardizedSolution(fit)
  loadings <- std_loadings[std_loadings$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
  print(loadings, row.names = FALSE)
  
  # Factor correlations
  cat("\nFactor Correlations:\n")
  cors <- std_loadings[std_loadings$op == "~~" & std_loadings$lhs != std_loadings$rhs & 
                         std_loadings$lhs %in% c("neg_wtf", "pos_wtf", "neg_ftw", "pos_ftw"),
                       c("lhs", "rhs", "est.std", "pvalue")]
  print(cors, row.names = FALSE)
  
  return(fit)
}

# =============================================================================
# FUNCTION: MEASUREMENT INVARIANCE ACROSS GENDER
# =============================================================================

run_invariance <- function(data, model, group_var = "gender", wave_name) {
  cat("\n", strrep("=", 70), "\n")
  cat("MEASUREMENT INVARIANCE ACROSS GENDER:", wave_name, "\n")
  cat(strrep("=", 70), "\n\n")
  
  # Configural invariance (same structure)
  cat("1. Configural Invariance (same factor structure)...\n")
  fit_config <- cfa(model, data = data, group = group_var, std.lv = TRUE)
  
  # Metric invariance (equal loadings)
  cat("2. Metric Invariance (equal loadings)...\n")
  fit_metric <- cfa(model, data = data, group = group_var, std.lv = TRUE,
                    group.equal = "loadings")
  
  # Scalar invariance (equal intercepts)
  cat("3. Scalar Invariance (equal intercepts)...\n")
  fit_scalar <- cfa(model, data = data, group = group_var, std.lv = TRUE,
                    group.equal = c("loadings", "intercepts"))
  
  # Strict invariance (equal residuals)
  cat("4. Strict Invariance (equal residuals)...\n")
  fit_strict <- cfa(model, data = data, group = group_var, std.lv = TRUE,
                    group.equal = c("loadings", "intercepts", "residuals"))
  
  # Compare models (chi-square difference tests)
  cat("\nModel Comparison (Chi-Square Difference Tests):\n")
  print(anova(fit_config, fit_metric, fit_scalar, fit_strict))
  
  # Fit indices table
  cat("\nFit Indices by Invariance Level:\n")
  models <- list(Configural = fit_config, Metric = fit_metric, 
                 Scalar = fit_scalar, Strict = fit_strict)
  
  fit_table <- data.frame(
    Model = names(models),
    CFI = sapply(models, function(x) fitMeasures(x, "cfi")),
    RMSEA = sapply(models, function(x) fitMeasures(x, "rmsea")),
    SRMR = sapply(models, function(x) fitMeasures(x, "srmr")),
    Chi_sq = sapply(models, function(x) fitMeasures(x, "chisq")),
    df = sapply(models, function(x) fitMeasures(x, "df"))
  )
  fit_table[, -1] <- round(fit_table[, -1], 3)
  print(fit_table, row.names = FALSE)
  
  # Delta CFI (Chen, 2007 criterion: ΔCFI < .01 supports invariance)
  cat("\nChange in CFI (Chen, 2007: ΔCFI < .01 supports invariance):\n")
  cfi_vals <- sapply(models, function(x) fitMeasures(x, "cfi"))
  delta_cfi <- c(NA, diff(cfi_vals))
  names(delta_cfi) <- names(models)
  print(round(delta_cfi, 4))
  
  return(list(configural = fit_config, metric = fit_metric, 
              scalar = fit_scalar, strict = fit_strict))
}

# =============================================================================
# RUN CFA AND MEASUREMENT INVARIANCE
# =============================================================================

# Prepare gender grouping variable (on respondent-employed analytic sample)
df_m2_analytic$gender <- factor(df_m2_analytic$B1PRSEX)
df_m3_analytic$gender <- factor(df_m3_analytic$C1PRSEX)

# Run CFA
fit_m2 <- run_cfa(df_m2_analytic, cfa_model_m2, "MIDUS 2")
fit_m3 <- run_cfa(df_m3_analytic, cfa_model_m3, "MIDUS 3")

# Run measurement invariance across gender
inv_m2 <- run_invariance(df_m2_analytic, cfa_model_m2, "gender", "MIDUS 2")
inv_m3 <- run_invariance(df_m3_analytic, cfa_model_m3, "gender", "MIDUS 3")

# --- Sanity checks ---
stopifnot(
  # CFA models converged
  "M2 CFA did not converge" = lavInspect(fit_m2, "converged"),
  "M3 CFA did not converge" = lavInspect(fit_m3, "converged"),
  # CFI is in a reasonable range (> .80 at minimum)
  "M2 CFA CFI implausibly low" = fitMeasures(fit_m2, "cfi") > 0.80,
  "M3 CFA CFI implausibly low" = fitMeasures(fit_m3, "cfi") > 0.80,
  # All 4 MI levels returned for each wave
  "M2 MI missing levels" = all(c("configural","metric","scalar","strict") %in% names(inv_m2)),
  "M3 MI missing levels" = all(c("configural","metric","scalar","strict") %in% names(inv_m3)),
  # All MI models converged
  "M2 configural MI failed" = lavInspect(inv_m2$configural, "converged"),
  "M3 strict MI failed"     = lavInspect(inv_m3$strict, "converged")
)
cat("05_cfa_invariance checks passed.\n")

# =============================================================================
# GENERATE HTML OUTPUT TABLES
# =============================================================================

OUTPUT_DIR <- file.path("dissertation_code", "output", "tables", "cfa")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

apa_save_html <- function(tbl, filename, dir = OUTPUT_DIR) {
  path <- file.path(dir, filename)
  save_kable(tbl, file = path)
  message("Table saved to: ", path)
  invisible(tbl)
}

format_p_stars <- function(p) {
  case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
}

format_p_apa <- function(p) {
  case_when(p < .001 ~ "< .001",
            TRUE ~ paste0("= ", sub("^0", "", sprintf("%.3f", p))))
}

# --- CFA Fit Indices Table ---
build_cfa_fit_table <- function(fits, fit_names, title = "CFA Model Fit Indices") {
  indices <- c("chisq", "df", "pvalue", "cfi", "tli",
               "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
  tbl <- map2_dfr(fits, fit_names, function(fit, name) {
    fm <- fitMeasures(fit, indices)
    tibble(
      Model = name,
      `chi-sq` = sprintf("%.2f", fm["chisq"]),
      df = as.integer(fm["df"]),
      p = format_p_apa(fm["pvalue"]),
      CFI = sprintf("%.3f", fm["cfi"]),
      TLI = sprintf("%.3f", fm["tli"]),
      RMSEA = sprintf("%.3f", fm["rmsea"]),
      `RMSEA 90% CI` = paste0("[", sprintf("%.3f", fm["rmsea.ci.lower"]),
                              ", ", sprintf("%.3f", fm["rmsea.ci.upper"]), "]"),
      SRMR = sprintf("%.3f", fm["srmr"])
    )
  })
  kable(tbl, caption = title, align = c("l", rep("r", 5), "r", "c", "r")) %>%
    kable_styling(bootstrap_options = "condensed", full_width = FALSE,
                  html_font = "Times New Roman") %>%
    row_spec(0, bold = TRUE) %>%
    footnote(general = "Good fit: CFI > .95, TLI > .95, RMSEA < .06, SRMR < .08.",
             general_title = "Note. ", footnote_as_chunk = TRUE)
}

# --- Standardized Factor Loadings Table ---
build_cfa_loadings_table <- function(fit, title = "Standardized Factor Loadings") {
  factor_labels <- c("neg_wtf" = "Neg W-to-F", "pos_wtf" = "Pos W-to-F",
                     "neg_ftw" = "Neg F-to-W", "pos_ftw" = "Pos F-to-W")
  loadings <- standardizedSolution(fit) %>%
    filter(op == "=~") %>%
    mutate(Factor = factor_labels[lhs], Item = rhs,
           Loading = sprintf("%.3f", est.std), SE = sprintf("%.3f", se),
           sig = pvalue < .05) %>%
    select(Factor, Item, Loading, SE, sig)
  sig_rows <- which(loadings$sig)
  kable(loadings %>% select(-sig),
        col.names = c("Factor", "Item", "\u03BB", "SE"),
        caption = title, align = c("l", "l", "r", "r")) %>%
    kable_styling(bootstrap_options = "condensed", full_width = FALSE,
                  html_font = "Times New Roman") %>%
    row_spec(0, bold = TRUE) %>%
    collapse_rows(columns = 1, valign = "top") %>%
    footnote(general = "All loadings standardized. All p < .001 unless noted.",
             general_title = "Note. ", footnote_as_chunk = TRUE)
}

# --- Factor Correlations Table ---
build_cfa_cors_table <- function(fit, title = "Factor Correlations") {
  factors <- c("neg_wtf", "pos_wtf", "neg_ftw", "pos_ftw")
  factor_labels <- c("neg_wtf" = "Neg W-to-F", "pos_wtf" = "Pos W-to-F",
                     "neg_ftw" = "Neg F-to-W", "pos_ftw" = "Pos F-to-W")
  cors <- standardizedSolution(fit) %>%
    filter(op == "~~", lhs != rhs, lhs %in% factors, rhs %in% factors) %>%
    mutate(lhs = factor_labels[lhs], rhs = factor_labels[rhs],
           r = paste0(sprintf("%.3f", est.std), format_p_stars(pvalue))) %>%
    select(lhs, rhs, r)
  kable(cors, col.names = c("Factor 1", "Factor 2", "r"),
        caption = title, align = c("l", "l", "r")) %>%
    kable_styling(bootstrap_options = "condensed", full_width = FALSE,
                  html_font = "Times New Roman") %>%
    row_spec(0, bold = TRUE) %>%
    footnote(general = "*p < .05. **p < .01. ***p < .001.",
             general_title = "Note. ", footnote_as_chunk = TRUE)
}

# --- Measurement Invariance Summary Table ---
build_invariance_table <- function(fits, title = "Measurement Invariance Across Gender") {
  level_names <- names(fits)
  tbl <- map_dfr(seq_along(fits), function(i) {
    fm <- fitMeasures(fits[[i]], c("chisq", "df", "cfi", "rmsea", "srmr"))
    tibble(Level = level_names[i],
           `chi-sq` = sprintf("%.2f", fm["chisq"]),
           df = as.integer(fm["df"]),
           CFI = sprintf("%.3f", fm["cfi"]),
           RMSEA = sprintf("%.3f", fm["rmsea"]),
           SRMR = sprintf("%.3f", fm["srmr"]))
  })
  cfi_vals <- sapply(fits, function(x) fitMeasures(x, "cfi"))
  delta_cfi <- c(NA, diff(cfi_vals))
  tbl$dCFI <- ifelse(is.na(delta_cfi), "---", sprintf("%.4f", delta_cfi))
  rmsea_vals <- sapply(fits, function(x) fitMeasures(x, "rmsea"))
  delta_rmsea <- c(NA, diff(rmsea_vals))
  tbl$dRMSEA <- ifelse(is.na(delta_rmsea), "---", sprintf("%.4f", delta_rmsea))
  tbl$Decision <- ifelse(is.na(delta_cfi), "---",
                         ifelse(abs(delta_cfi) < .01, "Supported", "Not supported"))
  kable(tbl, caption = title,
        align = c("l", "r", "r", "r", "r", "r", "r", "r", "l")) %>%
    kable_styling(bootstrap_options = "condensed", full_width = FALSE,
                  html_font = "Times New Roman") %>%
    row_spec(0, bold = TRUE) %>%
    footnote(general = paste0(
      "dCFI < .01 supports invariance (Chen, 2007). ",
      "Configural = same structure; Metric = equal loadings; ",
      "Scalar = equal intercepts; Strict = equal residuals."),
      general_title = "Note. ", footnote_as_chunk = TRUE)
}

# --- Generate and save all CFA/MI tables ---

# CFA fit indices (both waves combined)
apa_save_html(
  build_cfa_fit_table(list(fit_m2, fit_m3), c("MIDUS 2", "MIDUS 3"),
                      title = "CFA Model Fit Indices for 4-Factor Spillover Model"),
  "cfa_fit_indices.html")

# Standardized loadings per wave
apa_save_html(
  build_cfa_loadings_table(fit_m2, title = "Standardized Factor Loadings (MIDUS 2)"),
  "cfa_loadings_m2.html")
apa_save_html(
  build_cfa_loadings_table(fit_m3, title = "Standardized Factor Loadings (MIDUS 3)"),
  "cfa_loadings_m3.html")

# Factor correlations per wave
apa_save_html(
  build_cfa_cors_table(fit_m2, title = "Factor Correlations (MIDUS 2)"),
  "cfa_factor_correlations_m2.html")
apa_save_html(
  build_cfa_cors_table(fit_m3, title = "Factor Correlations (MIDUS 3)"),
  "cfa_factor_correlations_m3.html")

# Measurement invariance per wave
apa_save_html(
  build_invariance_table(
    list(Configural = inv_m2$configural, Metric = inv_m2$metric,
         Scalar = inv_m2$scalar, Strict = inv_m2$strict),
    title = "Measurement Invariance Across Gender (MIDUS 2)"),
  "invariance_gender_m2.html")
apa_save_html(
  build_invariance_table(
    list(Configural = inv_m3$configural, Metric = inv_m3$metric,
         Scalar = inv_m3$scalar, Strict = inv_m3$strict),
    title = "Measurement Invariance Across Gender (MIDUS 3)"),
  "invariance_gender_m3.html")

cat("\nCFA/MI tables saved to: ", OUTPUT_DIR, "\n")
