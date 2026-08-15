library(WebPower)
library(ggplot2)
library(dplyr)
library(tidyr)

# Table 1: Minimum detectable effects (fixed N)
power_table <- function(n_obs, sample_name) {
  
  cat("\n===", sample_name, "(n =", n_obs, ") ===\n")
  cat("Minimum detectable effect sizes:\n\n")
  
  models <- list(
    "Main effect (EF only)" = list(p1 = 8, p2 = 1),
    "2-way (EF × Gender)" = list(p1 = 10, p2 = 1),
    "2-way (EF × Support)" = list(p1 = 10, p2 = 1),
    "3-way (EF × Gender × Support)" = list(p1 = 15, p2 = 1)
  )
  
  results <- data.frame(
    Model = character(),
    Predictors = integer(),
    f2_80 = numeric(),
    f2_90 = numeric(),
    dR2_80 = numeric(),
    dR2_90 = numeric()
  )
  
  for (model_name in names(models)) {
    m <- models[[model_name]]
    
    res_80 <- wp.regression(n = n_obs, p1 = m$p1, p2 = m$p2, alpha = 0.05, power = 0.80)
    res_90 <- wp.regression(n = n_obs, p1 = m$p1, p2 = m$p2, alpha = 0.05, power = 0.90)
    
    results <- rbind(results, data.frame(
      Model = model_name,
      Predictors = m$p1,
      f2_80 = round(res_80$f2, 4),
      f2_90 = round(res_90$f2, 4),
      dR2_80 = round(res_80$f2 / (1 + res_80$f2), 4),
      dR2_90 = round(res_90$f2 / (1 + res_90$f2), 4)
    ))
  }
  
  print(results, row.names = FALSE)
  invisible(results)
}

# Table 2: Power at fixed effect sizes
power_at_effect_sizes <- function(n_obs, sample_name, 
                                  f2_values = c(0.01, 0.02, 0.05, 0.10, 0.15)) {
  
  cat("\n===", sample_name, "(n =", n_obs, ") ===\n")
  cat("Power at conventional effect sizes:\n\n")
  
  models <- list(
    "Main effect" = 8,
    "2-way" = 10,
    "3-way" = 15
  )
  
  cat(sprintf("%-12s", "f2 (dR2)"))
  for (model_name in names(models)) {
    cat(sprintf("%12s", model_name))
  }
  cat("\n")
  cat(paste(rep("-", 12 + 12 * length(models)), collapse = ""), "\n")
  
  for (f2 in f2_values) {
    dR2 <- round(f2 / (1 + f2), 3)
    cat(sprintf("%-12s", paste0(f2, " (", dR2, ")")))
    
    for (model_name in names(models)) {
      p1 <- models[[model_name]]
      res <- wp.regression(n = n_obs, p1 = p1, p2 = 1, f2 = f2, alpha = 0.05, power = NULL)
      cat(sprintf("%12s", paste0(round(res$power * 100, 1), "%")))
    }
    cat("\n")
  }
}

# Generate power curve data
power_curve_data <- function(samples, f2_range = seq(0.005, 0.15, by = 0.005)) {
  
  models <- list(
    "Main effect" = 8,
    "2-way" = 10,
    "3-way" = 15
  )
  
  results <- data.frame()
  
  for (sample_name in names(samples)) {
    n_obs <- samples[[sample_name]]
    
    for (model_name in names(models)) {
      p1 <- models[[model_name]]
      
      for (f2 in f2_range) {
        res <- wp.regression(n = n_obs, p1 = p1, p2 = 1, f2 = f2, alpha = 0.05, power = NULL)
        
        results <- rbind(results, data.frame(
          Sample = sample_name,
          n = n_obs,
          Model = model_name,
          f2 = f2,
          dR2 = f2 / (1 + f2),
          Power = res$power
        ))
      }
    }
  }
  
  results$Sample <- factor(results$Sample, levels = names(samples))
  results$Model <- factor(results$Model, levels = c("Main effect", "2-way", "3-way"))
  
  return(results)
}

# Create plot
power_plot <- function(samples) {
  
  df <- power_curve_data(samples)
  
  ggplot(df, aes(x = f2, y = Power, color = Model, linetype = Sample)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray40") +
    geom_hline(yintercept = 0.90, linetype = "dotted", color = "gray40") +
    annotate("text", x = 0.14, y = 0.82, label = "80%", size = 3, color = "gray40") +
    annotate("text", x = 0.14, y = 0.92, label = "90%", size = 3, color = "gray40") +
    scale_x_continuous(
      name = expression(paste("Effect size (", f^2, ")")),
      sec.axis = sec_axis(~ . / (1 + .), name = expression(paste(Delta, R^2)))
    ) +
    scale_y_continuous(name = "Power", labels = scales::percent, limits = c(0, 1)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Statistical Power by Effect Size and Model Complexity",
      subtitle = paste0("MIDUS II (n = ", samples[["MIDUS II"]], 
                        ") and MIDUS III (n = ", samples[["MIDUS III"]], ")"),
      caption = "α = .05, testing single coefficient"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank()
    )
}

# Run everything — derive sample sizes from the regression complete-case data
n_m2 <- nrow(df_m2_all)
n_m3 <- nrow(df_m3_all)
cat("Power analysis using regression sample sizes: MIDUS II n =", n_m2,
    ", MIDUS III n =", n_m3, "\n")

samples <- list("MIDUS II" = n_m2, "MIDUS III" = n_m3)

power_table(n_m2, "MIDUS II")
power_at_effect_sizes(n_m2, "MIDUS II")

power_table(n_m3, "MIDUS III")
power_at_effect_sizes(n_m3, "MIDUS III")

# Generate and display plot
power_plot(samples)

#####

library(pwr)

# Power analysis for standardized beta coefficients
# Uses the relationship: t = β * sqrt(n - k - 1) / sqrt(1 - R²)
# Which connects to f² as: f² = β² * (1 - R²_x) / (1 - R²_y)

power_for_beta <- function(n_obs, n_predictors, beta_values = seq(0.05, 0.25, by = 0.05),
                           alpha = 0.05, r_squared_model = 0.15) {
  
  # Degrees of freedom
  df_residual <- n_obs - n_predictors - 1
  
  results <- data.frame(
    beta = beta_values,
    power = NA
  )
  
  for (i in seq_along(beta_values)) {
    beta <- beta_values[i]
    
    # Convert standardized beta to f² for a single coefficient
    # Assuming predictor is uncorrelated with other predictors (conservative)
    # f² = beta² / (1 - R²_full)
    f2 <- beta^2 / (1 - r_squared_model)
    
    # Calculate power using pwr.f2.test
    res <- pwr.f2.test(u = 1, v = df_residual, f2 = f2, sig.level = alpha)
    results$power[i] <- res$power
  }
  
  return(results)
}

# More direct approach using t-distribution
power_for_beta_direct <- function(n_obs, n_predictors, 
                                  beta_values = seq(0.05, 0.25, by = 0.05),
                                  alpha = 0.05, 
                                  r_squared_x = 0.3) {  # R² of focal predictor with other predictors
  
  cat("Sample size:", n_obs, "\n")
  cat("Number of predictors:", n_predictors, "\n")
  cat("Predictor multicollinearity (R²_x):", r_squared_x, "\n\n")
  
  df <- n_obs - n_predictors - 1
  t_crit <- qt(1 - alpha/2, df)
  
  results <- data.frame(
    beta = beta_values,
    se_beta = NA,
    ncp = NA,  # non-centrality parameter
    power = NA
  )
  
  for (i in seq_along(beta_values)) {
    beta <- beta_values[i]
    
    # SE of standardized beta ≈ sqrt((1 - R²_y) / ((1 - R²_x) * (n - k - 1)))
    # For standardized variables, SE ≈ 1 / sqrt((1 - R²_x) * (n - k - 1))
    se_beta <- 1 / sqrt((1 - r_squared_x) * df)
    
    # Non-centrality parameter
    ncp <- beta / se_beta
    
    # Power = P(|t| > t_crit | ncp)
    power <- 1 - pt(t_crit, df, ncp) + pt(-t_crit, df, ncp)
    
    results$se_beta[i] <- round(se_beta, 4)
    results$ncp[i] <- round(ncp, 2)
    results$power[i] <- round(power, 3)
  }
  
  print(results, row.names = FALSE)
  invisible(results)
}

# =============================================================================
# Run for your MIDUS samples
# =============================================================================

library(pwr)

# Power analysis for standardized beta coefficients
# Uses the relationship: t = β * sqrt(n - k - 1) / sqrt(1 - R²)
# Which connects to f² as: f² = β² * (1 - R²_x) / (1 - R²_y)

power_for_beta <- function(n_obs, n_predictors, beta_values = seq(0.05, 0.25, by = 0.05),
                           alpha = 0.05, r_squared_model = 0.15) {
  
  # Degrees of freedom
  df_residual <- n_obs - n_predictors - 1
  
  results <- data.frame(
    beta = beta_values,
    power = NA
  )
  
  for (i in seq_along(beta_values)) {
    beta <- beta_values[i]
    
    # Convert standardized beta to f² for a single coefficient
    # Assuming predictor is uncorrelated with other predictors (conservative)
    # f² = beta² / (1 - R²_full)
    f2 <- beta^2 / (1 - r_squared_model)
    
    # Calculate power using pwr.f2.test
    res <- pwr.f2.test(u = 1, v = df_residual, f2 = f2, sig.level = alpha)
    results$power[i] <- res$power
  }
  
  return(results)
}

# More direct approach using t-distribution
power_for_beta_direct <- function(n_obs, n_predictors, 
                                  beta_values = seq(0.05, 0.25, by = 0.05),
                                  alpha = 0.05, 
                                  r_squared_x = 0.3) {  # R² of focal predictor with other predictors
  
  cat("Sample size:", n_obs, "\n")
  cat("Number of predictors:", n_predictors, "\n")
  cat("Predictor multicollinearity (R²_x):", r_squared_x, "\n\n")
  
  df <- n_obs - n_predictors - 1
  t_crit <- qt(1 - alpha/2, df)
  
  results <- data.frame(
    beta = beta_values,
    se_beta = NA,
    ncp = NA,  # non-centrality parameter
    power = NA
  )
  
  for (i in seq_along(beta_values)) {
    beta <- beta_values[i]
    
    # SE of standardized beta ≈ sqrt((1 - R²_y) / ((1 - R²_x) * (n - k - 1)))
    # For standardized variables, SE ≈ 1 / sqrt((1 - R²_x) * (n - k - 1))
    se_beta <- 1 / sqrt((1 - r_squared_x) * df)
    
    # Non-centrality parameter
    ncp <- beta / se_beta
    
    # Power = P(|t| > t_crit | ncp)
    power <- 1 - pt(t_crit, df, ncp) + pt(-t_crit, df, ncp)
    
    results$se_beta[i] <- round(se_beta, 4)
    results$ncp[i] <- round(ncp, 2)
    results$power[i] <- round(power, 3)
  }
  
  print(results, row.names = FALSE)
  invisible(results)
}

# =============================================================================
# Run for your MIDUS samples
# =============================================================================

cat("=== MIDUS II (n =", n_m2, ") ===\n")
cat("\nMain Effects Model (8 predictors):\n")
power_for_beta_direct(n_m2, 8, beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20))

cat("\n2-way Interaction Model (10 predictors):\n")
power_for_beta_direct(n_m2, 10, beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20))

cat("\n3-way Interaction Model (15 predictors):\n")
power_for_beta_direct(n_m2, 15, beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20))

cat("\n\n=== MIDUS III (n =", n_m3, ") ===\n")
cat("\nMain Effects Model (8 predictors):\n")
power_for_beta_direct(n_m3, 8, beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20))

cat("\n2-way Interaction Model (10 predictors):\n")
power_for_beta_direct(n_m3, 10, beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20))

cat("\n3-way Interaction Model (15 predictors):\n")
power_for_beta_direct(n_m3, 15, beta_values = c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20))