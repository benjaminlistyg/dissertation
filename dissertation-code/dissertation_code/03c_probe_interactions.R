# =============================================================================
# PROBE INTERACTION MODELS
# Simple slopes (+/-1 SD) and Johnson-Neyman regions for all 32 moderation
# models from 03_regressions.R. Runs after 03_regressions.R.
# =============================================================================

library(tidyverse)
library(interactions)
library(broom)
library(knitr)
library(kableExtra)
library(ggplot2)
library(cowplot)

# Output dirs (anchored at dissertation_code/ so paths match 07_midus_apa_tables.R
# regardless of whether the script is sourced from the project root or from
# within the dissertation_code/ subdirectory).
probe_dir     <- file.path("dissertation_code", "output", "tables", "interactions")
plot_dir      <- file.path("dissertation_code", "output", "figures", "interactions")
plot_sig_dir  <- file.path(plot_dir, "significant")
plot_nsig_dir <- file.path(plot_dir, "not-significant")
dir.create(probe_dir,     recursive = TRUE, showWarnings = FALSE)
dir.create(plot_sig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(plot_nsig_dir, recursive = TRUE, showWarnings = FALSE)

# Clean stray plots at the top level of plot_dir (keep the two subfolders)
stray <- list.files(plot_dir, full.names = TRUE, recursive = FALSE)
stray <- stray[!file.info(stray)$isdir]
if (length(stray) > 0) {
  file.remove(stray)
  cat("Removed", length(stray), "stray file(s) from", plot_dir, "\n")
}

# --- Catalog of every interaction model --------------------------------------
probe_specs <- tibble::tribble(
  ~model_name,            ~ef,       ~mod,           ~mod_type,    ~outcome,  ~wave,
  # MIDUS 2: Gender (binary)
  "m5_gender_negfw_m2",   "B3TEFZ1", "B1PRSEX",      "binary",     "NegF->W", "M2",
  "m6_gender_negwf_m2",   "B3TEFZ1", "B1PRSEX",      "binary",     "NegW->F", "M2",
  "m7_gender_posfw_m2",   "B3TEFZ1", "B1PRSEX",      "binary",     "PosF->W", "M2",
  "m8_gender_poswf_m2",   "B3TEFZ1", "B1PRSEX",      "binary",     "PosW->F", "M2",
  # MIDUS 2: Supervisor
  "m9_super_negfw_m2",    "B3TEFZ1", "B1SJCSS_sum",  "continuous", "NegF->W", "M2",
  "m10_super_negwf_m2",   "B3TEFZ1", "B1SJCSS_sum",  "continuous", "NegW->F", "M2",
  "m11_super_posfw_m2",   "B3TEFZ1", "B1SJCSS_sum",  "continuous", "PosF->W", "M2",
  "m12_super_poswf_m2",   "B3TEFZ1", "B1SJCSS_sum",  "continuous", "PosW->F", "M2",
  # MIDUS 2: Coworker
  "m13_cowork_negfw_m2",  "B3TEFZ1", "B1SJCCS_sum",  "continuous", "NegF->W", "M2",
  "m14_cowork_negwf_m2",  "B3TEFZ1", "B1SJCCS_sum",  "continuous", "NegW->F", "M2",
  "m15_cowork_posfw_m2",  "B3TEFZ1", "B1SJCCS_sum",  "continuous", "PosF->W", "M2",
  "m16_cowork_poswf_m2",  "B3TEFZ1", "B1SJCCS_sum",  "continuous", "PosW->F", "M2",
  # MIDUS 2: Spouse
  "m17_spouse_negfw_m2",  "B3TEFZ1", "B1SSPEMP_sum", "continuous", "NegF->W", "M2",
  "m18_spouse_negwf_m2",  "B3TEFZ1", "B1SSPEMP_sum", "continuous", "NegW->F", "M2",
  "m19_spouse_posfw_m2",  "B3TEFZ1", "B1SSPEMP_sum", "continuous", "PosF->W", "M2",
  "m20_spouse_poswf_m2",  "B3TEFZ1", "B1SSPEMP_sum", "continuous", "PosW->F", "M2",
  # MIDUS 3: Gender
  "m5_gender_negfw_m3",   "C3TEF",   "C1PRSEX",      "binary",     "NegF->W", "M3",
  "m6_gender_negwf_m3",   "C3TEF",   "C1PRSEX",      "binary",     "NegW->F", "M3",
  "m7_gender_posfw_m3",   "C3TEF",   "C1PRSEX",      "binary",     "PosF->W", "M3",
  "m8_gender_poswf_m3",   "C3TEF",   "C1PRSEX",      "binary",     "PosW->F", "M3",
  # MIDUS 3: Supervisor
  "m9_super_negfw_m3",    "C3TEF",   "C1SJCSS_sum",  "continuous", "NegF->W", "M3",
  "m10_super_negwf_m3",   "C3TEF",   "C1SJCSS_sum",  "continuous", "NegW->F", "M3",
  "m11_super_posfw_m3",   "C3TEF",   "C1SJCSS_sum",  "continuous", "PosF->W", "M3",
  "m12_super_poswf_m3",   "C3TEF",   "C1SJCSS_sum",  "continuous", "PosW->F", "M3",
  # MIDUS 3: Coworker
  "m13_cowork_negfw_m3",  "C3TEF",   "C1SJCCS_sum",  "continuous", "NegF->W", "M3",
  "m14_cowork_negwf_m3",  "C3TEF",   "C1SJCCS_sum",  "continuous", "NegW->F", "M3",
  "m15_cowork_posfw_m3",  "C3TEF",   "C1SJCCS_sum",  "continuous", "PosF->W", "M3",
  "m16_cowork_poswf_m3",  "C3TEF",   "C1SJCCS_sum",  "continuous", "PosW->F", "M3",
  # MIDUS 3: Spouse
  "m17_spouse_negfw_m3",  "C3TEF",   "C1SSPEMP_sum", "continuous", "NegF->W", "M3",
  "m18_spouse_negwf_m3",  "C3TEF",   "C1SSPEMP_sum", "continuous", "NegW->F", "M3",
  "m19_spouse_posfw_m3",  "C3TEF",   "C1SSPEMP_sum", "continuous", "PosF->W", "M3",
  "m20_spouse_poswf_m3",  "C3TEF",   "C1SSPEMP_sum", "continuous", "PosW->F", "M3"
)

mod_label <- c(
  B1PRSEX = "Gender", C1PRSEX = "Gender",
  B1SJCSS_sum = "Supervisor Support", C1SJCSS_sum = "Supervisor Support",
  B1SJCCS_sum = "Coworker Support",   C1SJCCS_sum = "Coworker Support",
  B1SSPEMP_sum = "Spousal Support",   C1SSPEMP_sum = "Spousal Support"
)

# --- Helpers -----------------------------------------------------------------

# Test interaction term significance from the model
get_int_p <- function(model, ef, mod) {
  co <- summary(model)$coefficients
  int_name <- intersect(c(paste0(ef, ":", mod), paste0(mod, ":", ef)), rownames(co))
  if (length(int_name) == 0) return(NA_real_)
  co[int_name[1], "Pr(>|t|)"]
}

# Simple slopes via interactions::sim_slopes
probe_one <- function(spec) {
  model <- get(spec$model_name, envir = .GlobalEnv)
  int_p  <- get_int_p(model, spec$ef, spec$mod)

  if (spec$mod_type == "binary") {
    modx_vals <- sort(unique(model$model[[spec$mod]]))
  } else {
    modx_vals <- NULL  # sim_slopes defaults to mean +/- 1 SD
  }

  ss <- tryCatch(
    sim_slopes(model, pred = !!spec$ef, modx = !!spec$mod,
               modx.values = modx_vals, johnson_neyman = FALSE,
               confint = TRUE),
    error = function(e) NULL
  )

  jn <- if (spec$mod_type == "continuous") {
    tryCatch(
      johnson_neyman(model, pred = !!spec$ef, modx = !!spec$mod,
                     alpha = .05, plot = FALSE),
      error = function(e) NULL
    )
  } else NULL

  slopes_df <- if (!is.null(ss)) {
    as_tibble(ss$slopes) %>%
      rename_with(~ c("mod_value", "slope", "se", "ci_lo", "ci_hi", "t", "p")[seq_along(.)])
  } else tibble()

  list(spec = spec, int_p = int_p, slopes = slopes_df, jn = jn)
}

# --- Run all probes ----------------------------------------------------------
cat("\n=== Probing", nrow(probe_specs), "interaction models ===\n")
probe_results <- probe_specs %>%
  split(seq_len(nrow(.))) %>%
  map(probe_one)

# --- Long tidy table of every simple slope -----------------------------------
slopes_long <- map_dfr(probe_results, function(r) {
  if (nrow(r$slopes) == 0) return(NULL)
  r$slopes %>%
    mutate(model     = r$spec$model_name,
           wave      = r$spec$wave,
           outcome   = r$spec$outcome,
           moderator = mod_label[r$spec$mod],
           int_p     = r$int_p) %>%
    select(wave, outcome, moderator, model, int_p,
           mod_value, slope, se, ci_lo, ci_hi, t, p)
})

# --- Johnson-Neyman region summary -------------------------------------------
jn_summary <- map_dfr(probe_results, function(r) {
  if (is.null(r$jn)) return(NULL)
  bounds <- r$jn$bounds
  tibble(
    wave      = r$spec$wave,
    outcome   = r$spec$outcome,
    moderator = mod_label[r$spec$mod],
    model     = r$spec$model_name,
    int_p     = r$int_p,
    jn_lower  = if (length(bounds) >= 1) bounds[1] else NA_real_,
    jn_upper  = if (length(bounds) >= 2) bounds[2] else NA_real_
  )
})

# --- Save CSVs ---------------------------------------------------------------
write_csv(slopes_long, file.path(probe_dir, "simple_slopes_all.csv"))
write_csv(jn_summary,  file.path(probe_dir, "johnson_neyman_all.csv"))

# --- HTML table: only significant interactions (APA 7 formatted) -------------
sig_slopes <- slopes_long %>% filter(!is.na(int_p) & int_p < .05)

# Inline APA 7 helpers (07_midus_apa_tables.R may not be sourced yet here).
nb_minus_local <- function(s) gsub("-", "‑", s, fixed = TRUE)
nb_comma_local <- function(s) gsub(", ", ", ", s, fixed = TRUE)
esc_lt_local   <- function(s) gsub("<", "&lt;", s, fixed = TRUE)
fmt_p <- function(p) {
  ifelse(p < .001, "&lt; .001", sub("^0", "", sprintf("%.3f", p)))
}

if (nrow(sig_slopes) > 0) {
  sig_tbl <- sig_slopes %>%
    mutate(
      slope_fmt = nb_minus_local(sprintf("%.3f", slope)),
      se_fmt    = sprintf("%.3f", se),
      ci_fmt    = nb_comma_local(nb_minus_local(
                    sprintf("[%.3f, %.3f]", ci_lo, ci_hi))),
      p_fmt     = fmt_p(p),
      int_p_fmt = fmt_p(int_p),
      mod_value = nb_minus_local(sprintf("%.3f", mod_value))
    ) %>%
    select(Wave = wave, Outcome = outcome, Moderator = moderator,
           `Interaction <i>p</i>` = int_p_fmt,
           `Moderator value` = mod_value,
           `EF slope` = slope_fmt,
           `<i>SE</i>` = se_fmt,
           `95% CI` = ci_fmt,
           `<i>p</i>` = p_fmt)

  total_cols <- ncol(sig_tbl)

  apa_css <- paste0(
    "font-family: 'Times New Roman', Times, serif; ",
    "font-size: 11pt; border-collapse: collapse; margin: 0 auto; ",
    "border-top: 2px solid black; border-bottom: 2px solid black;"
  )
  css_body <- "border: none; padding: 2px 8px; white-space: nowrap;"
  css_head <- paste0("border: none; padding: 3px 8px; ",
                     "border-bottom: 1px solid black; white-space: nowrap;")

  full_note <- paste0(
    "Simple slopes of executive functioning at meaningful moderator values ",
    "(±1 <i>SD</i> for continuous moderators; observed levels for binary ",
    "moderators). Only models with a significant interaction term ",
    "(<i>p</i> &lt; .05) are listed. <i>SE</i> = standard error; ",
    "95% CI = 95% confidence interval."
  )

  html_tbl <- kable(
    sig_tbl, format = "html",
    caption = NULL,
    escape = FALSE,
    table.attr = paste0("style=\"", apa_css, "\"")
  ) %>%
    kable_classic(full_width = FALSE, html_font = "Times New Roman",
                  font_size = 11, position = "center",
                  lightable_options = "basic") %>%
    row_spec(0, bold = TRUE, extra_css = css_head) %>%
    row_spec(seq_len(nrow(sig_tbl)), extra_css = css_body)

  # Inject a colspan'd tfoot Note so it spans the table width on Word paste.
  tfoot_html <- paste0(
    "<tfoot>",
    "<tr><td colspan=\"", total_cols, "\" ",
    "style=\"border-top: 1px solid black; padding: 8px 8px 0 8px; ",
    "font-family: 'Times New Roman', Times, serif; font-size: 11pt; ",
    "text-align: left; white-space: normal;\">",
    "<em>Note.</em> ", full_note,
    "</td></tr>",
    "</tfoot>"
  )
  html_tbl[1] <- sub("</table>",
                     paste0(tfoot_html, "</table>"),
                     html_tbl[1], fixed = TRUE)

  save_kable(html_tbl, file.path(probe_dir, "simple_slopes_significant.html"))
} else {
  cat("No significant interactions to probe.\n")
}

# --- Interaction plots -------------------------------------------------------
# For every model: interact_plot of EF slope at moderator levels.
# For continuous moderators with a significant interaction: also Johnson-Neyman.

outcome_axis <- c(
  "NegW->F" = "Negative Work-to-Family Spillover",
  "NegF->W" = "Negative Family-to-Work Spillover",
  "PosW->F" = "Positive Work-to-Family Spillover",
  "PosF->W" = "Positive Family-to-Work Spillover"
)

zap_haven <- function(df) {
  as.data.frame(lapply(df, function(x) {
    if (inherits(x, "haven_labelled")) as.numeric(x) else x
  }))
}

outcome_arrow <- c(
  "NegW->F" = "Negative Work→Family",
  "NegF->W" = "Negative Family→Work",
  "PosW->F" = "Positive Work→Family",
  "PosF->W" = "Positive Family→Work"
)

# APA-style ggplot theme: Times New Roman, white background, only x/y axes,
# no minor grid, no panel border, bold title. `base_size` controls ALL text
# (title, axis titles, axis tick labels, legend) uniformly.
apa_theme <- function(base_size = 11) {
  theme_classic(base_size = base_size, base_family = "Times") +
    theme(
      plot.title       = element_text(size = base_size, face = "bold",
                                      hjust = 0),
      plot.subtitle    = element_text(size = base_size, hjust = 0),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size, color = "black"),
      axis.line        = element_line(color = "black", linewidth = 0.5),
      axis.ticks       = element_line(color = "black", linewidth = 0.4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "right",
      legend.title     = element_text(size = base_size),
      legend.text      = element_text(size = base_size),
      plot.margin      = margin(8, 10, 8, 10)
    )
}

# Subtitle string with italic <i>p</i> rendered via ggtext if available, else
# plain text. Using bquote for portable italic markup.
apa_int_subtitle <- function(int_p) {
  if (is.na(int_p))   return(bquote(italic(p)*" = NA"))
  if (int_p < .001)   return(bquote("Interaction "*italic(p)*" < .001"))
  bquote("Interaction "*italic(p)*" = "*.(sub("^0", "", sprintf("%.3f", int_p))))
}

make_plots <- function(r) {
  spec  <- r$spec
  model <- get(spec$model_name, envir = .GlobalEnv)
  d <- zap_haven(model$model)
  if (spec$mod %in% c("B1PRSEX", "C1PRSEX")) {
    d[[spec$mod]] <- factor(d[[spec$mod]], levels = c(1, 2),
                            labels = c("Male", "Female"))
  }
  model <- tryCatch(update(model, data = d), error = function(e) model)

  is_sig  <- !is.na(r$int_p) && r$int_p < .05
  out_dir <- if (is_sig) plot_sig_dir else plot_nsig_dir
  # Significant interaction plots use 12pt text (publication-ready);
  # non-significant supplementary plots keep the 11pt default.
  theme_size <- if (is_sig) 12L else 11L
  mod_lab <- unname(mod_label[spec$mod])
  out_lab <- unname(outcome_axis[spec$outcome])

  # APA-style figures: title/note belong in the figure caption, not the
  # plot panel. All interaction plots therefore render with no in-panel
  # title and no p-value subtitle.
  add_titles <- function(p) p + labs(title = NULL, subtitle = NULL)

  # --- Simple-slopes plot ---
  modx_vals <- if (spec$mod_type == "binary") {
    if (is.factor(model$model[[spec$mod]])) levels(model$model[[spec$mod]])
    else sort(unique(model$model[[spec$mod]]))
  } else NULL
  p_int <- tryCatch(
    add_titles(
      interact_plot(model, pred = !!spec$ef, modx = !!spec$mod,
                    modx.values = modx_vals,
                    interval = TRUE, int.width = 0.95,
                    x.label = "Executive Functioning",
                    y.label = out_lab,
                    legend.main = mod_lab,
                    colors = "Greys") +
        apa_theme(base_size = theme_size)
    ),
    error = function(e) {
      message("interact_plot failed for ", spec$model_name, ": ", e$message)
      NULL
    }
  )
  if (!is.null(p_int)) {
    ggsave(file.path(out_dir, paste0(spec$model_name, "_simpleslopes.png")),
           plot = p_int, width = 6, height = 4.5, dpi = 300, bg = "white")
  }

  # --- Johnson-Neyman plot (continuous moderators only) ---
  p_jn <- NULL
  if (spec$mod_type == "continuous") {

    # interactions::johnson_neyman() returns a plot with two distractions:
    # (1) a vertical-band "Range of observed data" annotation drawn as a
    #     text label, and (2) a legend whose title is the literal column
    #     reference (e.g., 'cbso1[, "Significance"]'). Strip the text/label
    #     layers and rename the legend "Significance".
    clean_jn <- function(p) {
      keep <- !vapply(p$layers, function(L) {
        inherits(L$geom, c("GeomText", "GeomLabel"))
      }, logical(1))
      p$layers <- p$layers[keep]
      p + labs(fill = "Significance", colour = "Significance")
    }

    # NB: pred/modx are swapped so EF ends up on the x-axis (consistent
    # with the simple-slopes plot). The plot now shows the conditional
    # slope of the moderator across values of EF — JN bounds delimit the
    # EF region where the moderator's effect is significant. No in-panel
    # title or subtitle (APA caption-only convention).
    add_jn_titles <- function(p) {
      p + labs(title = NULL, subtitle = NULL,
               x = "Executive Functioning",
               y = paste0(mod_lab, " slope"))
    }
    p_jn <- tryCatch({
      jn <- johnson_neyman(model, pred = !!spec$mod, modx = !!spec$ef,
                           alpha = .05, plot = TRUE)
      add_jn_titles(clean_jn(jn$plot) + apa_theme(base_size = theme_size))
    }, error = function(e) {
      message("JN plot failed for ", spec$model_name, ": ", e$message)
      NULL
    })
    if (!is.null(p_jn)) {
      ggsave(file.path(out_dir, paste0(spec$model_name, "_jn.png")),
             plot = p_jn, width = 6, height = 4.5, dpi = 300, bg = "white")
    }
  }

  # --- Combined two-panel figure: simple slopes (A) + Johnson-Neyman (B) ---
  # Only produced when both panels exist, i.e. continuous moderators with both
  # plots successfully rendered. Panels are labeled A and B per APA 7
  # multi-panel figure conventions.
  if (!is.null(p_int) && !is.null(p_jn)) {
    combined <- cowplot::plot_grid(
      p_int, p_jn,
      labels = c("A", "B"),
      label_fontfamily = "Times",
      label_fontface   = "bold",
      label_size       = 12,
      ncol = 2,
      align = "h", axis = "tb"
    )
    ggsave(file.path(out_dir, paste0(spec$model_name, "_combined.png")),
           plot = combined, width = 12, height = 4.5, dpi = 300, bg = "white")
  }
  invisible(NULL)
}

cat("Writing interaction plots to ", plot_dir, " ...\n", sep = "")
walk(probe_results, make_plots)

# --- Console summary ---------------------------------------------------------
n_sig_int <- probe_specs %>%
  mutate(int_p = map_dbl(probe_results, "int_p")) %>%
  summarise(n_sig = sum(int_p < .05, na.rm = TRUE),
            n_tot = sum(!is.na(int_p))) %>% as.list()

cat(sprintf("Interaction terms significant at p < .05: %d/%d\n",
            n_sig_int$n_sig, n_sig_int$n_tot))
cat("Wrote:\n",
    " - ", file.path(probe_dir, "simple_slopes_all.csv"), "\n",
    " - ", file.path(probe_dir, "johnson_neyman_all.csv"), "\n",
    if (n_sig_int$n_sig > 0)
      paste0(" - ", file.path(probe_dir, "simple_slopes_significant.html"), "\n")
    else "",
    " - PNG plots in ", plot_dir, "\n",
    sep = "")

# Sanity checks
stopifnot(
  "probe_specs missing rows"        = nrow(probe_specs) == 32,
  "slopes_long empty"               = nrow(slopes_long) > 0
)
cat("03c_probe_interactions checks passed.\n")
