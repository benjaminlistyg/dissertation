# Fix MASS::select masking dplyr::select (MASS is loaded by lavaan/semTools
# and persists across re-runs in the same session).
# Binding in the global env takes priority over any package namespace.
library(dplyr)
select <- dplyr::select
filter <- dplyr::filter

scripts <- c(
  "dissertation_code/01_midus_data_read.R",
  "dissertation_code/02_midus_scoring.R",
  "dissertation_code/03_regressions.R",
  "dissertation_code/03b_descriptives.R",
  "dissertation_code/03c_probe_interactions.R",
  "dissertation_code/04_multiverse.R",
  "dissertation_code/04b_multiverse_apa.R",
  "dissertation_code/05_cfa_invariance.R",
  "dissertation_code/06_power_analysis.R",
  "dissertation_code/07_midus_apa_tables.R"
)

n_scripts <- length(scripts)
pb <- txtProgressBar(min = 0, max = n_scripts, style = 3)
t0 <- Sys.time()

for (i in seq_along(scripts)) {
  cat(sprintf("\n[%d/%d] %s\n", i, n_scripts, scripts[i]))
  ti <- Sys.time()
  source(scripts[i])
  cat(sprintf("    ... done (%.1fs)\n",
              as.numeric(difftime(Sys.time(), ti, units = "secs"))))
  setTxtProgressBar(pb, i)
}
close(pb)

cat(sprintf("\nPipeline complete in %.1fs\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
