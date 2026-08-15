# Built to Spill: Executive Functioning and Work-Family Spillover

Dissertation code, documents, and presentations. Analyses use MIDUS 2 and MIDUS 3 data to test main and moderated effects of executive functioning (BTACT) on work-family spillover.

## Replication

1. **Get data.** MIDUS data are not redistributed here. Download from [ICPSR](https://www.icpsr.umich.edu/web/ICPSR/series/203) (free account + terms of use) and place these SPSS files in `dissertation-code/`:
   - `M2_P1_SURVEY_N4963_20200720.sav`
   - `M2_P3_BTACT_N4512_20211123.sav`
   - `M3_P1_SURVEY_N3294_20181219.sav`
   - `M3_P3_BTACT_N3291_20210922.sav`
2. **Install R packages.**
   ```r
   install.packages(c("tidyverse", "haven", "broom", "interactions", "lavaan",
                      "semTools", "pwr", "WebPower", "knitr", "kableExtra",
                      "cowplot", "htmltools", "tidylog"))
   ```
3. **Run pipeline.** With `dissertation-code/` as the working directory (open `dissertation-code.Rproj`):
   ```r
   source("dissertation_code/00_run.R")
   ```
   Runs scripts 01–07 in order: data read, scoring, regressions, descriptives, interaction probing, multiverse, CFA/measurement invariance, power analysis, APA tables. Output lands in `output/` (tables + figures).

## Layout

- `dissertation-code/dissertation_code/` — analysis pipeline (entry: `00_run.R`)
- `dissertation-code/v1_pre_modifications/`, `v2_post_modifications/` — pipeline snapshots
- `dissertation-docs/` — dissertation drafts and final document
- `dissertation-presentations/` — proposal and defense slides
