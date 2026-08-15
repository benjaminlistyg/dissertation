# Dissertation Pipeline — Session Memory

A comprehensive reference for the MIDUS Executive Functioning × Work–Family Spillover dissertation pipeline. Captures every modification, decision, and convention established during the most recent session so the next maintainer (or the next session) can pick up without re-deriving anything.

---

## 1. Top-level folder structure

```
dissertation-code/                       ← project root
├── dissertation_code/                   ← original pipeline (legacy, kept in sync)
│   ├── 00_run.R … 07_midus_apa_tables.R
│   └── output/                          ← legacy output target
├── v1_pre_modifications/                ← "baseline" version snapshot
│   ├── dissertation_code/
│   ├── data/                            ← symlinks to .sav files
│   └── output/                          ← populated by v1's pipeline run
├── v2_post_modifications/               ← current "edited" version
│   ├── dissertation_code/
│   ├── data/                            ← symlinks to .sav files
│   └── output/                          ← populated by v2's pipeline run
├── M2_P1_SURVEY_N4963_20200720.sav      ← raw data lives at project root
├── M2_P3_BTACT_N4512_20211123.sav
├── M3_P1_SURVEY_N3294_20181219.sav
├── M3_P3_BTACT_N3291_20210922.sav
└── memory.md                            ← this file
```

- **`dissertation_code/`** is the original codebase, kept in sync with v2 so it can serve as a "live" working copy.
- **`v1_pre_modifications/`** is intended to hold the *pre-modification* state (sum scoring, two household variables, uncentered predictors). Currently it is a *copy* of v2's scripts — Phase 2 (the actual revert) was not executed. To make v1 reflect the true baseline, all session edits would need to be undone there.
- **`v2_post_modifications/`** is the current state with every modification applied (centering, mean scoring, household consolidation, etc.). This is what's being actively developed.

### How each version is run

```r
# In R, with the working directory set to v2_post_modifications/dissertation_code/
source("00_run.R")
```

Both v1 and v2 are self-contained: `.sav` data symlinks live *inside* `dissertation_code/`, scripts reference siblings (no `dissertation_code/` prefix in `00_run.R`'s `scripts` vector), and `OUTPUT_DIR` is `file.path("..", "output", "tables")` so outputs land at the version root.

---

## 2. Pipeline architecture

`00_run.R` drives a `txtProgressBar` (style 3) that sources scripts in order and times each one:

| Step | Script | What it builds in memory | What it writes to disk |
|---|---|---|---|
| 1 | `01_midus_data_read.R` | `df_m2`, `df_m3` (raw merged from .sav files) | — |
| 2 | `02_midus_scoring.R` | Scored scales, centered variables, derived household var, analytic / dual-earner subsets | — |
| 3 | `03_regressions.R` | `m1`–`m20` (centered) and `m5`–`m20_raw` (uncentered) lm objects; `reg_ids_m{2,3}` | — |
| 4 | `03b_descriptives.R` | `descriptives_data` list (data + IDs + scale items) | — |
| 5 | `03c_probe_interactions.R` | `probe_results`, `slopes_long`, `jn_summary` | **CSVs, simple_slopes_significant.html, interaction PNGs** |
| 6 | `04_multiverse.R` | `results_m{2,3}`, `paired_df`, `mv_replicability` | — |
| 7 | `04b_multiverse_apa.R` | (auxiliary multiverse tables) | — |
| 8 | `05_cfa_invariance.R` | `fit_m{2,3}`, invariance fits | — |
| 9 | `06_power_analysis.R` | Power calculations | — |
| 10 | `07_midus_apa_tables.R` | (functions + auto-gen block) | **All HTML tables across `output/tables/`** |

### Critical convention

> `07_midus_apa_tables.R` defines all `apa_table_*()` functions **and** runs an auto-gen block at the bottom. Scripts 01–06 do NOT have access to functions in 07. Any table generation in 03–06 must be inlined or deferred to 07's `exists()`-gated auto-gen block.

---

## 3. Variable scoring (in `02_midus_scoring.R`)

### Pre-scored outcomes — rescaled to per-item means

MIDUS provides four spillover scales pre-scored as **4-item sums** (range 4–20). We divide each by 4 to put outcomes on a per-item Likert scale (~1–5), comparable to the now-averaged predictors. Standardized betas / p / R² are invariant.

```r
df_m2$B1SNEGWF <- df_m2$B1SNEGWF / 4    # Negative Work-to-Family Spillover
df_m2$B1SNEGFW <- df_m2$B1SNEGFW / 4
df_m2$B1SPOSWF <- df_m2$B1SPOSWF / 4
df_m2$B1SPOSFW <- df_m2$B1SPOSFW / 4
df_m3$C1SNEGWF <- df_m3$C1SNEGWF / 4
df_m3$C1SNEGFW <- df_m3$C1SNEGFW / 4
df_m3$C1SPOSWF <- df_m3$C1SPOSWF / 4
df_m3$C1SPOSFW <- df_m3$C1SPOSFW / 4
```

Individual spillover items (e.g., `B1SF27A`–`P`) are retained for CFA in `05_cfa_invariance.R`.

### Predictor scales — mean scoring (changed from sum)

| Scale | MIDUS 2 var | MIDUS 3 var | Items | Direction | Range |
|---|---|---|---|---|---|
| Job Demands | `B1SJCDS_sum` | `C1SJCDS_sum` | 5 | ↑ = more demands | 1–5 |
| Coworker Support | `B1SJCCS_sum` | `C1SJCCS_sum` | 2 | ↑ = more support | 1–5 |
| Supervisor Support | `B1SJCSS_sum` | `C1SJCSS_sum` | 3 | ↑ = more support | 1–5 |
| Spousal Support | `B1SSPEMP_sum` | `C1SSPEMP_sum` | 6 | ↑ = more support | 1–4 |
| Family Demands | `FAM_DEMANDS_M2` | `FAM_DEMANDS_M3` | 5 (POMP composite) | ↑ = more demands | 0–100 |

> **Naming note**: variable names still carry the `_sum` suffix even though they are now per-item means. This was a deliberate decision to avoid a cascade of edits across `var_labels`, descriptives, multiverse formulas, alpha item maps, etc. Display labels via `label_terms()` say "Job Demands" / "Supervisor Support" — the `_sum` suffix is only visible to code, not to readers.

#### Job Demands construction

5 items, Section F. Q29c/Q42c is already reverse-keyed in the survey; the other four get `6 - x`. Final result is `rowMeans(…, na.rm = FALSE)`.

```r
df_m2$B1SJCDS_sum <- rowMeans(cbind(
  6 - df_m2$B1SF28A,   # reverse
  6 - df_m2$B1SF28J,   # reverse
  6 - df_m2$B1SF29A,   # reverse
  df_m2$B1SF29C,       # already reverse-keyed (R)
  6 - df_m2$B1SF29D    # reverse
), na.rm = FALSE)
```

Items: M2 = `B1SF28A`, `B1SF28J`, `B1SF29A`, `B1SF29C`, `B1SF29D`; M3 = `C1SF36A`, `C1SF36J`, `C1SF42A`, `C1SF42C`, `C1SF42D`.

#### Coworker Support

2 items, all reverse-coded. M2: `B1SF30A`, `B1SF30B`. M3: `C1SF37A`, `C1SF37B`.

#### Supervisor Support

3 items, all reverse-coded. M2: `B1SF30C`, `B1SF30D`, `B1SF30E`. M3: `C1SF37C`, `C1SF37D`, `C1SF37E`.

#### Spousal Support

6 items, all reverse-coded via `5 - x` (because spousal items are on a 1–4 scale, not 1–5 like the workplace items). M2: `B1SL11A`–`B1SL11F`. M3: `C1SL12A`–`C1SL12F`.

#### Family Demands (POMP composite)

The only scale that combines items on **different rating scales** (5-point home-demand items + 4-point family-demand items). Solution: each item is reverse-coded, then put through `pomp(x, min, max)` which returns `100 * (x - min) / (max - min)`. The five POMP-transformed items are then averaged via `rowMeans`.

Home demand items (5-point): `B1SF33A`, `B1SF33C` (already reversed), `B1SF33D` (M2 equivalents on M3: `C1SF43A/C/D`).
Family demand items (4-point): `B1SJ2G`, `B1SL11G` (M3: `C1SJ4G`, `C1SL12G`).

### Derived household variable

```r
df_m2$B1PHHSIZE_oth <- df_m2$B1PHHSIZE - 1
df_m3$C1PHHSIZE_oth <- df_m3$C1PHHSIZE - 1
```

**Rationale**: `B1PHHSIZE = B1PC4 + B1PKIDHH + 1` (by survey construction — household size = other adults + kids + respondent). Including both `B1PHHSIZE` and `B1PC4` as covariates created near-perfect collinearity (VIFs to infinity in the no-missingness limit; very high in practice). We collapsed the pair into a single derived variable representing household members other than the respondent (adults + kids combined).

`B1PC4` / `C1PC4` ("Other Household Members") is now dropped from every regression formula, the `var_labels` map, and the descriptives. Display label: "Household Size".

### Mean-centered variables (`_c` suffix)

Centering is applied to **the focal predictor and the moderators that enter interaction terms** so that the "EF main effect" coefficient in moderation models represents the EF slope at the moderator's *mean* rather than at moderator = 0. Specifically centered:

- Executive Functioning: `B3TEFZ1_c`, `C3TEF_c`
- Supervisor Support: `B1SJCSS_c`, `C1SJCSS_c`
- Coworker Support: `B1SJCCS_c`, `C1SJCCS_c`
- Spousal Support: `B1SSPEMP_c`, `C1SSPEMP_c`
- Job Demands: `B1SJCDS_c`, `C1SJCDS_c`
- Family Demands: `FAM_DEMANDS_M2_c`, `FAM_DEMANDS_M3_c`

**Left raw** (per explicit user decision):
- Age, Education, Income, Household Size — entered as raw covariates in regressions
- Gender — binary 0/1 (centering doesn't help)

Centered variants live on `df_m{2,3}_analytic` and `df_m{2,3}_dual_earner` and carry through `filter()` into `df_m{2,3}_all`.

---

## 4. Sample construction (in `03_regressions.R`)

```r
base_vars_m2 <- c("B1SNEGWF", "B1SNEGFW", "B1SPOSWF", "B1SPOSFW",
                  "B3TEFZ1", "B1SJCDS_sum", "FAM_DEMANDS_M2",
                  "B1STINC1", "B1PHHSIZE_oth", "B1PAGE_M2", "B1PRSEX", "B1PB1")
all_vars_m2 <- c(base_vars_m2, "B1SJCSS_sum", "B1SJCCS_sum", "B1SSPEMP_sum")

df_m2_all <- df_m2_analytic %>% filter(complete.cases(across(all_of(all_vars_m2))))
```

`df_m{2,3}_all` are the listwise-complete regression samples used in all primary regressions (n ≈ 1289 / 729). They include the M2ID column for downstream sample alignment.

```r
reg_ids_m2 <- df_m2_all$M2ID
reg_ids_m3 <- df_m3_all$M2ID
```

These ID vectors are then used in `03b_descriptives.R` to filter `descriptives_data` to exactly the regression respondents, so that descriptive statistics and Cronbach's alphas are computed on the same N as the regressions.

---

## 5. Regression model objects

### Naming convention

| Pattern | What it is |
|---|---|
| `m1_negwf_m2` … `m4_posfw_m2` | Main effects (H1–H4), MIDUS 2 |
| `m1_negwf_m3` … `m4_posfw_m3` | Main effects, MIDUS 3 |
| `m5_gender_*_m2/m3` | Gender × EF moderation |
| `m9_super_*_m2/m3` | Supervisor Support × EF |
| `m13_cowork_*_m2/m3` | Coworker Support × EF |
| `m17_spouse_*_m2/m3` | Spousal Support × EF |
| `m{5–20}_*_m{2,3}_raw` | Uncentered (raw-variable) sibling of each interaction model |

Within each block, the `_negfw_`, `_negwf_`, `_posfw_`, `_poswf_` suffix is the outcome (`negfw` = Negative Family-to-Work, etc.).

### Centered vs. raw interaction models

All interaction `lm()` formulas in v2 use the centered (`_c`) versions of EF and the moderator. Example:

```r
m9_super_negfw_m2 <- lm(B1SNEGFW ~ B3TEFZ1_c * B1SJCSS_c +
                          B1PRSEX + B1SJCDS_sum + FAM_DEMANDS_M2 +
                          B1STINC1 + B1PHHSIZE_oth + B1PAGE_M2 + B1PB1,
                        data = df_m2_all)
```

Main-effects models (`m1`–`m4`) still use raw `B3TEFZ1` / `C3TEF` — centering an isolated main effect only shifts the intercept.

### Parallel `_raw` models (uncentered sensitivity comparison)

`03_regressions.R` builds 32 `_raw` sibling models via a `make_raw()` helper at the bottom of the script:

```r
.raw_var_swaps <- c(
  "B3TEFZ1_c"        = "B3TEFZ1",
  "C3TEF_c"          = "C3TEF",
  "B1SJCSS_c"        = "B1SJCSS_sum",
  # … etc
  "FAM_DEMANDS_M3_c" = "FAM_DEMANDS_M3"
)

make_raw <- function(centered_model) {
  f_text <- paste(deparse(formula(centered_model)), collapse = " ")
  for (k in names(.raw_var_swaps)) {
    f_text <- gsub(paste0("\\b", k, "\\b"), .raw_var_swaps[[k]], f_text)
  }
  update(centered_model, as.formula(f_text))
}

for (.nm in .interaction_models) {
  if (exists(.nm)) assign(paste0(.nm, "_raw"), make_raw(get(.nm)))
}
```

Result: each interaction model exists in two forms in memory:
- `m9_super_negfw_m2` — centered
- `m9_super_negfw_m2_raw` — raw

`07`'s auto-gen block writes two parallel output trees (see Output section below).

---

## 6. Descriptives (`03b_descriptives.R`)

### `desc_vars` (and matching `desc_labels`)

15 variables in presentation order: 4 spillover DVs → EF → 4 covariates (Job Demands, Family Demands, Income, Household Size) → 3 moderators (Supervisor, Coworker, Spousal Support) → 3 demographics (Age, Gender, Education).

### `scale_items_m{2,3}` lists

Item maps used by `07`'s `apa_table_descriptives()` to compute Cronbach's α. Maps each multi-item scale → vector of its raw item names. Used by `psych::alpha(check.keys = TRUE, use = "complete.obs")`.

### Sample-alignment stopifnots

```r
desc_data_m2 <- df_m2_analytic %>% filter(M2ID %in% reg_ids_m2)
stopifnot(
  setequal(desc_data_m2$M2ID, reg_ids_m2),
  nrow(desc_data_m2) == length(reg_ids_m2)
)
```

These guard against drift between the descriptives sample and the regression sample.

---

## 7. Multiverse (`04_multiverse.R`)

### Arms

- **Arm A**: covariate variations of main-effects models
  - "No covariates (EF only)"
  - "Demographic covariates only (no work or family demand variables)"
  - "Education-only SES indicator (income excluded)"
- **Arm A2** (added this session — overcontrol-bias check): drop-set series
  - "All covariates except job demands and family demands"
  - "All covariates except household variables (income, size)"
  - "All covariates except respondent demographics (age, gender, education)"
- **Arm B**: covariates entered as moderators
  - "Full covariate model with EF × Age interaction" (Age uncentered)
  - "Full covariate model with EF × Education interaction"
  - "Full covariate model with EF × Income interaction"
  - "Full covariate model with EF × Job Demands interaction"
  - "Full covariate model with EF × Family Demands interaction"
- **Arm C**: alternative inclusion criteria (dual-earner subsample)
  - "Dual-earner subsample: main effects"
  - "Dual-earner subsample: EF × Gender moderation"
  - "Dual-earner subsample: EF × Supervisor support moderation"
  - "Dual-earner subsample: EF × Coworker support moderation"
  - "Dual-earner subsample: EF × Spousal support moderation"
- **Arm D**: model-set-specific sample (alternative listwise filtering per moderator set)

### Replicability data (`mv_replicability`)

Pairs M2 and M3 effects across multiverse specifications, applies OSC (2015) criteria. Used by `07`'s `apa_table_replicability()` to produce both:
- `replicability_multiverse.html` (default "W1/W2" labels)
- `multiverse_cross_wave.html` (custom "MIDUS 2 / MIDUS 3" labels, split Specification + Outcome columns, NWF/NFW/PWF/PFW acronyms, EF abbreviation)

---

## 8. Interaction probing (`03c_probe_interactions.R`)

### `probe_specs` table

Catalog of all 32 interaction models with their `ef` and `mod` columns. **These columns reference the centered variable names** (`B3TEFZ1_c`, `B1SJCSS_c`, etc.) because v2 fits the models with centered predictors. Gender (`B1PRSEX` / `C1PRSEX`) is uncentered.

### `mod_label` map

Translates moderator variable codes (both centered `_c` and raw `_sum` aliases) to human labels ("Supervisor Support", etc.).

### Sim-slopes computation

```r
sim_slopes(model, pred = !!spec$ef, modx = !!spec$mod,
           modx.values = modx_vals, johnson_neyman = FALSE, confint = TRUE)
```

For continuous moderators, `modx.values` defaults to mean ± 1 SD.

### Johnson-Neyman convention

`pred` and `modx` are **swapped** so EF appears on the x-axis of the JN plot (instead of the moderator). Interpretation: the JN bound demarcates the region of EF where the moderator's effect is significant.

```r
jn <- johnson_neyman(model, pred = !!spec$mod, modx = !!spec$ef,
                     alpha = .05, plot = TRUE)
```

### `clean_jn()`

Post-processes the `interactions::johnson_neyman()` plot to strip text/label layers (the "Range of observed data" annotation) and rename the auto-generated legend title (`cbso1[, "Significance"]`) to just "Significance".

### Plot styling

Custom `apa_theme(base_size)` — Times New Roman, white background, only x/y axes, no minor grid, no panel border, all text elements uniformly sized at `base_size`. Called with `base_size = 12` for significant plots, `base_size = 11` for non-significant. Significant plots also have title/subtitle suppressed (APA convention: caption goes in the manuscript text, not in the figure panel).

### Combined panel figures

For every continuous-moderator interaction with successful simple-slopes + JN plots:
```r
cowplot::plot_grid(p_int, p_jn,
                   labels = c("A", "B"),
                   label_fontfamily = "Times",
                   label_fontface = "bold",
                   label_size = 12,
                   ncol = 2, align = "h", axis = "tb")
```
Saved as `<model>_combined.png` (12 × 4.5 in, 300 dpi).

---

## 9. APA 7 table styling (in `07_midus_apa_tables.R`)

### Universal pattern

Every `apa_table_*()` function follows the same recipe:

1. `kable_classic(lightable_options = "basic")` — no body row borders, no vertical rules
2. Per-cell `extra_css` with `border: none` so only the explicit APA rules survive:
   - Top: 2px solid black (on `<table>` AND on cells of first row — Word ignores table-level borders on paste; cell borders survive)
   - Under header: 1px solid black
   - Optional midrule above fit-stat block: 1px
   - Bottom: 2px solid black
3. Statistical symbols italicized inline: `<i>B</i>`, `<i>SE</i>`, `<i>t</i>`, `<i>p</i>`, `<i>R</i>²`, `<i>N</i>`, etc. β stays upright (Greek convention).
4. *Note* injected via a hand-built `<tfoot><tr><td colspan="N">…</td></tr></tfoot>` (not `kableExtra::footnote()`) so it spans the full table width on Word paste. General note first, then probability note on its own line (`<br>` separator).
5. `<` escaped to `&lt;` everywhere (necessary because `escape = FALSE` is set so `<em>` survives, which means raw `<` in p-values like `< .001` would be parsed as a tag opener).
6. Numerics use non-breaking minus (`‑`, U+2011) and non-breaking space inside CI brackets so values don't break across lines in Word.
7. `kableExtra::footnote()` calls anywhere in the file are explicitly namespaced (`kableExtra::footnote(...)`) so they're robust to other packages masking the symbol.

### `apa7_css()` helper

Returns a list of CSS strings:
- `$table` — font-family, font-size, `border-collapse: collapse`, top + bottom borders
- `$body` — cell defaults (no border, padding, `white-space: nowrap`)
- `$body_wrap` — same but allows wrapping (used for label columns)
- `$head` — cell defaults + 2px top + 1px bottom rules
- `$mid` — for the row above a fit-stat block (1px top rule)
- `$last_body` — for the last body row (2px bottom rule)

### `apa7_tfoot()` helper

Injects a colspan'd `<tfoot>` with the *Note* spanning the full table width. Modifies the kable string in place (`k[1] <- sub("</table>", ..., k[1])`) so the `lightable` / `lightable_class` attributes are preserved for `save_kable()`.

### `var_labels` map

Master lookup table translating variable codes → human labels. Used by `label_terms()` everywhere. Includes:
- M2 raw codes
- M3 raw codes
- Interaction terms (`B3TEFZ1:B1PRSEX`, etc.) — both raw and centered orderings (`A:B` and `B:A`)
- Centered variable aliases (`B3TEFZ1_c`, `C1SJCSS_c`, etc.) added in this session so v2's centered predictors display correctly

### Per-table function summary

| Function | Cols | Notes |
|---|---|---|
| `apa_table_single` | Predictor + B + β + SE + t + p + 95% CI (7 cols) | Optional `outcome_label` injects centered italic-bold colspan'd row at top |
| `apa_table_multi` | (DEPRECATED — replaced by single-model tables in v2) | Earlier rewritten to long format, then to 2×2 grid, then to single unified table; final decision was to drop it in favor of one HTML per model |
| `apa_table_multiverse` | Specification + Outcome + Term + B + β + SE + p + 95% CI + Sign + Sig + CI (11 cols) | Outcome acronyms NWF/NFW/PWF/PFW; EF instead of "Executive Functioning"; OSC criteria added (each row compared to first row for that outcome/term); summary block with Step 1/2/3 counts |
| `apa_table_multiverse_wide` | Specification + per-outcome blocks | |
| `apa_table_replicability` | Model + Term + W1 B [95% CI] + W2 B [95% CI] + Sign + Sig + CI (7 cols, OR 8 with `split_model = TRUE`) | OSC summary block at bottom; `wave_labels` param; `col_widths` param injects `<colgroup>`; `split_model` separates Model into Specification + Outcome columns |
| `apa_table_descriptives` | Variable + n + M + SD + Min + Max + α (7 cols) | Sample locked to `ids` (regression IDs); standardized α via `psych::alpha(check.keys = TRUE, use = "complete.obs")` |
| `apa_table_correlations` | Variable + 15 numeric cols (lower triangle) | |
| `apa_table_cfa_fit`, `apa_table_cfa_loadings`, `apa_table_cfa_cors`, `apa_table_invariance` | various | All converted to APA 7 |
| `apa_table_power_mde`, `apa_table_power_fixed`, `apa_table_power_beta` | various | All converted to APA 7 |

### Caption suppression

`apa_caption(N, title)` was set to return `NULL` so no in-table "Table N" caption appears. Every `caption = title` argument across the 13 `kable()` call sites was changed to `caption = NULL`. Outcome identity is conveyed by filenames and by external dissertation captions.

---

## 10. Output structure

### Centered (primary) results — `v2_post_modifications/output/`

```
output/
├── tables/
│   ├── regressions/                            ← 40 per-model HTML files
│   │   ├── table01_main_effects_m2_a_negwf.html
│   │   ├── table01_main_effects_m2_b_negfw.html
│   │   ├── table01_main_effects_m2_c_poswf.html
│   │   ├── table01_main_effects_m2_d_posfw.html
│   │   ├── table02_main_effects_m3_a-d_*.html
│   │   ├── table03_gender_moderation_m2_a-d_*.html
│   │   ├── … etc through table10_spousal_moderation_m3_d_poswf.html
│   ├── regressions_uncentered/                 ← 32 _raw sibling files
│   │   ├── table03_gender_moderation_m2_a_negfw_raw_uncentered.html
│   │   └── … etc (only interactions, not main effects)
│   ├── multiverse/
│   │   ├── multiverse_long_m{2,3}.html
│   │   └── multiverse_wide_m{2,3}.html
│   ├── replicability/
│   │   ├── replicability_regressions.html
│   │   ├── replicability_multiverse.html
│   │   └── multiverse_cross_wave.html          ← new this session
│   ├── descriptives/
│   │   ├── descriptives_m{2,3}.html
│   │   └── correlations_m{2,3}.html
│   ├── cfa/
│   ├── power/
│   └── interactions/                           ← from 03c
│       ├── simple_slopes_all.csv
│       ├── johnson_neyman_all.csv
│       └── simple_slopes_significant.html
├── figures/
│   └── interactions/
│       ├── significant/                        ← 12pt, no title, EF on x-axis
│       │   ├── m{N}_*_simpleslopes.png
│       │   ├── m{N}_*_jn.png
│       │   └── m{N}_*_combined.png             ← cowplot 2-panel
│       └── not-significant/                    ← 11pt, with title
└── consolidated-results/
    ├── results.html                             ← writeup + inline tables
    ├── results.docx                             ← native flextable + officer
    └── table_01–45_*.html                       ← individual numbered tables
```

### Output paths in code

```r
# In 07_midus_apa_tables.R (v2)
OUTPUT_DIR <- file.path("..", "output", "tables")

DIRS <- list(
  regressions             = file.path(OUTPUT_DIR, "regressions"),
  regressions_uncentered  = file.path(OUTPUT_DIR, "regressions_uncentered"),
  multiverse              = file.path(OUTPUT_DIR, "multiverse"),
  cfa                     = file.path(OUTPUT_DIR, "cfa"),
  power                   = file.path(OUTPUT_DIR, "power"),
  descriptives            = file.path(OUTPUT_DIR, "descriptives"),
  replicability           = file.path(OUTPUT_DIR, "replicability")
)
```

Because v2 is sourced from inside `dissertation_code/`, `..` resolves to the version root (`v2_post_modifications/`), and outputs land at `v2_post_modifications/output/tables/…`.

### Auto-gen block structure in `07`

```r
if (exists("m1_negwf_m2")) {
  reg_specs <- list(
    list(n=1, models=list(m1_negwf_m2, m2_negfw_m2, m3_poswf_m2, m4_posfw_m2),
         names=c("Neg W→F", "Neg F→W", "Pos W→F", "Pos F→W"),
         title="Main Effects of Executive Functioning on Work-Family Spillover (MIDUS 2)",
         file="table01_main_effects_m2"),
    # … table02 (M3 main effects), 03–04 (gender), 05–06 (super), 07–08 (cowork), 09–10 (spouse)
  )

  outcome_slug_map <- c("Neg W→F"="negwf", "Neg F→W"="negfw",
                        "Pos W→F"="poswf", "Pos F→W"="posfw")
  outcome_letter <- c("a", "b", "c", "d")

  for (spec in reg_specs) {
    for (j in seq_along(spec$models)) {
      slug <- outcome_slug_map[spec$names[j]]
      apa_save_html(
        apa_table_single(spec$models[[j]]),
        paste0(spec$file, "_", outcome_letter[j], "_", slug, ".html"),
        DIRS$regressions)
    }
  }

  # Parallel uncentered output for interaction tables (3–10 only)
  for (spec in reg_specs) {
    if (!grepl("moderation", spec$file, fixed = TRUE)) next
    for (j in seq_along(spec$models)) {
      raw_mod <- NULL
      for (cand in ls(envir = .GlobalEnv, pattern = "^m[0-9]+_.*_raw$")) {
        if (identical(get(sub("_raw$", "", cand), envir = .GlobalEnv),
                      spec$models[[j]])) {
          raw_mod <- get(cand, envir = .GlobalEnv); break
        }
      }
      if (is.null(raw_mod)) next
      apa_save_html(
        apa_table_single(raw_mod),
        paste0(spec$file, "_", outcome_letter[j], "_",
               outcome_slug_map[spec$names[j]], "_raw_uncentered.html"),
        DIRS$regressions_uncentered)
    }
  }
}
```

---

## 11. Consolidated results document

`build_docx.R` (a one-off builder script, not part of `00_run.R`) constructs a native Word document at `output/consolidated-results/results.docx`. It:

1. Sources the upstream pipeline (01 → 02 → 03 → 03b → 07)
2. Builds 45 `flextable` objects (one per table) with APA 7 styling
3. Composes narrative paragraphs via `officer::fpar` with inline italics for statistical symbols
4. Inserts a portrait section break, switches to landscape, then emits one table per page

### Document layout

| Section | Page orientation | Margins |
|---|---|---|
| Narrative | Portrait Letter | 1″ |
| Tables (1 per page) | Landscape Letter | 0.5″ |

### Narrative structure (outcome-by-outcome, M2 → M3)

Each section heading covers one moderator group (Main Effects, Gender, Supervisor, Coworker, Spousal, Replicability), with paragraphs organized by outcome — first outcome in M2 + M3 in a single flowing paragraph, then second outcome, etc. (Per explicit user feedback that the alternative — italic "*MIDUS 2 (Table N).*" lead-in style — read awkwardly.)

### Tables 1–45

| Tables | Content |
|---|---|
| 1–2 | Descriptives M2, M3 |
| 3–4 | Correlations M2, M3 |
| 5–12 | Main effects (4 outcomes × 2 waves) |
| 13–20 | Gender × EF (4 outcomes × 2 waves) |
| 21–28 | Supervisor × EF |
| 29–36 | Coworker × EF |
| 37–44 | Spousal × EF |
| 45 | Cross-Wave Replicability |

Each table uses generous column widths so 12pt content stays on a single line.

---

## 12. Convention decisions made this session

### What stays raw vs. centered

User-decided: center **only** EF + Supervisor + Coworker + Spousal + Job Demands + Family Demands. **Do not** center Age, Education, Income, Household Size, or Gender.

### Sum vs. mean scoring

User-decided: use means for Job Demands, Coworker, Supervisor, Spousal (variable names retained as `_sum`). Family Demands was already a POMP mean. Spillover DVs divided by 4 to convert MIDUS-provided sums to per-item means.

### Two-version folder layout

v1 = baseline (intended), v2 = current modifications. Phase 1 (folder structure) is done. Phase 2 (actually reverting v1 to baseline) was not performed in this session.

### "No Viewer output" policy

`apa_save_html()` no longer calls `print(tbl)` to the RStudio Viewer. The kable-print block at the bottom of `03_regressions.R` was also removed. Tables save to disk only.

### APA 7 table format

No in-table "Table N" caption. Outcome label optionally injected as a centered italic-bold colspan'd row at the top of individual tables. Statistical symbols italicized inline. Body cells nowrap; label columns wrap. Three rules only (top 2px / header 1px / bottom 2px) plus optional 1px interior rule above fit-stat block. Notes injected via custom `<tfoot>`.

### One HTML per model

Each of the 40 regression models gets its own HTML file (instead of 4-model combined tables). Filenames pattern: `tableNN_<spec>_m{2,3}_{a-d}_<outcomeslug>.html`.

### Multiverse outcome acronyms

`NEGWF` → NWF, `NEGFW` → NFW, `POSWF` → PWF, `POSFW` → PFW. Defined in the table's Note. Used in `multiverse_long_m{2,3}.html` and `multiverse_cross_wave.html`. "Executive Functioning" is also abbreviated to "EF" in the Term column of those tables.

### OSC (2015) criteria columns

Three boolean-style criterion columns (Sign / Sig / CI) are appended to multiverse_long and replicability tables:
- ✓ if criterion met
- ✗ if not met
- — if not applicable (reference row, or reference effect not significant)

Summary block at the bottom of each table shows Step 1 / Step 2 / Step 3 counts and percentages.

### Display label cleanup

"Household Size (excl. respondent)" was simplified to just "Household Size" in all `var_labels` maps and existing HTML output files.

---

## 13. Known issues & gotchas

### `03c` requires `probe_specs$ef` / `mod` to match the variable names actually used in fitted models

If v2's models switch from raw to centered (or vice versa), `03c`'s catalog must be updated in parallel. Currently set to centered for v2. The fix is in `probe_specs$ef = "B3TEFZ1_c"` (etc.) and `mod_label` includes both centered and raw aliases.

### `flextable::footnote` masks `kableExtra::footnote`

Earlier in the session, `library(flextable)` calls in the (since-removed) `apa_word_*()` functions caused `kableExtra::footnote()` to break in all downstream `apa_table_*()` functions. All `footnote()` calls in `07` are now explicitly namespaced as `kableExtra::footnote()` to be robust to future namespace masking.

### `save_kable` HTML write path doesn't use pandoc

Confirmed: no script in the pipeline calls pandoc. The earlier "+RTS: command not found" / pandoc errors traced back to RStudio's Viewer-rendering subsystem, which is no longer invoked now that `print(tbl)` has been removed from `apa_save_html()` and `03_regressions.R`.

### Symlinks for data files

`.sav` files are symlinks inside each `v{1,2}_*/dissertation_code/` so `haven::read_sav("M2_…sav")` (no path prefix) finds them in the working directory. If you move the project tree, the symlinks (which point to absolute paths at the original project root) will break and need to be recreated.

### `B1PHHSIZE_oth` is built on the raw `df_m{2,3}` (before sample restrictions)

This is intentional so it propagates through `df_m_analytic` → `df_m_all` → `df_m_dual_earner` via `filter()`. Don't move the construction down past the sample-filtering block.

### Centered variables (`_c`) live on `df_m{2,3}_analytic` and `df_m{2,3}_dual_earner`

Built **after** the sample restriction (`filter(B1SF13 == 1)`). The centering mean is the analytic-sample mean, not the larger merged-data mean. Since `df_m_all` is essentially `df_m_analytic` minus a small handful of cases with missingness on the broader predictor set, the analytic-sample mean is a close approximation to the regression-sample mean. Acceptable for centering purposes.

### Output directory path is relative to working directory

`OUTPUT_DIR <- file.path("..", "output", "tables")`. Must be sourced from inside `dissertation_code/`. If you accidentally source from the version root, outputs land at `v2_post_modifications/../output/…` which is `dissertation-code/output/…` (the *project-root* output, not the version output) — a silent mis-routing.

---

## 14. Quick command reference

### Run the full v2 pipeline

```r
# In R, with cwd = v2_post_modifications/dissertation_code/
source("00_run.R")
```

### Rebuild just the Word document

```r
# In R, cwd = project root
source("dissertation_code/build_docx.R")
```

### Spot-check a single table

```r
# In R, with the relevant model object in scope
apa_save_html(apa_table_single(m9_super_negfw_m2),
              "test.html", DIRS$regressions)
```

### Probe a single interaction

```r
sim_slopes(m9_super_negfw_m2, pred = B3TEFZ1_c, modx = B1SJCSS_c)
johnson_neyman(m9_super_negfw_m2, pred = B1SJCSS_c, modx = B3TEFZ1_c,
               alpha = .05, plot = TRUE)
```

(Note `pred` and `modx` swapped for the JN call so EF ends up on the x-axis.)

### Re-apply label cleanup to an HTML output file (if `var_labels` ever drifts)

```bash
perl -i -pe '
  s/B3TEFZ1_c\b/Executive Functioning/g;
  s/B1SJCSS_c\b/Supervisor Support/g;
  # … etc
' path/to/output.html
```

---

## 15. Pending / out-of-scope items

These came up in this session but were not completed:

- **Phase 2 of versioning**: actually revert `v1_pre_modifications/dissertation_code/` to a true pre-modifications state (un-do centering, restore the two-variable household covariate, restore sum scoring, remove `_raw` infrastructure, etc.). Currently v1 is a *copy* of v2 with only the path-prefix fixes applied.
- **GitHub commit & push** to `https://github.com/benjaminlistyg/dissertation.git`. Not initialized or pushed during this session.
- **Multiverse cross-wave for `_raw` models**: the raw-model HTMLs in `regressions_uncentered/` are emitted, but no cross-wave / OSC comparison is computed for the raw versions.

---

*End of session memory.*
