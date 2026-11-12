# vibe_work_ts — Hourly bike-sharing demand: decomposition and fable forecasting

A second, independent time-series study of the same bike-sharing system, this time on the **hourly**
file. **Start with `report/report.html`.**

## Deliverables

| File | What it is |
| --- | --- |
| `report/report.html` | **The written report** — data study, multi-seasonal decomposition, model selection (including the hold-out trap), the final model, diagnostics, the forecast for 17:00 on 2 January 2013, and the complete appendix of R code and R console output. |
| `presentation/slides.html` | **The presentation** (20 slides). Open in a browser; ← → / space / Home / End to navigate. |
| `output/tables/*.csv`, `*.md` | The 18 tables of the report (T00 … T14). |
| `output/figures/*.png` | The 17 figures of the report (F01 … F17). |
| `output/logs/*.log` | The complete console output of each script. |
| `R/*.R` | The analysis code. |
| `run_all.R` | Re-runs everything and rebuilds the report. |

## How to reproduce

From the project root (the folder that contains `vibe_work_ts/`):

```
Rscript vibe_work_ts/run_all.R
```

Requires R 4.6.1 with `tidyverse` (2.0.0), `easystats` (0.7.6), `patchwork` (1.3.2), `tsibble` (1.2.0),
`fable` (0.5.0), `feasts` (0.5.0) and `ggfortify` (0.4.24). Runtime ≈ 8 minutes (the automatic ARIMA on
17,208 hourly observations and the 57-origin rolling evaluation dominate).

## Package policy

As required, the only packages loaded are

```r
library(tidyverse); library(easystats); library(patchwork)
library(tsibble);   library(fable);     library(feasts); library(ggfortify)
```

Everything else is base R: `Box.test()`, `acf()`/`pacf()`, `shapiro.test()`, `ts()`, `decompose()`,
and the rolling-origin loop.

## Script map

| Script | Output |
| --- | --- |
| `R/00_setup.R` | libraries, paths, ggplot theme, helper functions |
| `R/01_data_prep.R` | hour.csv → regular hourly tsibble (165 missing hours interpolated and flagged), completeness, codebook, EDA → T00–T04, F01–F04 |
| `R/02_decomposition.R` | multi-seasonal STL (24 h + 168 h), seasonal indices, `gg_season`, `gg_subseries`, classical decomposition with `ggfortify` → T05–T06, F05–F09 |
| `R/03_models.R` | Guerrero/Box-Cox, KPSS & unit-root tests, 11 candidate models, single hold-out + 57-origin rolling evaluation, selection → T07–T09, F10–F13 |
| `R/04_forecast.R` | final model on all data, diagnostics, forecast for 2 Jan 2013, sensitivity → T10–T14, F14–F17 |
| `report/build_report.R` | injects code + logs into `report_src.html` → `report.html` |

## Headline results

| | value |
| --- | --- |
| **17:00 on Wed 2 Jan 2013** | **386 rentals** (median), 95% interval **[149; 996]**, expected value 434 |
| implied daily total for that day | **3,406** rentals (sum of the 24 hourly medians) |
| final model | `log(cnt) ~ hour-of-week (168 dummies) + holiday + trend + 4 annual Fourier harmonics` |
| fit | R² 0.990 (log scale) / 0.838 (count scale); RMSE 73 bikes/hour |

## Three findings worth reading the report for

1. **The dataset's `season` coding contradicts its own description.** Cross-tabulating month against season
   shows that the four levels are the astronomical quarters starting 21 December, and the mean temperatures
   (12.2 / 22.3 / 29.0 / 17.3 °C) identify them as winter, spring, summer, autumn — the labels on the
   dataset page are shifted by one. See report §1.3.
2. **The hourly file is not complete**: 165 of the 17,544 calendar hours (0.94%) are absent, concentrated in
   the early morning and in January 2011 / October 2012. They are interpolated and flagged.
3. **A single hold-out window selects the wrong model here.** The last two weeks of December 2012 contain the
   Christmas collapse, which rewards the model *without* a trend. Across 57 rolling origins the trend version
   is far better (RMSE 90 vs 116) and nearly unbiased. See report §4.2.

## Data provenance

`data/hour.csv` is a working copy of `bike+sharing+dataset/hour.csv` (17,379 rows, 2011-01-01 to
2012-12-31, columns as documented in the dataset Readme).
