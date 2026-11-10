# vibe_work — Bike-Sharing demand analysis (AIN4741 group project)

Everything for the assignment lives in this folder. **Start with `report/report.html`.**

## Deliverables

| File | What it is |
| --- | --- |
| `report/report.html` | **The written report.** Methodology, results, conclusions, plus the full appendix (all R code and all R console output). Open it in a browser; the figures are linked from `output/figures/`. |
| `presentation/slides.html` | **The presentation** (20 slides). Open in a browser, navigate with ← → / space / Home / End. |
| `output/tables/*.csv`, `*.md` | The 42 tables of the report (T01 … T17) as CSV and as markdown. |
| `output/figures/*.png` | The 19 figures of the report (F01 … F17). |
| `output/logs/*.log` | The complete console output of each analysis script. |
| `R/*.R` | The analysis code. |
| `run_all.R` | Re-runs everything and rebuilds the report. |

## How to reproduce

From the project root (the folder that contains `vibe_work/`):

```
Rscript vibe_work/run_all.R
```

Requires R 4.6.1 with `tidyverse` (2.0.0), `easystats` (0.7.6) and `patchwork` (1.3.2).
Runtime: about 5 minutes (most of it is the SARIMA grid search and the rolling-origin evaluation).

## Package policy

As required, the only packages loaded are

```r
library(tidyverse)
library(easystats)
library(patchwork)
```

Everything else is **base R**: `lm()`, `arima()` (both SARIMA and regression with AR(1) errors),
`stl()`, `ts()`, `Box.test()`, `shapiro.test()`, plus three tests implemented by hand —
the augmented Dickey-Fuller regression, the Breusch-Pagan regression and the blocked cross-validation.

## Script map

| Script | Output |
| --- | --- |
| `R/00_setup.R` | libraries, paths, ggplot theme, helper functions (ADF, ACF/PACF, table writers) |
| `R/01_data_prep.R` | cleaning, codebook, descriptives, EDA → T01–T03, F01–F04 |
| `R/02_regression.R` | collinearity, subset selection, final model, diagnostics, forecast → T04–T10, F05–F10 |
| `R/03_timeseries.R` | STL, ADF, 36-model SARIMA search, diagnostics, forecast → T11–T17, F11–F17 |
| `report/build_report.R` | injects the code and the logs into `report/report_src.html` → `report/report.html` |

## Headline results

| | estimate for 2 Jan 2013 | 95% interval |
| --- | --- | --- |
| Regression model (weather + calendar) | **3,354** bikes | prediction interval [1,888; 4,820] |
| SARIMA(1,1,1)(0,1,1)₇ (history only) | **2,969** bikes | forecast interval [978; 4,959] |

Reference: the system averaged 3,121 rentals/day in January 2012.

## Data provenance

* `data/day.csv` — working copy of `bike+sharing+dataset/day.csv`.
* `data/dca_asos_2013-01-02.csv` — the 24 hourly METAR reports of Ronald Reagan Washington National
  Airport (DCA) on 2 January 2013, retrieved from the Iowa Environmental Mesonet ASOS archive. These are
  the exogenous inputs of the regression forecast; the daily means and the normalisation to the units of
  the dataset are computed in `R/02_regression.R` (Section 7).
