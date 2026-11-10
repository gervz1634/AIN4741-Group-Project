###############################################################################
## build_report.R -- assembles report.html by injecting the R code and the R
## console output (the .log files produced by 01/02/03) into report_src.html.
## Base R only.  Run from the project root:  source("vibe_work/report/build_report.R")
###############################################################################

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

read_utf8 <- function(f) {
  if (!file.exists(f)) return(sprintf("[[ file %s not found ]]", f))
  paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

scripts <- c(
  "00_setup.R"      = "vibe_work/R/00_setup.R",
  "01_data_prep.R"  = "vibe_work/R/01_data_prep.R",
  "02_regression.R" = "vibe_work/R/02_regression.R",
  "03_timeseries.R" = "vibe_work/R/03_timeseries.R"
)
logs <- c(
  "01_data_prep.log"  = "vibe_work/output/logs/01_data_prep.log",
  "02_regression.log" = "vibe_work/output/logs/02_regression.log",
  "03_timeseries.log" = "vibe_work/output/logs/03_timeseries.log"
)

## ---------------------------------------------------------------------------
## Appendix A : project layout and how to reproduce
## ---------------------------------------------------------------------------
app_A <- sprintf('
<h2 id="s6">6. Appendix &mdash; R code and R output</h2>

<h3>A. Project layout and how to reproduce everything</h3>

<p>Every number, table and figure of this report is produced by three R scripts. Each script is
self-contained, writes its tables to <code>output/tables/*.csv</code> (plus a markdown twin
<code>*.md</code> for easy pasting), its figures to <code>output/figures/*.png</code>, and its console
output to <code>output/logs/*.log</code>. The package policy of the assignment is respected literally:
the only <code>library()</code> calls are <code>tidyverse</code>, <code>easystats</code> and
<code>patchwork</code>; everything else (linear models, <code>arima()</code>, <code>stl()</code>,
<code>Box.test()</code>, the ADF regression, the Breusch-Pagan regression, cross-validation) is base R.</p>

<pre><code>vibe_work/
├── R/
│   ├── 00_setup.R        libraries, paths, theme, helper functions (ADF, ACF/PACF, tables)
│   ├── 01_data_prep.R    loading, cleaning, codebook, descriptives, EDA      → T01–T03, F01–F04
│   ├── 02_regression.R   collinearity, subset selection, final model, forecast → T04–T10, F05–F10
│   └── 03_timeseries.R   STL, ADF, SARIMA search, diagnostics, forecast      → T11–T17, F11–F17
├── data/
│   ├── day.csv                        working copy of the dataset
│   └── dca_asos_2013-01-02.csv        24 hourly METAR reports of Washington National Airport
├── output/
│   ├── figures/  F01 … F17 (.png)
│   ├── tables/   T01 … T17 (.csv and .md)
│   └── logs/     01_data_prep.log, 02_regression.log, 03_timeseries.log
├── report/       report.html (this document), report_src.html, build_report.R
├── presentation/ slides.html
└── run_all.R     re-runs the three scripts and rebuilds this report</code></pre>

<p>To reproduce: <code>Rscript vibe_work/run_all.R</code> from the project root
(R 4.6.1, tidyverse 2.0.0, easystats 0.7.6, patchwork 1.3.2).</p>

<h3>B. Index of the tables and figures</h3>

<table>
<thead><tr><th>Label</th><th>Content</th><th>Produced in</th></tr></thead>
<tbody>
<tr><td>T01</td><td>codebook: every variable and the role it plays</td><td>01</td></tr>
<tr><td>T02 / T02b</td><td>descriptive statistics; categorical level counts</td><td>01</td></tr>
<tr><td>T03</td><td>Pearson correlations of the continuous predictors</td><td>01</td></tr>
<tr><td>T04 / T04b</td><td>VIFs before remediation; season-vs-month comparison</td><td>02</td></tr>
<tr><td>T05 / T05b</td><td>VIFs after remediation; VIFs of the final (interaction) model</td><td>02</td></tr>
<tr><td>T06</td><td>comparison of all candidate regression models (train, CV and hold-out)</td><td>02</td></tr>
<tr><td>T07 / T07b</td><td>coefficients of the final regression model; temperature slope by season</td><td>02</td></tr>
<tr><td>T08</td><td>overall fitness of the final regression model</td><td>02</td></tr>
<tr><td>T09 / T09c</td><td>diagnostic tests; OLS versus AR(1)-error standard errors</td><td>02</td></tr>
<tr><td>T10 / T10a</td><td>regression forecast for 2 Jan 2013; the observed DCA weather of that day</td><td>02</td></tr>
<tr><td>T11</td><td>weekly seasonal pattern estimated by STL</td><td>03</td></tr>
<tr><td>T12</td><td>augmented Dickey-Fuller tests</td><td>03</td></tr>
<tr><td>T13</td><td>SARIMA grid search (36 specifications)</td><td>03</td></tr>
<tr><td>T14</td><td>out-of-sample comparison of the time-series specifications</td><td>03</td></tr>
<tr><td>T15 / T16</td><td>SARIMA coefficients; residual diagnostics</td><td>03</td></tr>
<tr><td>T17 / T18</td><td>seven-day forecast; the two models side by side</td><td>03 / report</td></tr>
<tr><td>F01&ndash;F04</td><td>EDA: series, distributions, correlations, temp vs atemp</td><td>01</td></tr>
<tr><td>F05, F05b</td><td>VIF charts before / after remediation</td><td>02</td></tr>
<tr><td>F06&ndash;F10</td><td>check_model panel, coefficient plot, partial effects, residual ACF, hold-out fit</td><td>02</td></tr>
<tr><td>F11&ndash;F17</td><td>STL, weekly sub-series, ACF/PACF, residual ACF/histogram, forecast, fitted values</td><td>03</td></tr>
</tbody>
</table>

<h3>C. The R code</h3>
')

## ---------------------------------------------------------------------------
## Appendix B : the code listing
## ---------------------------------------------------------------------------
code_html <- character(0)
for (i in seq_along(scripts)) {
  nm <- names(scripts)[i]
  code_html <- c(code_html,
    sprintf('<h4 id="app-code-%d">C.%d &nbsp;<code>%s</code></h4>', i, i, nm),
    '<details open><summary>click to collapse / expand the source</summary>',
    sprintf('<pre><code>%s</code></pre>', esc(read_utf8(scripts[[i]]))),
    '</details>')
}

## ---------------------------------------------------------------------------
## Appendix C : the console output
## ---------------------------------------------------------------------------
log_html <- character(0)
for (i in seq_along(logs)) {
  nm <- names(logs)[i]
  script_nm <- sub("\\.log$", ".R", nm)
  log_html <- c(log_html,
    sprintf('<h4 id="app-log-%d">D.%d &nbsp;<code>%s</code></h4>', i, i, nm),
    sprintf('<p class="small">Complete console output of <code>%s</code> (also saved verbatim in <code>%s</code>).</p>',
            script_nm, logs[[i]]),
    '<details open><summary>click to collapse / expand the output</summary>',
    sprintf('<pre><code>%s</code></pre>', esc(read_utf8(logs[[i]]))),
    '</details>')
}

appendix <- paste0(app_A,
                   paste(code_html, collapse = "\n"),
                   '\n<h3>D. The R console output</h3>\n',
                   paste(log_html, collapse = "\n"),
                   '\n<p class="small">End of appendix.</p>\n')

## ---------------------------------------------------------------------------
## assemble
## ---------------------------------------------------------------------------
src <- read_utf8("vibe_work/report/report_src.html")
if (!grepl("<!--APPENDIX-->", src, fixed = TRUE)) stop("marker <!--APPENDIX--> not found")
out <- sub("<!--APPENDIX-->", appendix, src, fixed = TRUE)
writeLines(strsplit(out, "\n", fixed = TRUE)[[1]],
           "vibe_work/report/report.html", useBytes = TRUE)
cat("report.html written:",
    round(file.info("vibe_work/report/report.html")$size / 1024), "kB\n")
