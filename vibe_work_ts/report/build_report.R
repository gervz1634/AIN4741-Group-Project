###############################################################################
## build_report.R -- assembles vibe_work_ts/report/report.html by injecting the
## R sources and the console logs into report_src.html.  Base R only.
## Run from the project root: source("vibe_work_ts/report/build_report.R")
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
  "00_setup.R"        = "vibe_work_ts/R/00_setup.R",
  "01_data_prep.R"    = "vibe_work_ts/R/01_data_prep.R",
  "02_decomposition.R" = "vibe_work_ts/R/02_decomposition.R",
  "03_models.R"       = "vibe_work_ts/R/03_models.R",
  "04_forecast.R"     = "vibe_work_ts/R/04_forecast.R"
)
logs <- c(
  "01_data_prep.log"    = "vibe_work_ts/output/logs/01_data_prep.log",
  "02_decomposition.log" = "vibe_work_ts/output/logs/02_decomposition.log",
  "03_models.log"       = "vibe_work_ts/output/logs/03_models.log",
  "04_forecast.log"     = "vibe_work_ts/output/logs/04_forecast.log"
)

app_A <- '
<h2 id="s8">8. Appendix &mdash; R code and R output</h2>

<h3>A. Project layout and how to reproduce everything</h3>

<p>Every number, table and figure of this report is produced by four R scripts plus one setup file. Each
script writes its tables to <code>output/tables/*.csv</code> (with a markdown twin <code>*.md</code>), its
figures to <code>output/figures/*.png</code> and its console output to <code>output/logs/*.log</code>.
The package policy of the assignment is respected literally &mdash; the only <code>library()</code> calls are
<code>tidyverse</code>, <code>easystats</code>, <code>patchwork</code>, <code>tsibble</code>,
<code>fable</code>, <code>feasts</code> and <code>ggfortify</code> &mdash; and everything else
(<code>Box.test()</code>, <code>acf()</code>, <code>shapiro.test()</code>, <code>ts()</code>,
<code>decompose()</code>, the rolling-origin loop) is base R.</p>

<pre><code>vibe_work_ts/
├── R/
│   ├── 00_setup.R         libraries, paths, ggplot theme, helper functions
│   ├── 01_data_prep.R     hour.csv -> regular tsibble, completeness, EDA   -> T00-T04, F01-F04
│   ├── 02_decomposition.R multi-seasonal STL, seasonal indices, classical
│   │                      decomposition with ggfortify                     -> T05-T06, F05-F09
│   ├── 03_models.R        transformation, stationarity, 11 candidate models,
│   │                      rolling-origin evaluation, model selection        -> T07-T09, F10-F13
│   └── 04_forecast.R      final model, diagnostics, forecast, sensitivity  -> T10-T14, F14-F17
├── data/   hour.csv, hourly_ts.rds, decomposition.rds, model_selection.rds, forecast.rds
├── output/ figures/ (F01..F17)  tables/ (T00..T14)  logs/ (4 .log files)
├── report/ report.html (this document), report_src.html, build_report.R
├── presentation/ slides.html
└── run_all.R</code></pre>

<p>To reproduce: <code>Rscript vibe_work_ts/run_all.R</code> from the project root
(R 4.6.1, tsibble 1.2.0, fable 0.5.0, feasts 0.5.0, ggfortify 0.4.24). The whole pipeline takes about
eight minutes, most of it the automatic ARIMA on 17,208 hourly observations and the 57-origin
rolling evaluation.</p>

<h3>B. Index of tables and figures</h3>
<table>
<thead><tr><th>Label</th><th>Content</th><th>Script</th></tr></thead>
<tbody>
<tr><td>T00</td><td>codebook of hour.csv</td><td>01</td></tr>
<tr><td>T01</td><td>completeness of the hourly series</td><td>01</td></tr>
<tr><td>T01b</td><td>the four season levels: dates, temperature, demand</td><td>report text</td></tr>
<tr><td>T02</td><td>descriptive statistics</td><td>01</td></tr>
<tr><td>T03 / T04</td><td>hourly profile by day type; the 7x24 matrix</td><td>01</td></tr>
<tr><td>T05a / T05b</td><td>STL variance shares; strength of seasonality</td><td>02</td></tr>
<tr><td>T06 / T06b</td><td>daily seasonal index; weekly seasonal index</td><td>02</td></tr>
<tr><td>T07</td><td>variance-versus-level table</td><td>03</td></tr>
<tr><td>T08</td><td>KPSS, ndiffs, nsdiffs</td><td>03</td></tr>
<tr><td>T09 / T09b</td><td>11 candidate models; rolling-origin evaluation</td><td>03</td></tr>
<tr><td>T10 / T11</td><td>hour-of-week profile; annual Fourier coefficients</td><td>04</td></tr>
<tr><td>T12 / T12a</td><td>residual diagnostics; overall fit</td><td>04</td></tr>
<tr><td>T13 / T13b</td><td>the hourly forecast for 2 Jan 2013; plausibility references</td><td>04</td></tr>
<tr><td>T14</td><td>sensitivity of the forecast</td><td>04</td></tr>
<tr><td>F01&ndash;F04</td><td>series overview, hourly profile, 7x24 heatmap, seasonal scales</td><td>01</td></tr>
<tr><td>F05&ndash;F09</td><td>STL components, seasonal indices, gg_season, gg_subseries, classical decomposition</td><td>02</td></tr>
<tr><td>F10&ndash;F13</td><td>ACF (feasts), ACF (ggfortify), hold-out forecasts, in-sample fit</td><td>03</td></tr>
<tr><td>F14&ndash;F17</td><td>gg_tsresiduals, residual structure, weekly forecast, daily forecast</td><td>04</td></tr>
</tbody>
</table>

<h3>C. The R code</h3>
'

code_html <- character(0)
for (i in seq_along(scripts)) {
  nm <- names(scripts)[i]
  code_html <- c(code_html,
    sprintf('<h4 id="app-code-%d">C.%d &nbsp;<code>%s</code></h4>', i, i, nm),
    '<details open><summary>click to collapse / expand the source</summary>',
    sprintf('<pre><code>%s</code></pre>', esc(read_utf8(scripts[[i]]))),
    '</details>')
}

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

src <- read_utf8("vibe_work_ts/report/report_src.html")
if (!grepl("<!--APPENDIX-->", src, fixed = TRUE)) stop("marker <!--APPENDIX--> not found")
out <- sub("<!--APPENDIX-->", appendix, src, fixed = TRUE)
writeLines(strsplit(out, "\n", fixed = TRUE)[[1]],
           "vibe_work_ts/report/report.html", useBytes = TRUE)
cat("report.html written:",
    round(file.info("vibe_work_ts/report/report.html")$size / 1024), "kB\n")
