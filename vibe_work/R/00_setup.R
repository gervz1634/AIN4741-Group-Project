###############################################################################
## 00_setup.R -- shared libraries, paths, plotting theme and helper functions
## AIN4741 Group Project : Capital Bikeshare (Washington D.C., day-level data)
##
## Package policy for this project (as required by the assignment):
##   library(tidyverse) | library(easystats) | library(patchwork)
## Everything else used is BASE R (stats::lm, stats::arima, stats::stl, ...).
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(easystats)
  library(patchwork)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## English month / weekday abbreviations in every plot and table
invisible(try(Sys.setlocale("LC_TIME", "C"), silent = TRUE))

## ---- paths -----------------------------------------------------------------
path_raw   <- "bike+sharing+dataset/day.csv"   # original file, read-only source
path_data  <- "vibe_work/data/day.csv"         # working copy
path_fig   <- "vibe_work/output/figures"
path_tab   <- "vibe_work/output/tables"
path_log   <- "vibe_work/output/logs"

for (d in c(path_fig, path_tab, path_log)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

## ---- house style -----------------------------------------------------------
theme_ev <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "grey90", linewidth = 0.3),
      plot.title         = element_text(face = "bold", size = base_size + 2),
      plot.subtitle      = element_text(colour = "grey35", size = base_size - 1),
      plot.caption       = element_text(colour = "grey45", size = base_size - 3,
                                        hjust = 0),
      strip.text         = element_text(face = "bold"),
      legend.position    = "bottom",
      plot.title.position = "plot"
    )
}
theme_set(theme_ev())

## project palette
pal_main <- c("#1F5673", "#2E8B9E", "#7FB7BE", "#E4A11B", "#C84630", "#8C6D9B")

## ---- file helpers ----------------------------------------------------------
save_fig <- function(plot, name, width = 9, height = 5.4, dpi = 150) {
  f <- file.path(path_fig, paste0(name, ".png"))
  ggsave(f, plot, width = width, height = height, dpi = dpi, bg = "white")
  invisible(f)
}

save_tab <- function(x, name) {
  f <- file.path(path_tab, paste0(name, ".csv"))
  readr::write_csv(as.data.frame(x), f, na = "")
  invisible(f)
}

## write a markdown pipe-table (used to keep report tables in sync with code)
save_md <- function(x, name, digits = 3, caption = NULL) {
  d <- as.data.frame(x)
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], function(v) formatC(v, format = "f", digits = digits))
  d[] <- lapply(d, as.character)
  hdr <- paste0("| ", paste(names(d), collapse = " | "), " |")
  sep <- paste0("|", paste(rep(" --- |", ncol(d)), collapse = ""))
  rows <- apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  out <- c(if (!is.null(caption)) paste0("**", caption, "**"), "",
           hdr, sep, rows)
  f <- file.path(path_tab, paste0(name, ".md"))
  writeLines(out, f, useBytes = TRUE)
  invisible(f)
}

## ---- statistics helpers (base R only) --------------------------------------
## Manual augmented Dickey-Fuller test.
##   Delta y_t = a + b*t + gamma*y_{t-1} + sum_i delta_i * Delta y_{t-i} + e_t
## The t-ratio of gamma is compared with MacKinnon (1996) large-sample critical
## values, because the tseries/urca packages are outside the allowed package set.
adf_crit <- list(
  none  = c(`1%` = -2.58, `5%` = -1.95, `10%` = -1.62),
  drift = c(`1%` = -3.43, `5%` = -2.86, `10%` = -2.57),
  trend = c(`1%` = -3.96, `5%` = -3.41, `10%` = -3.12)
)

adf_test <- function(x, lags = 7, type = c("drift", "trend", "none")) {
  type <- match.arg(type)
  x <- as.numeric(x)
  n <- length(x)
  dx <- diff(x)                       # dx[i] = x[i+1] - x[i]
  idx <- (lags + 1):(n - 1)           # usable equations
  d <- tibble(y = dx[idx])
  d$y1 <- x[idx]                      # lagged level  (x_t-1)
  for (i in seq_len(lags)) d[[paste0("dy", i)]] <- dx[idx - i]
  if (type %in% c("drift", "trend")) d$int <- 1
  if (type == "trend")               d$trend <- idx
  fit <- lm(y ~ ., data = d)
  co  <- summary(fit)$coefficients
  tstat <- co["y1", "t value"]
  cv <- adf_crit[[type]]
  tibble(
    test    = "ADF",
    type    = type,
    lags    = lags,
    statistic = tstat,
    crit_1pct = cv[[1]], crit_5pct = cv[[2]], crit_10pct = cv[[3]],
    reject_5pct = tstat < cv[[2]],
    p_value_approx = approx(c(cv[[1]], cv[[2]], cv[[3]]), c(0.01, 0.05, 0.10),
                            xout = tstat, rule = 2)$y
  )
}

## tidy ACF / PACF for ggplot (self-computed, no forecast package)
acf_tbl <- function(x, lag.max = 21, type = c("acf", "pacf"), label = NULL) {
  type <- match.arg(type)
  x <- as.numeric(x)
  a <- if (type == "acf") stats::acf(x, lag.max = lag.max, plot = FALSE)
       else stats::pacf(x, lag.max = lag.max, plot = FALSE)
  tibble(lag = as.numeric(a$lag), value = as.numeric(a$acf),
         type = type, series = label %||% "")
}

plot_acf_pacf <- function(x, lag.max = 21, title = "", label = "") {
  n <- sum(!is.na(as.numeric(x)))
  ci <- qnorm(0.975) / sqrt(n)
  a <- acf_tbl(x, lag.max, "acf", label)
  p <- acf_tbl(x, lag.max, "pacf", label)
  bind_rows(a, p) %>%
    ggplot(aes(lag, value)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed",
               colour = pal_main[5], linewidth = 0.4) +
    geom_col(width = 0.32, fill = pal_main[1]) +
    facet_wrap(~ type, ncol = 2, scales = "free_y") +
    labs(title = title, x = "Lag", y = NULL) +
    theme_ev(10)
}

## 95% interval helper (tidy output from predict.lm)
ci_table <- function(x) {
  tibble(
    quantity  = rownames(x),
    fit       = x[, "fit"],
    lower_95  = x[, "lwr"],
    upper_95  = x[, "upr"]
  )
}

cat("## 00_setup.R loaded\n")
cat("   R version      :", R.version.string, "\n")
cat("   tidyverse      :", as.character(packageVersion("tidyverse")), "\n")
cat("   easystats      :", as.character(packageVersion("easystats")), "\n")
cat("   patchwork      :", as.character(packageVersion("patchwork")), "\n")
