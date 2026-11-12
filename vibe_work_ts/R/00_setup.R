###############################################################################
## 00_setup.R -- shared libraries, paths, theme and helper functions
## AIN4741 -- Part B, second study: HOURLY bike-sharing demand (hour.csv)
##
## Package policy for this study (as required):
##   library(tidyverse) | library(easystats) | library(patchwork)
##   library(tsibble)   | library(fable)     | library(feasts) | library(ggfortify)
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(easystats)
  library(patchwork)
  library(tsibble)
  library(fable)
  library(feasts)
  library(ggfortify)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## English month / weekday abbreviations in every plot
invisible(try(Sys.setlocale("LC_TIME", "C"), silent = TRUE))

## ---- paths -----------------------------------------------------------------
path_raw  <- "bike+sharing+dataset/hour.csv"
path_data <- "vibe_work_ts/data/hour.csv"
path_fig  <- "vibe_work_ts/output/figures"
path_tab  <- "vibe_work_ts/output/tables"
path_log  <- "vibe_work_ts/output/logs"
for (d in c(path_fig, path_tab, path_log)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## ---- house style -----------------------------------------------------------
theme_ev <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(colour = "grey90", linewidth = 0.3),
      plot.title          = element_text(face = "bold", size = base_size + 2),
      plot.subtitle       = element_text(colour = "grey35", size = base_size - 1),
      plot.caption        = element_text(colour = "grey45", size = base_size - 3, hjust = 0),
      strip.text          = element_text(face = "bold"),
      legend.position     = "bottom",
      plot.title.position = "plot"
    )
}
theme_set(theme_ev())

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

save_md <- function(x, name, digits = 3, caption = NULL) {
  d <- as.data.frame(x)
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], function(v) formatC(v, format = "f", digits = digits))
  d[] <- lapply(d, as.character)
  hdr <- paste0("| ", paste(names(d), collapse = " | "), " |")
  sep <- paste0("|", paste(rep(" --- |", ncol(d)), collapse = ""))
  rows <- apply(d, 1, function(z) paste0("| ", paste(z, collapse = " | "), " |"))
  out <- c(if (!is.null(caption)) paste0("**", caption, "**"), "", hdr, sep, rows)
  f <- file.path(path_tab, paste0(name, ".md"))
  writeLines(out, f, useBytes = TRUE)
  invisible(f)
}

## ---- helpers for the hourly study -----------------------------------------
## log-scale forecast -> original scale (median of a lognormal)
exp_median <- function(x) exp(x)

## 95% interval of a fable forecast, back-transformed with exp()
ci_of <- function(fc, level = 95) {
  fc %>%
    hilo(level = level) %>%
    unpack_hilo(paste0(level, "%")) %>%
    rename(lo = 2, hi = 3) %>%
    mutate(across(c(.mean, lo, hi), ~ exp(.x)))
}

## strength of each seasonal component of an STL decomposition
## (Wang, Smith & Hyndman 2006): max(0, 1 - Var(R) / Var(S + R))
season_strength <- function(remainder, seasonal) {
  max(0, 1 - var(remainder, na.rm = TRUE) / var(seasonal + remainder, na.rm = TRUE))
}

cat("## 00_setup.R loaded\n")
cat("   R          :", R.version.string, "\n")
cat("   tidyverse  :", as.character(packageVersion("tidyverse")), "\n")
cat("   easystats  :", as.character(packageVersion("easystats")), "\n")
cat("   patchwork  :", as.character(packageVersion("patchwork")), "\n")
cat("   tsibble    :", as.character(packageVersion("tsibble")), "\n")
cat("   fable      :", as.character(packageVersion("fable")), "\n")
cat("   feasts     :", as.character(packageVersion("feasts")), "\n")
cat("   ggfortify  :", as.character(packageVersion("ggfortify")), "\n")
