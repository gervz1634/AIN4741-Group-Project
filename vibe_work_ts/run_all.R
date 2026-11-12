###############################################################################
## run_all.R -- reproduce the whole hourly analysis and rebuild the report
##
##   Rscript vibe_work_ts/run_all.R
##
## Each analysis script runs in its own R process so that the console output of
## every step is captured cleanly in vibe_work_ts/output/logs/.
###############################################################################

rscript <- file.path(R.home("bin"), "Rscript")

steps <- c("01_data_prep", "02_decomposition", "03_models", "04_forecast")

for (s in steps) {
  script <- file.path("vibe_work_ts", "R", paste0(s, ".R"))
  log    <- file.path("vibe_work_ts", "output", "logs", paste0(s, ".log"))
  dir.create(dirname(log), showWarnings = FALSE, recursive = TRUE)
  cat("\n================  running", s, "================\n")
  t0 <- Sys.time()
  status <- system2(rscript, shQuote(script), stdout = log, stderr = log)
  cat("   exit status:", status, " | elapsed:",
      round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), "min\n")
  if (status != 0) stop("script failed: ", script, " -- see ", log)
}

cat("\n================  building the report  ================\n")
source(file.path("vibe_work_ts", "report", "build_report.R"))

cat("\nAll done.\n",
    "  report      : vibe_work_ts/report/report.html\n",
    "  presentation: vibe_work_ts/presentation/slides.html\n",
    "  tables      : vibe_work_ts/output/tables/\n",
    "  figures     : vibe_work_ts/output/figures/\n",
    "  logs        : vibe_work_ts/output/logs/\n", sep = "")
