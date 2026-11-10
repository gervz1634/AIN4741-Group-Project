###############################################################################
## run_all.R -- reproduce the whole analysis and rebuild the report
##
##   Rscript vibe_work/run_all.R
##
## Each analysis script is executed in its own R process, so that the objects of
## one script can never leak into another and the console output of each step is
## captured cleanly in vibe_work/output/logs/.  The report is then rebuilt from
## those logs and from the R sources.
###############################################################################

rscript <- file.path(R.home("bin"), "Rscript")
root    <- getwd()

steps <- c("01_data_prep", "02_regression", "03_timeseries")

for (s in steps) {
  script <- file.path("vibe_work", "R", paste0(s, ".R"))
  log    <- file.path("vibe_work", "output", "logs", paste0(s, ".log"))
  dir.create(dirname(log), showWarnings = FALSE, recursive = TRUE)
  cat("\n================  running", s, "================\n")
  t0 <- Sys.time()
  status <- system2(rscript, shQuote(script), stdout = log, stderr = log)
  cat("   exit status:", status,
      " | elapsed:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), "min\n")
  if (status != 0) stop("script failed: ", script, " -- see ", log)
}

cat("\n================  building the report  ================\n")
source(file.path("vibe_work", "report", "build_report.R"))

cat("\nAll done.\n",
    "  report      : vibe_work/report/report.html\n",
    "  presentation: vibe_work/presentation/slides.html\n",
    "  tables      : vibe_work/output/tables/\n",
    "  figures     : vibe_work/output/figures/\n",
    "  logs        : vibe_work/output/logs/\n", sep = "")
