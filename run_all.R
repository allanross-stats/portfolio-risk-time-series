# run_all.R
# Reproduces every table and figure. Run from the project root:
#   Rscript run_all.R            (uses data/prices.csv if it exists)
#   Rscript run_all.R --refresh  (re-downloads prices first)

args <- commandArgs(trailingOnly = TRUE)
if ("--refresh" %in% args || !file.exists("data/prices.csv")) {
  source("R/01_get_data.R")
}

scripts <- c("R/02_returns.R", "R/03_risk_metrics.R", "R/04_time_series.R",
             "R/05_garch.R", "R/06_var_es.R", "R/07_backtest.R",
             "R/08_efficient_frontier.R", "R/09_robustness.R")
for (s in scripts) {
  message("\n==== ", s, " ====")
  source(s, local = new.env())
}
message("\nDone. Tables in output/tables, figures in output/figures.")
