# 00_setup.R
# Shared configuration: packages, assets, portfolio weights, dates, helpers.
# Every other script sources this file first.

suppressPackageStartupMessages({
  library(quantmod)   # data download (Yahoo Finance, FRED)
  library(xts)
  library(zoo)
  library(forecast)   # auto.arima
  library(tseries)    # adf.test
  library(fGarch)     # GARCH(1,1) with Student-t errors
  library(quadprog)   # efficient frontier (quadratic programming)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

set.seed(2025)

# ---- Study window ----------------------------------------------------------
START_DATE <- as.Date("2019-06-17")   # first date all assets trade (incl. LINK-USD history)
END_DATE   <- as.Date("2025-10-31")
TEST_START <- as.Date("2024-01-01")   # out-of-sample period for the VaR backtest

# ---- Assets ----------------------------------------------------------------
# Yahoo Finance tickers. Crypto trades 7 days a week; stocks 5. See 02_returns.R.
ASSETS <- c(
  VTI  = "VTI",       # Vanguard Total Stock Market ETF
  BND  = "BND",       # Vanguard Total Bond Market ETF
  VNQ  = "VNQ",       # Vanguard Real Estate ETF
  GLD  = "GLD",       # SPDR Gold Shares
  VWO  = "VWO",       # Vanguard Emerging Markets ETF
  QUAL = "QUAL",      # iShares MSCI USA Quality Factor ETF
  PSP  = "PSP",       # Invesco Global Listed Private Equity ETF
  ARKK = "ARKK",      # ARK Innovation ETF
  BTC  = "BTC-USD",   # Bitcoin
  ETH  = "ETH-USD",   # Ethereum
  LINK = "LINK-USD"   # Chainlink
)
CRYPTO <- c("BTC", "ETH", "LINK")
BENCHMARK <- "VTI"    # market proxy for beta and Treynor

# ---- Portfolio weights (from the paper) -----------------------------------
WEIGHTS <- list(
  Control = c(VTI = 0.40, BND = 0.60),
  Low     = c(VTI = 0.15, BND = 0.60, VNQ = 0.10, GLD = 0.15),
  Medium  = c(VTI = 0.30, BND = 0.15, VWO = 0.20, QUAL = 0.15, VNQ = 0.10,
              BTC = 0.05, ETH = 0.05),
  High    = c(VTI = 0.10, PSP = 0.05, VWO = 0.15, ARKK = 0.15, BTC = 0.15,
              LINK = 0.20, ETH = 0.20)
)
stopifnot(all(abs(sapply(WEIGHTS, sum) - 1) < 1e-9))
PORTFOLIOS <- names(WEIGHTS)

# ---- Risk-free rate --------------------------------------------------------
# Used only if the FRED download in 01_get_data.R fails.
RF_FALLBACK_ANNUAL <- 0.025

TRADING_DAYS <- 252

# ---- Paths -----------------------------------------------------------------
PRICE_FILE <- "data/prices.csv"
RF_FILE    <- "data/risk_free.csv"
FIG_DIR    <- "output/figures"
TAB_DIR    <- "output/tables"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Plot style ------------------------------------------------------------
PORT_COLORS <- c(Control = "#4C78A8", Low = "#54A24B",
                 Medium = "#F58518", High = "#E45756")
theme_set(theme_minimal(base_size = 11) +
            theme(plot.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank(),
                  legend.position = "bottom"))

save_plot <- function(p, name, w = 8, h = 4.5) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p, width = w, height = h,
         dpi = 150, bg = "white")
}

save_table <- function(df, name) {
  write.csv(df, file.path(TAB_DIR, paste0(name, ".csv")), row.names = FALSE)
}

xts_to_long <- function(x, value_name = "value") {
  df <- data.frame(Date = index(x), coredata(x), check.names = FALSE)
  out <- pivot_longer(df, -Date, names_to = "Series", values_to = value_name)
  out$Series <- factor(out$Series, levels = colnames(x))
  out
}
