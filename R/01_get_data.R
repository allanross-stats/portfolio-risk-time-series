# 01_get_data.R
# Download adjusted daily prices from Yahoo Finance and the 3-month Treasury
# yield from FRED. Saves data/prices.csv and data/risk_free.csv so the rest of
# the analysis is reproducible offline.

source("R/00_setup.R")

get_adjusted <- function(ticker) {
  x <- getSymbols(ticker, src = "yahoo", from = START_DATE - 10, to = END_DATE + 1,
                  auto.assign = FALSE, warnings = FALSE)
  Ad(x)
}

message("Downloading prices for ", length(ASSETS), " assets...")
price_list <- lapply(ASSETS, get_adjusted)
prices <- do.call(merge, price_list)          # outer join on date
colnames(prices) <- names(ASSETS)

write.csv(data.frame(Date = index(prices), coredata(prices)),
          PRICE_FILE, row.names = FALSE)
message("Saved ", PRICE_FILE, " (", nrow(prices), " rows)")

# 3-month Treasury bill yield (percent, annualized), FRED series DGS3MO
rf <- tryCatch({
  r <- getSymbols("DGS3MO", src = "FRED", auto.assign = FALSE)
  r <- r[paste0(START_DATE - 10, "/", END_DATE)]
  r
}, error = function(e) {
  message("FRED download failed; using fallback rate of ",
          RF_FALLBACK_ANNUAL * 100, "%")
  NULL
})

if (!is.null(rf)) {
  write.csv(data.frame(Date = index(rf), DGS3MO = as.numeric(rf)),
            RF_FILE, row.names = FALSE)
  message("Saved ", RF_FILE)
}
