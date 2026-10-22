# 02_returns.R
# Clean prices, align calendars, and build asset and portfolio returns.
#
# Calendar choice: stocks trade ~252 days a year, crypto trades 365. Mixing the
# two calendars creates days where crypto moves and stocks show a "0" return,
# which understates stock volatility and distorts correlations. We keep only
# U.S. trading days and use crypto's latest close on each of those days, so a
# weekend crypto move is counted in Monday's return.
#
# Portfolio construction: fixed weights, rebalanced daily. Daily simple returns
# are weighted, then converted to log returns for the time-series models.

source("R/00_setup.R")

raw <- read.csv(PRICE_FILE)
prices <- xts(raw[, -1], order.by = as.Date(raw$Date))
colnames(prices) <- names(ASSETS)

# Carry the last available price forward (crypto across weekends/holidays),
# then keep only days the U.S. stock market was open.
stock_days <- index(prices)[!is.na(prices[, BENCHMARK])]
prices <- na.locf(prices)
prices <- prices[stock_days]
prices <- prices[paste0(START_DATE, "/", END_DATE)]
stopifnot(!anyNA(prices))

message("Analysis window: ", min(index(prices)), " to ", max(index(prices)),
        " (", nrow(prices), " trading days)")

# ---- Asset returns ---------------------------------------------------------
asset_simple <- na.omit(prices / lag.xts(prices) - 1)
asset_log    <- na.omit(diff(log(prices)))

# ---- Portfolio returns -----------------------------------------------------
port_simple <- do.call(merge, lapply(PORTFOLIOS, function(p) {
  w <- WEIGHTS[[p]]
  xts(as.numeric(asset_simple[, names(w)] %*% w), order.by = index(asset_simple))
}))
colnames(port_simple) <- PORTFOLIOS
port_log <- log1p(port_simple)

# Growth of $1
port_value <- exp(cumsum(port_log))

# ---- Risk-free rate (daily, simple) ---------------------------------------
if (file.exists(RF_FILE)) {
  rf_raw <- read.csv(RF_FILE)
  rf <- xts(rf_raw$DGS3MO / 100, order.by = as.Date(rf_raw$Date))
  rf <- na.locf(merge(rf, xts(, index(port_simple)), fill = NA))
  rf <- na.locf(rf, fromLast = TRUE)[index(port_simple)]
} else {
  rf <- xts(rep(RF_FALLBACK_ANNUAL, nrow(port_simple)), index(port_simple))
}
rf_daily <- (1 + rf)^(1 / TRADING_DAYS) - 1
colnames(rf_daily) <- "rf"

saveRDS(list(prices = prices, asset_simple = asset_simple, asset_log = asset_log,
             port_simple = port_simple, port_log = port_log,
             port_value = port_value, rf_daily = rf_daily),
        "data/returns.rds")

# ---- Figure: growth of $1 --------------------------------------------------
p <- ggplot(xts_to_long(port_value, "Value"),
            aes(Date, Value, color = Series)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = PORT_COLORS, name = NULL) +
  scale_y_log10(labels = dollar, breaks = c(0.5, 1, 2, 4, 8, 16)) +
  labs(title = "Growth of $1 invested (log scale)",
       subtitle = paste("Daily-rebalanced portfolios,",
                        format(min(index(port_value)), "%b %Y"), "to",
                        format(max(index(port_value)), "%b %Y")),
       x = NULL, y = NULL)
save_plot(p, "01_growth_of_1")

# ---- Figure: correlation matrix -------------------------------------------
cm <- cor(asset_log)
cm_long <- as.data.frame(as.table(cm))
names(cm_long) <- c("A", "B", "r")
cm_long$A <- factor(cm_long$A, levels = colnames(cm))
cm_long$B <- factor(cm_long$B, levels = rev(colnames(cm)))
p <- ggplot(cm_long, aes(A, B, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.8) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       limits = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation of daily log returns", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
save_plot(p, "02_correlation_matrix", w = 7.5, h = 6)
save_table(data.frame(Asset = rownames(cm), round(cm, 3), check.names = FALSE),
           "asset_correlations")
