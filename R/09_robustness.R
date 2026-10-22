# 09_robustness.R
# Are the conclusions an accident of the time window?
# Splits the sample into the 2019-2021 bull run (including the crypto boom)
# and 2022 onward (rate hikes, crypto crash, recovery) and recomputes the key
# metrics. If the High portfolio only looks good in one period, its full-sample
# Sharpe ratio is mostly hindsight.

source("R/00_setup.R")
d <- readRDS("data/returns.rds")

periods <- list(`Jun 2019 - Dec 2021` = c(START_DATE, as.Date("2021-12-31")),
                `Jan 2022 - Oct 2025` = c(as.Date("2022-01-01"), END_DATE),
                `Full sample`         = c(START_DATE, END_DATE))

rows <- list()
for (nm in names(periods)) {
  rng <- paste0(periods[[nm]][1], "/", periods[[nm]][2])
  r <- d$port_simple[rng]; rf <- as.numeric(d$rf_daily[rng])
  for (p in PORTFOLIOS) {
    x <- as.numeric(r[, p]); ex <- x - rf
    v <- cumprod(1 + x)
    rows[[length(rows) + 1]] <- data.frame(
      Period = nm, Portfolio = p,
      Ann_Return = prod(1 + x)^(TRADING_DAYS / length(x)) - 1,
      Ann_Volatility = sd(x) * sqrt(TRADING_DAYS),
      Sharpe = mean(ex) / sd(ex) * sqrt(TRADING_DAYS),
      Max_Drawdown = min(v / cummax(v) - 1))
  }
}
rob <- bind_rows(rows)
print(rob %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
save_table(rob %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           "subperiod_robustness")

# ---- Rebalancing assumption ------------------------------------------------
# The main analysis rebalances to target weights every day. Daily rebalancing
# sells winners and buys losers, which can add return when assets are volatile
# and mean-revert (and can cost return when they trend). Compare with
# buy-and-hold (set weights once, never rebalance) and monthly rebalancing.
px <- d$prices
growth <- function(p, rule) {
  w <- WEIGHTS[[p]]; P <- px[, names(w)]
  if (rule == "Buy and hold") {
    v <- as.numeric((P / matrix(as.numeric(P[1, ]), nrow(P), ncol(P), byrow = TRUE)) %*% w)
  } else {
    rets <- d$asset_simple[, names(w)]
    ep <- if (rule == "Monthly") endpoints(rets, "months") else seq_len(nrow(rets))
    v <- numeric(nrow(rets)); hold <- w; val <- 1
    for (t in seq_len(nrow(rets))) {
      hold <- hold * (1 + as.numeric(rets[t, ]))
      val <- sum(hold); v[t] <- val
      if (t %in% ep) hold <- val * w
    }
    v <- c(1, v)   # start at $1 on the first day, like buy-and-hold
  }
  v
}
reb <- bind_rows(lapply(PORTFOLIOS, function(p) {
  bind_rows(lapply(c("Daily", "Monthly", "Buy and hold"), function(rule) {
    v <- growth(p, rule)
    data.frame(Portfolio = p, Rebalancing = rule,
               Ann_Return = tail(v, 1)^(TRADING_DAYS / (length(v) - 1)) - 1,
               Ending_Value_of_1 = tail(v, 1))
  }))
}))
print(reb %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
save_table(reb %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           "rebalancing_sensitivity")

rob$Portfolio <- factor(rob$Portfolio, levels = PORTFOLIOS)
rob$Period <- factor(rob$Period, levels = names(periods))
p <- ggplot(rob, aes(Portfolio, Sharpe, fill = Portfolio)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, color = "grey30") +
  facet_wrap(~Period) +
  scale_fill_manual(values = PORT_COLORS, guide = "none") +
  labs(title = "Sharpe ratio by period",
       subtitle = "Every portfolio's risk-adjusted return fell sharply after 2021; the ranking held",
       x = NULL, y = "Annualized Sharpe ratio")
save_plot(p, "10_sharpe_by_period", w = 9, h = 4)
