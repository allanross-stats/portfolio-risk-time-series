# 03_risk_metrics.R
# Return and risk summary for each portfolio, with every formula written out
# (no black-box package calls) so each number can be checked by hand.

source("R/00_setup.R")
d <- readRDS("data/returns.rds")
r  <- d$port_simple
rf <- as.numeric(d$rf_daily)
mkt <- d$asset_simple[, BENCHMARK]

ann_return <- function(x) prod(1 + x)^(TRADING_DAYS / length(x)) - 1   # CAGR
ann_vol    <- function(x) sd(x) * sqrt(TRADING_DAYS)

# Sharpe: mean daily excess return / sd of daily excess return, annualized
sharpe <- function(x, rf) {
  ex <- x - rf
  mean(ex) / sd(ex) * sqrt(TRADING_DAYS)
}

# Sortino: mean excess return / downside deviation (only returns below the
# risk-free rate count as risk), annualized
sortino <- function(x, rf) {
  ex <- x - rf
  dd <- sqrt(mean(pmin(ex, 0)^2))
  mean(ex) / dd * sqrt(TRADING_DAYS)
}

# Beta: Cov(portfolio, market) / Var(market)
beta_vs <- function(x, m) cov(x, m) / var(m)

# Treynor: annualized excess return per unit of beta
treynor <- function(x, rf, m) mean(x - rf) * TRADING_DAYS / beta_vs(x, m)

# Maximum drawdown: worst peak-to-trough fall in portfolio value
max_drawdown <- function(x) {
  v <- cumprod(1 + x)
  min(v / cummax(v) - 1)
}

metrics <- bind_rows(lapply(PORTFOLIOS, function(p) {
  x <- as.numeric(r[, p])
  m <- as.numeric(mkt)
  data.frame(
    Portfolio        = p,
    Ann_Return       = ann_return(x),
    Ann_Volatility   = ann_vol(x),
    Sharpe           = sharpe(x, rf),
    Sortino          = sortino(x, rf),
    Beta             = beta_vs(x, m),
    Treynor          = treynor(x, rf, m),
    Max_Drawdown     = max_drawdown(x),
    Skewness         = mean((x - mean(x))^3) / sd(x)^3,
    Excess_Kurtosis  = mean((x - mean(x))^4) / sd(x)^4 - 3
  )
}))

print(metrics %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
save_table(metrics %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           "portfolio_risk_metrics")

# Asset-level volatility, beta and R-squared vs. the market (replaces the
# hand-entered numbers in the paper's tables)
asset_tab <- bind_rows(lapply(names(ASSETS), function(a) {
  x <- as.numeric(d$asset_simple[, a]); m <- as.numeric(mkt)
  data.frame(Asset = a,
             Ann_Return = ann_return(x),
             Ann_Volatility = ann_vol(x),
             Beta = beta_vs(x, m),
             R_Squared = cor(x, m)^2)
}))
save_table(asset_tab %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           "asset_risk_metrics")

# ---- Figure: risk vs return ------------------------------------------------
p <- ggplot(metrics, aes(Ann_Volatility, Ann_Return, color = Portfolio)) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%s  %.1f%% / %.1f%%", Portfolio,
                                100 * Ann_Return, 100 * Ann_Volatility),
                vjust = ifelse(Portfolio == "Low", 1.8, -1)),
            hjust = 0.3, size = 3.2, show.legend = FALSE) +
  scale_color_manual(values = PORT_COLORS, guide = "none") +
  scale_x_continuous(labels = percent, limits = c(0, max(metrics$Ann_Volatility) * 1.25)) +
  scale_y_continuous(labels = percent, limits = c(0, max(metrics$Ann_Return) * 1.15)) +
  labs(title = "Annualized return vs. volatility",
       subtitle = "Labels show return / volatility", x = "Annualized volatility",
       y = "Annualized return (CAGR)")
save_plot(p, "03_risk_vs_return", w = 7, h = 5)

# ---- Figure: drawdowns -----------------------------------------------------
dd <- do.call(merge, lapply(PORTFOLIOS, function(p) {
  v <- cumprod(1 + r[, p]); v / cummax(v) - 1
}))
colnames(dd) <- PORTFOLIOS
p <- ggplot(xts_to_long(dd, "Drawdown"), aes(Date, Drawdown, color = Series)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = PORT_COLORS, name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(title = "Drawdown from previous peak", x = NULL, y = NULL)
save_plot(p, "04_drawdowns")
