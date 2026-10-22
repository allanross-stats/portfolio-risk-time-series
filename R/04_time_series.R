# 04_time_series.R
# Stationarity, autocorrelation and ARIMA for each portfolio's daily log returns.
# Adds what the original paper was missing: an out-of-sample test of whether
# ARIMA forecasts beat a simple "average return" forecast, and formal tests for
# volatility clustering (the motivation for GARCH in 05_garch.R).

source("R/00_setup.R")
d <- readRDS("data/returns.rds")
r <- d$port_log

# Engle's ARCH LM test: regress squared residuals on their own lags.
# Under H0 (no ARCH effects) n * R^2 ~ chi-squared(lags).
arch_lm <- function(x, lags = 5) {
  e2 <- (x - mean(x))^2
  X <- embed(e2, lags + 1)
  fit <- lm(X[, 1] ~ X[, -1])
  stat <- nrow(X) * summary(fit)$r.squared
  c(statistic = stat, p.value = pchisq(stat, lags, lower.tail = FALSE))
}

results <- list()
for (p in PORTFOLIOS) {
  x <- as.numeric(r[, p])
  v <- log(as.numeric(d$port_value[, p]))

  adf_level  <- suppressWarnings(adf.test(v))
  adf_return <- suppressWarnings(adf.test(x))
  lb_ret <- Box.test(x, lag = 10, type = "Ljung-Box")
  lb_sq  <- Box.test((x - mean(x))^2, lag = 10, type = "Ljung-Box")
  arch   <- arch_lm(x)

  # ---- ARIMA: fit on training data, forecast the test period one day ahead
  train <- r[index(r) < TEST_START, p]
  test  <- r[index(r) >= TEST_START, p]
  fit <- auto.arima(as.numeric(train), seasonal = FALSE, stepwise = TRUE)
  # Re-apply the fitted model to the full series without re-estimating, so each
  # test-day forecast uses only information available the day before.
  refit <- Arima(as.numeric(r[, p]), model = fit)
  arima_fc <- fitted(refit)[index(r) >= TEST_START]
  mean_fc  <- rep(mean(as.numeric(train)), length(test))
  rmse <- function(f) sqrt(mean((as.numeric(test) - f)^2))

  results[[p]] <- data.frame(
    Portfolio          = p,
    ADF_p_log_value    = adf_level$p.value,
    ADF_p_returns      = adf_return$p.value,
    LjungBox_p_returns = lb_ret$p.value,
    LjungBox_p_squared = lb_sq$p.value,
    ARCH_LM_p          = arch[["p.value"]],
    ARIMA_order        = paste0("(", paste(arimaorder(fit), collapse = ","), ")"),
    RMSE_ARIMA_test    = rmse(arima_fc),
    RMSE_mean_test     = rmse(mean_fc),
    ARIMA_vs_mean      = rmse(arima_fc) / rmse(mean_fc) - 1
  )
}
ts_tab <- bind_rows(results)
print(ts_tab, digits = 3)
save_table(ts_tab %>% mutate(across(where(is.numeric), ~ signif(.x, 4))),
           "time_series_tests")

# ---- Figure: ACF of returns vs. squared returns ----------------------------
acf_df <- bind_rows(lapply(PORTFOLIOS, function(p) {
  x <- as.numeric(r[, p])
  a1 <- acf(x, lag.max = 20, plot = FALSE)$acf[-1]
  a2 <- acf((x - mean(x))^2, lag.max = 20, plot = FALSE)$acf[-1]
  rbind(data.frame(Portfolio = p, Lag = 1:20, ACF = a1, Series = "Returns"),
        data.frame(Portfolio = p, Lag = 1:20, ACF = a2, Series = "Squared returns"))
}))
acf_df$Portfolio <- factor(acf_df$Portfolio, levels = PORTFOLIOS)
ci <- 1.96 / sqrt(nrow(r))
p <- ggplot(acf_df, aes(Lag, ACF)) +
  geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "grey50") +
  geom_col(aes(fill = Series), width = 0.6, position = "dodge") +
  facet_wrap(~Portfolio, nrow = 1) +
  scale_fill_manual(values = c(Returns = "grey55", `Squared returns` = "#E45756"),
                    name = NULL) +
  labs(title = "Returns are nearly unpredictable; their size is not",
       subtitle = "Autocorrelation of daily log returns vs. squared returns (dashed = 95% band)",
       x = "Lag (days)", y = "Autocorrelation")
save_plot(p, "05_acf_returns_vs_squared", w = 10, h = 4)
