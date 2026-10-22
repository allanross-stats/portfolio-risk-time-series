# 07_backtest.R
# Out-of-sample VaR backtest. For every trading day from TEST_START onward we
# estimate tomorrow's 1-day VaR using ONLY the previous WINDOW days, then check
# whether the actual loss exceeded it (an "exception").
#
# Methods: Historical, Parametric (normal), GARCH(1,1)-t.
# A well-calibrated 95% VaR should be exceeded on about 5% of days.
#   Kupiec test         - is the exception rate consistent with 5% (or 1%)?
#   Christoffersen test - are exceptions independent, or do they cluster?

source("R/00_setup.R")
d <- readRDS("data/returns.rds")
r <- d$port_log

WINDOW <- 500          # about two years of trading days
REFIT_EVERY <- 21      # re-estimate GARCH parameters monthly; filter daily
LEVELS <- c(0.95, 0.99)

# GARCH variance recursion with fixed parameters
garch_filter <- function(x, mu, omega, alpha, beta, s2_start) {
  s2 <- numeric(length(x) + 1); s2[1] <- s2_start
  for (t in seq_along(x)) s2[t + 1] <- omega + alpha * (x[t] - mu)^2 + beta * s2[t]
  s2   # s2[t] = variance forecast for x[t]; last element = next-day forecast
}

kupiec <- function(hits, p) {
  n <- length(hits); x <- sum(hits); ph <- x / n
  if (x == 0) ll1 <- n * log(1 - p) else
    ll1 <- (n - x) * log(1 - ph) + x * log(ph)
  ll0 <- (n - x) * log(1 - p) + x * log(p)
  lr <- -2 * (ll0 - ll1)
  pchisq(lr, 1, lower.tail = FALSE)
}

christoffersen <- function(hits) {
  h0 <- head(hits, -1); h1 <- tail(hits, -1)
  n00 <- sum(!h0 & !h1); n01 <- sum(!h0 & h1); n10 <- sum(h0 & !h1); n11 <- sum(h0 & h1)
  if (n01 + n11 == 0) return(NA)
  p01 <- n01 / (n00 + n01); p11 <- if (n10 + n11 > 0) n11 / (n10 + n11) else 0
  p <- (n01 + n11) / (n00 + n01 + n10 + n11)
  ll <- function(a, b, c) ifelse(b > 0 & c > 0, b * log(c), 0) + ifelse(a > 0 & c < 1, a * log(1 - c), 0)
  l0 <- ll(n00 + n10, n01 + n11, p)
  l1 <- ll(n00, n01, p01) + ll(n10, n11, p11)
  pchisq(-2 * (l0 - l1), 1, lower.tail = FALSE)
}

test_idx <- which(index(r) >= TEST_START)
bt_rows <- list(); bt_series <- list()

for (p in PORTFOLIOS) {
  x <- as.numeric(r[, p])
  var_mat <- array(NA, c(length(test_idx), 3, length(LEVELS)),
                   dimnames = list(NULL, c("Historical", "Parametric (normal)",
                                           "GARCH(1,1)-t"), LEVELS))
  g <- NULL
  for (k in seq_along(test_idx)) {
    t <- test_idx[k]
    win <- x[(t - WINDOW):(t - 1)]
    if (is.null(g) || (k - 1) %% REFIT_EVERY == 0) {
      # Standard-error warnings during refits don't affect the point estimates
      fit <- suppressWarnings(garchFit(~ garch(1, 1), data = 100 * win,
                                       cond.dist = "std", include.mean = TRUE,
                                       trace = FALSE))
      cf <- coef(fit)
      g <- list(mu = cf[["mu"]] / 100, omega = cf[["omega"]] / 1e4,
                alpha = cf[["alpha1"]], beta = cf[["beta1"]], nu = cf[["shape"]],
                fit_end = t - 1, s2_end = tail(volatility(fit), 1)^2 / 1e4)
    }
    # Filter forward from the last refit to get today's variance forecast
    since <- if (t - 1 > g$fit_end) x[(g$fit_end + 1):(t - 1)] else numeric(0)
    s2 <- tail(garch_filter(since, g$mu, g$omega, g$alpha, g$beta,
                            g$omega + g$alpha * (x[g$fit_end] - g$mu)^2 +
                              g$beta * g$s2_end), 1)
    if (length(since) == 0)
      s2 <- g$omega + g$alpha * (x[g$fit_end] - g$mu)^2 + g$beta * g$s2_end
    for (j in seq_along(LEVELS)) {
      a <- LEVELS[j]
      var_mat[k, 1, j] <- -quantile(win, 1 - a, names = FALSE)
      var_mat[k, 2, j] <- -(mean(win) + qnorm(1 - a) * sd(win))
      var_mat[k, 3, j] <- -(g$mu + qstd(1 - a, nu = g$nu) * sqrt(s2))
    }
  }
  actual <- x[test_idx]
  for (j in seq_along(LEVELS)) for (m in dimnames(var_mat)[[2]]) {
    hits <- actual < -var_mat[, m, j]
    bt_rows[[length(bt_rows) + 1]] <- data.frame(
      Portfolio = p, Confidence = LEVELS[j], Method = m,
      Days = length(hits), Exceptions = sum(hits),
      Expected = round(length(hits) * (1 - LEVELS[j]), 1),
      Exception_Rate = mean(hits),
      Kupiec_p = kupiec(hits, 1 - LEVELS[j]),
      Christoffersen_p = christoffersen(hits))
  }
  bt_series[[p]] <- data.frame(Date = index(r)[test_idx], Portfolio = p,
                               Return = actual,
                               VaR_Hist = var_mat[, 1, "0.95"],
                               VaR_GARCH = var_mat[, 3, "0.95"])
}

bt_tab <- bind_rows(bt_rows)
print(bt_tab %>% mutate(across(where(is.double), ~ round(.x, 3))))
save_table(bt_tab %>% mutate(across(c(Exception_Rate, Kupiec_p, Christoffersen_p),
                                    ~ round(.x, 4))), "var_backtest")

# ---- Figure: backtest for the High portfolio -------------------------------
bs <- bind_rows(bt_series) %>% filter(Portfolio == "High")
bs$Breach <- bs$Return < -bs$VaR_GARCH
p <- ggplot(bs, aes(Date)) +
  geom_col(aes(y = Return), fill = "grey70", width = 1) +
  geom_line(aes(y = -VaR_Hist, color = "Historical VaR (95%)"), linewidth = 0.5) +
  geom_line(aes(y = -VaR_GARCH, color = "GARCH-t VaR (95%)"), linewidth = 0.5) +
  geom_point(data = subset(bs, Breach), aes(y = Return), color = "#B2182B", size = 1.6) +
  scale_color_manual(values = c(`Historical VaR (95%)` = "#4C78A8",
                                `GARCH-t VaR (95%)` = "#E45756"), name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(title = "Out-of-sample VaR backtest: High portfolio",
       subtitle = "Grey = daily log return; red dots = losses beyond the GARCH-t VaR",
       x = NULL, y = NULL)
save_plot(p, "08_var_backtest_high")
