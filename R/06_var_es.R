# 06_var_es.R
# Value at Risk (VaR) and Expected Shortfall (ES) for each portfolio, three ways:
#   1. Historical  - empirical quantile of actual returns (no distribution assumed)
#   2. Parametric  - assumes normally distributed returns
#   3. Monte Carlo - simulates correlated asset returns (multivariate Student-t,
#                    which allows fat tails) and compounds them over the horizon
# VaR(95%) = the loss exceeded on only 5% of days.
# ES(95%)  = the average loss on those worst 5% of days.
# Reported as a loss in dollars on a $1,000,000 portfolio.

source("R/00_setup.R")
d <- readRDS("data/returns.rds")

PORT_VALUE <- 1e6
LEVELS <- c(0.95, 0.99)
HORIZONS <- c(1, 10)
N_SIM <- 20000

hist_var_es <- function(ret, a) {
  q <- quantile(ret, 1 - a, names = FALSE)
  c(VaR = -q, ES = -mean(ret[ret <= q]))
}

param_var_es <- function(ret, a, h) {
  mu <- mean(ret) * h; s <- sd(ret) * sqrt(h); z <- qnorm(1 - a)
  c(VaR = -(mu + z * s), ES = -(mu - s * dnorm(z) / (1 - a)))
}

# Multivariate Student-t simulation of daily asset log returns
simulate_paths <- function(asset_log, h, n, nu = 5) {
  mu <- colMeans(asset_log)
  S  <- cov(asset_log) * (nu - 2) / nu      # scale so simulated cov matches sample
  L  <- chol(S)
  k  <- ncol(asset_log)
  total <- matrix(0, n, k)
  for (step in seq_len(h)) {
    z <- matrix(rnorm(n * k), n, k) %*% L
    w <- sqrt(nu / rchisq(n, nu))
    total <- total + sweep(z * w, 2, mu, "+")
  }
  colnames(total) <- colnames(asset_log)
  exp(total) - 1                              # h-day simple returns per asset
}

rows <- list(); sims_10d <- list()
asset_log <- coredata(d$asset_log)
sim_cache <- lapply(HORIZONS, function(h) simulate_paths(asset_log, h, N_SIM))
names(sim_cache) <- HORIZONS

for (p in PORTFOLIOS) {
  w <- WEIGHTS[[p]]
  daily <- as.numeric(d$port_simple[, p])
  for (h in HORIZONS) {
    # Overlapping h-day returns for the historical method
    hret <- if (h == 1) daily else
      as.numeric(rollapply(1 + daily, h, prod, align = "right")) - 1
    mc <- as.numeric(sim_cache[[as.character(h)]][, names(w)] %*% w)
    if (h == 10) sims_10d[[p]] <- mc
    for (a in LEVELS) {
      hs <- hist_var_es(hret, a)
      pm <- param_var_es(daily, a, h)
      ms <- hist_var_es(mc, a)
      rows[[length(rows) + 1]] <- data.frame(
        Portfolio = p, Horizon_days = h, Confidence = a,
        Method = c("Historical", "Parametric (normal)", "Monte Carlo (t)"),
        VaR_USD = PORT_VALUE * c(hs[["VaR"]], pm[["VaR"]], ms[["VaR"]]),
        ES_USD  = PORT_VALUE * c(hs[["ES"]],  pm[["ES"]],  ms[["ES"]]))
    }
  }
}
var_tab <- bind_rows(rows)
save_table(var_tab %>% mutate(across(c(VaR_USD, ES_USD), round)), "var_es")
print(var_tab %>% filter(Horizon_days == 10, Confidence == 0.95) %>%
        mutate(across(c(VaR_USD, ES_USD), round)))

# ---- Figure: simulated 10-day P&L distribution with VaR and ES -------------
sim_df <- bind_rows(lapply(PORTFOLIOS, function(p)
  data.frame(Portfolio = p, PnL = PORT_VALUE * sims_10d[[p]])))
sim_df$Portfolio <- factor(sim_df$Portfolio, levels = PORTFOLIOS)
# Trim the extreme 0.5% right tail for display only (it stretches the axis)
sim_df <- sim_df %>% group_by(Portfolio) %>%
  filter(PnL <= quantile(PnL, 0.995)) %>% ungroup()
marks <- var_tab %>%
  filter(Horizon_days == 10, Confidence == 0.95, Method == "Monte Carlo (t)") %>%
  mutate(Portfolio = factor(Portfolio, levels = PORTFOLIOS))
p <- ggplot(sim_df, aes(PnL)) +
  geom_histogram(aes(fill = Portfolio), bins = 120, alpha = 0.85) +
  geom_vline(data = marks, aes(xintercept = -VaR_USD), linewidth = 0.6) +
  geom_vline(data = marks, aes(xintercept = -ES_USD), linewidth = 0.6,
             linetype = "dashed") +
  geom_text(data = marks, aes(x = -VaR_USD, y = Inf,
                              label = paste0("VaR ", dollar(round(VaR_USD, -3)))),
            vjust = 1.5, hjust = -0.05, size = 3) +
  facet_wrap(~Portfolio, scales = "free", nrow = 2) +
  scale_fill_manual(values = PORT_COLORS, guide = "none") +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "K")) +
  labs(title = "Simulated 10-day profit and loss on $1M",
       subtitle = "Monte Carlo, multivariate Student-t; solid = 95% VaR, dashed = 95% Expected Shortfall",
       x = NULL, y = "Simulations")
save_plot(p, "07_var_es_distribution", w = 9, h = 6)
