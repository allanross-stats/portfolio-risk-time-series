# 05_garch.R
# GARCH(1,1) with Student-t errors for each portfolio.
#   sigma^2_t = omega + alpha * e^2_{t-1} + beta * sigma^2_{t-1}
# alpha = reaction to yesterday's shock, beta = memory of past volatility,
# alpha + beta = persistence (close to 1 = volatility shocks fade slowly).

source("R/00_setup.R")
d <- readRDS("data/returns.rds")
r <- d$port_log

fit_garch <- function(x) {
  garchFit(~ garch(1, 1), data = 100 * x, cond.dist = "std",
           include.mean = TRUE, trace = FALSE)
}

garch_rows <- list()
cond_vol <- list()
for (p in PORTFOLIOS) {
  x <- as.numeric(r[, p])
  fit <- fit_garch(x)
  cf <- coef(fit)
  persistence <- cf[["alpha1"]] + cf[["beta1"]]
  garch_rows[[p]] <- data.frame(
    Portfolio   = p,
    omega       = cf[["omega"]],
    alpha       = cf[["alpha1"]],
    beta        = cf[["beta1"]],
    persistence = persistence,
    half_life_days = log(0.5) / log(persistence),
    t_dof       = cf[["shape"]],
    long_run_ann_vol = sqrt(cf[["omega"]] / (1 - persistence)) / 100 *
                       sqrt(TRADING_DAYS)
  )
  # Annualized conditional volatility
  cond_vol[[p]] <- xts(volatility(fit) / 100 * sqrt(TRADING_DAYS), index(r))
}
garch_tab <- bind_rows(garch_rows)
print(garch_tab, digits = 3)
save_table(garch_tab %>% mutate(across(where(is.numeric), ~ signif(.x, 4))),
           "garch_parameters")

cv <- do.call(merge, cond_vol); colnames(cv) <- PORTFOLIOS
saveRDS(cv, "data/garch_conditional_vol.rds")

p <- ggplot(xts_to_long(cv, "Vol"), aes(Date, Vol, color = Series)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = PORT_COLORS, name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(title = "GARCH(1,1) conditional volatility",
       subtitle = "Annualized; volatility clusters and decays after shocks such as March 2020",
       x = NULL, y = NULL)
save_plot(p, "06_garch_conditional_vol")
