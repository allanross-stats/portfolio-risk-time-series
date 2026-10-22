# 08_efficient_frontier.R
# Long-only efficient frontier across all 11 assets (quadratic programming),
# with the four portfolios plotted against it.
# Caveat: uses in-sample average returns, which flatters assets that happened to
# do well (hindsight bias). It shows how efficient each mix WAS, not what to buy.

source("R/00_setup.R")
d <- readRDS("data/returns.rds")
R <- coredata(d$asset_simple)
mu <- colMeans(R) * TRADING_DAYS
S  <- cov(R) * TRADING_DAYS
n  <- length(mu)
rf_ann <- mean(d$rf_daily) * TRADING_DAYS

# Minimize w'Sw subject to sum(w) = 1, w'mu = target, w >= 0
frontier_point <- function(target) {
  A <- cbind(rep(1, n), mu, diag(n))
  b <- c(1, target, rep(0, n))
  sol <- tryCatch(solve.QP(Dmat = 2 * S, dvec = rep(0, n), Amat = A, bvec = b,
                           meq = 2), error = function(e) NULL)
  if (is.null(sol)) return(NULL)
  w <- pmax(sol$solution, 0); w <- w / sum(w)
  list(w = w, ret = sum(w * mu), vol = sqrt(drop(t(w) %*% S %*% w)))
}

targets <- seq(min(mu) + 1e-4, max(mu) - 1e-4, length.out = 120)
pts <- Filter(Negate(is.null), lapply(targets, frontier_point))
front <- data.frame(Vol = sapply(pts, `[[`, "vol"), Ret = sapply(pts, `[[`, "ret"))
front <- front[front$Ret >= front$Ret[which.min(front$Vol)], ]  # efficient half

sharpe_pts <- sapply(pts, function(z) (z$ret - rf_ann) / z$vol)
tangency <- pts[[which.max(sharpe_pts)]]
tan_w <- data.frame(Asset = names(mu), Weight = round(tangency$w, 4)) %>%
  filter(Weight > 0.001) %>% arrange(desc(Weight))
save_table(tan_w, "max_sharpe_weights")

port_pts <- bind_rows(lapply(PORTFOLIOS, function(p) {
  w <- setNames(rep(0, n), names(mu)); w[names(WEIGHTS[[p]])] <- WEIGHTS[[p]]
  data.frame(Portfolio = p, Ret = sum(w * mu),
             Vol = sqrt(drop(t(w) %*% S %*% w)))
}))
port_pts$Portfolio <- factor(port_pts$Portfolio, levels = PORTFOLIOS)
asset_pts <- data.frame(Asset = names(mu), Ret = mu, Vol = sqrt(diag(S)))

p <- ggplot() +
  geom_path(data = front, aes(Vol, Ret), linewidth = 1, color = "grey20") +
  geom_point(data = asset_pts, aes(Vol, Ret), color = "grey55", size = 2) +
  ggrepel::geom_text_repel(data = asset_pts, aes(Vol, Ret, label = Asset),
                           color = "grey40", size = 2.8, seed = 1) +
  geom_point(data = port_pts, aes(Vol, Ret, color = Portfolio), size = 3.5) +
  geom_point(aes(tangency$vol, tangency$ret), shape = 8, size = 3.5) +
  annotate("text", tangency$vol, tangency$ret, label = "Max Sharpe",
           hjust = -0.15, size = 3) +
  scale_color_manual(values = PORT_COLORS, name = NULL) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(title = "Efficient frontier (long-only) vs. the four portfolios",
       subtitle = "Annualized arithmetic mean return and volatility, in-sample",
       x = "Volatility", y = "Expected return")
save_plot(p, "09_efficient_frontier", w = 8, h = 5.5)
save_table(port_pts %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           "frontier_portfolio_points")
