library(testthat)
library(PortfolioAnalytics)

context("Extended chart smoke tests: DE, efficient frontier, risk budget")

# ---- Shared data ----
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

base_portf <- portfolio.spec(assets = colnames(R5))
base_portf <- add.constraint(base_portf, type = "weight_sum",
                              min_sum = 0.99, max_sum = 1.01)
base_portf <- add.constraint(base_portf, type = "box", min = 0.05, max = 0.55)
base_portf <- add.objective(base_portf, type = "return", name = "mean")
base_portf <- add.objective(base_portf, type = "risk", name = "StdDev")

# ============================================================================
# A. DEoptim chart dispatches
# ============================================================================

test_that("DEoptim chart.Weights runs without error", {
  skip_if_not_installed("DEoptim")

  set.seed(2943)
  opt_de <- optimize.portfolio(R5, base_portf,
                               optimize_method = "DEoptim",
                               search_size = 500,
                               trace = TRUE,
                               DEoptim.control = list(itermax = 5,
                                                      trace = FALSE))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.Weights(opt_de), NA)
})

test_that("DEoptim chart.Weights barplot runs without error", {
  skip_if_not_installed("DEoptim")

  set.seed(2943)
  opt_de <- optimize.portfolio(R5, base_portf,
                               optimize_method = "DEoptim",
                               search_size = 500,
                               trace = TRUE,
                               DEoptim.control = list(itermax = 5,
                                                      trace = FALSE))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.Weights(opt_de, plot.type = "bar"), NA)
})

test_that("DEoptim chart.RiskReward runs without error", {
  skip_if_not_installed("DEoptim")

  set.seed(2943)
  opt_de <- optimize.portfolio(R5, base_portf,
                               optimize_method = "DEoptim",
                               search_size = 500,
                               trace = TRUE,
                               DEoptim.control = list(itermax = 5,
                                                      trace = FALSE))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.RiskReward(opt_de, return.col = "mean",
                                 risk.col = "StdDev"), NA)
})

test_that("DEoptim plot method runs without error", {
  skip_if_not_installed("DEoptim")

  set.seed(2943)
  opt_de <- optimize.portfolio(R5, base_portf,
                               optimize_method = "DEoptim",
                               search_size = 500,
                               trace = TRUE,
                               DEoptim.control = list(itermax = 5,
                                                      trace = FALSE))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(plot(opt_de, return.col = "mean", risk.col = "StdDev"), NA)
})

# ============================================================================
# B. Efficient frontier charts (random-based)
# ============================================================================

test_that("chart.EfficientFrontier runs for random optimization", {
  set.seed(6734)
  opt_random <- optimize.portfolio(R5, base_portf,
                                   optimize_method = "random",
                                   search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.EfficientFrontier(opt_random, match.col = "StdDev"),
    NA
  )
})

test_that("chart.EF.Weights runs for random optimization", {
  set.seed(6734)
  opt_random <- optimize.portfolio(R5, base_portf,
                                   optimize_method = "random",
                                   search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.EF.Weights(opt_random, match.col = "StdDev"), NA)
})

# ============================================================================
# C. Efficient frontier charts (ROI-based)
# ============================================================================

test_that("chart.EfficientFrontier runs for ROI optimization (StdDev)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_roi <- portfolio.spec(assets = colnames(R5))
  portf_roi <- add.constraint(portf_roi, type = "full_investment")
  portf_roi <- add.constraint(portf_roi, type = "box", min = 0.05, max = 0.55)
  portf_roi <- add.objective(portf_roi, type = "risk", name = "StdDev")

  opt_roi <- optimize.portfolio(R5, portf_roi,
                                optimize_method = "ROI", trace = TRUE)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.EfficientFrontier(opt_roi, match.col = "StdDev",
                            n.portfolios = 10),
    NA
  )
})

test_that("chart.EF.Weights runs for ROI (efficient.frontier object)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_roi <- portfolio.spec(assets = colnames(R5))
  portf_roi <- add.constraint(portf_roi, type = "full_investment")
  portf_roi <- add.constraint(portf_roi, type = "box", min = 0.05, max = 0.55)
  portf_roi <- add.objective(portf_roi, type = "risk", name = "StdDev")

  ef <- create.EfficientFrontier(R5, portf_roi, type = "mean-StdDev",
                                  n.portfolios = 10)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.EF.Weights(ef, match.col = "StdDev"), NA)
})

test_that("chart.EfficientFrontier runs for efficient.frontier object", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_roi <- portfolio.spec(assets = colnames(R5))
  portf_roi <- add.constraint(portf_roi, type = "full_investment")
  portf_roi <- add.constraint(portf_roi, type = "box", min = 0.05, max = 0.55)
  portf_roi <- add.objective(portf_roi, type = "risk", name = "StdDev")

  ef <- create.EfficientFrontier(R5, portf_roi, type = "mean-StdDev",
                                  n.portfolios = 10)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.EfficientFrontier(ef, match.col = "StdDev"), NA)
})

# ============================================================================
# D. chart.EfficientFrontierOverlay
# ============================================================================

test_that("chart.EfficientFrontierOverlay works with multiple portfolio specs", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "box", min = 0.05, max = 0.55)
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.constraint(p2, type = "box", min = 0.10, max = 0.45)
  p2 <- add.objective(p2, type = "risk", name = "StdDev")

  plist <- combine.portfolios(list(p1, p2))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.EfficientFrontierOverlay(R5,
                                    portfolio_list = plist,
                                    type = "mean-StdDev",
                                    n.portfolios = 10,
                                    match.col = "StdDev"),
    NA
  )
})

# ============================================================================
# E. chart.RiskBudget for opt.list
# ============================================================================

test_that("chart.RiskBudget works for opt.list object", {
  portf_rb <- portfolio.spec(assets = colnames(R5))
  portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                              min_sum = 0.99, max_sum = 1.01)
  portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
  portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
  portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                            min_concentration = TRUE)

  set.seed(3856)
  opt1 <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                             search_size = 200, trace = TRUE)
  set.seed(4927)
  opt2 <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                             search_size = 200, trace = TRUE)
  opt_list <- combine.optimizations(list(opt1 = opt1, opt2 = opt2))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.RiskBudget(opt_list, match.col = "StdDev", risk.type = "percentage"),
    NA
  )
})

# ============================================================================
# F. chart.Concentration with different conc.type
# ============================================================================

test_that("chart.Concentration works with conc.type='pct_contrib'", {
  portf_rb <- portfolio.spec(assets = colnames(R5))
  portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                              min_sum = 0.99, max_sum = 1.01)
  portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
  portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
  portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                            min_concentration = TRUE)

  set.seed(7821)
  opt_rb <- optimize.portfolio(R5, portf_rb,
                               optimize_method = "random",
                               search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.Concentration(opt_rb, return.col = "mean",
                        risk.col = "StdDev", conc.type = "pct_contrib"),
    NA
  )
})

# ============================================================================
# G. EF weights by groups
# ============================================================================

test_that("chart.EF.Weights by.groups works for efficient.frontier object", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_g <- portfolio.spec(assets = colnames(R5))
  portf_g <- add.constraint(portf_g, type = "full_investment")
  portf_g <- add.constraint(portf_g, type = "box", min = 0.05, max = 0.55)
  portf_g <- add.constraint(portf_g, type = "group",
                             groups = list(1:2, 3:5),
                             group_min = c(0.2, 0.3),
                             group_max = c(0.7, 0.8))
  portf_g <- add.objective(portf_g, type = "risk", name = "StdDev")

  ef <- create.EfficientFrontier(R5, portf_g, type = "mean-StdDev",
                                  n.portfolios = 10)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.EF.Weights(ef, match.col = "StdDev", by.groups = TRUE), NA)
})
