
require(testthat)
require(PortfolioAnalytics)

context("chart smoke tests")

# ---- Shared test data ----
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# Build a basic portfolio spec with mean and StdDev objectives
base_portf <- portfolio.spec(assets = colnames(R5))
base_portf <- add.constraint(base_portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
base_portf <- add.constraint(base_portf, type = "box", min = 0.05, max = 0.55)
base_portf <- add.objective(base_portf, type = "return", name = "mean")
base_portf <- add.objective(base_portf, type = "risk", name = "StdDev")

# ---- chart.Concentration ----

test_that("chart.Concentration runs without error on random optimization", {
  set.seed(4817)
  opt_random <- optimize.portfolio(R5, base_portf, optimize_method = "random",
                                   search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Concentration(opt_random, return.col = "mean", risk.col = "StdDev",
                        conc.type = "weights")
  )
})

# ---- chart.Weights (random) ----

test_that("chart.Weights runs for random optimization", {
  set.seed(4817)
  opt_random <- optimize.portfolio(R5, base_portf, optimize_method = "random",
                                   search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_random))
})

# ---- chart.RiskReward (random) ----

test_that("chart.RiskReward runs for random optimization", {
  set.seed(4817)
  opt_random <- optimize.portfolio(R5, base_portf, optimize_method = "random",
                                   search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_random, return.col = "mean", risk.col = "StdDev")
  )
})

# ---- chart.RiskBudget (random) ----

test_that("chart.RiskBudget runs for random optimization with risk_budget objective", {
  portf_rb <- portfolio.spec(assets = colnames(R5))
  portf_rb <- add.constraint(portf_rb, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
  portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
  portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                            min_concentration = TRUE)

  set.seed(6203)
  opt_rb <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                               search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskBudget(opt_rb, risk.type = "percentage"))
})

# ---- PSO charts ----

test_that("PSO chart functions run without error", {
  skip_if_not_installed("pso")
  requireNamespace("foreach", quietly = TRUE)
  library(foreach)

  set.seed(7291)
  opt_pso <- optimize.portfolio(R5, base_portf, optimize_method = "pso",
                                trace = TRUE)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso))
  expect_no_error(chart.RiskReward(opt_pso, return.col = "mean", risk.col = "StdDev"))
  expect_no_error(plot(opt_pso, return.col = "mean", risk.col = "StdDev"))
})

# ---- GenSA charts ----

test_that("GenSA chart functions run without error", {
  skip_if_not_installed("GenSA")
  requireNamespace("foreach", quietly = TRUE)
  library(foreach)

  set.seed(3452)
  opt_gensa <- optimize.portfolio(R5, base_portf, optimize_method = "GenSA",
                                  trace = TRUE)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_gensa))
  expect_no_error(chart.RiskReward(opt_gensa, return.col = "mean", risk.col = "StdDev"))
  expect_no_error(plot(opt_gensa, return.col = "mean", risk.col = "StdDev"))
})

# ---- chart.RiskBudget for rebalancing ----

test_that("chart.RiskBudget works for rebalancing object", {
  portf_rb <- portfolio.spec(assets = colnames(R5))
  portf_rb <- add.constraint(portf_rb, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
  portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
  portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                            min_concentration = TRUE)

  set.seed(5819)
  opt_rebal <- optimize.portfolio.rebalancing(R5, portf_rb,
                                              optimize_method = "random",
                                              search_size = 200,
                                              rebalance_on = "quarters",
                                              training_period = 24,
                                              trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskBudget(opt_rebal, risk.type = "percentage",
                                   match.col = "StdDev"))
})

# ---- plot method for random optimization ----

test_that("plot.optimize.portfolio.random runs without error", {
  set.seed(4817)
  opt_random <- optimize.portfolio(R5, base_portf, optimize_method = "random",
                                   search_size = 500, trace = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(opt_random, return.col = "mean", risk.col = "StdDev"))
})
