##### test_charts_solver_advanced.R #####
# Phase 3C: chart.DE, chart.RP, charts.multiple advanced paths
# Targets uncovered code:
#   - chart.Scatter.DE: chart.assets=TRUE, neighbors (vector, matrix), trajectory
#   - chart.Weight.RP: neighbors (single-number, vector indices, matrix)
#   - chart.Weights.opt.list: line + barplot modes
#   - chart.RiskReward.opt.list: chart.assets=TRUE, labels, colorset
#   - barplotOptWeights
#   - chart.Weights.optimize.portfolio.rebalancing

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ===========================================================================
# A. DEoptim chart.Scatter.DE (chart.RiskReward) with chart.assets
# ===========================================================================

test_that("DE chart.RiskReward with chart.assets=TRUE runs", {
  skip_if_not_installed("DEoptim")

  set.seed(1843)
  opt_de <- optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                               search_size = 500, trace = TRUE,
                               DEoptim.control = list(itermax = 5,
                                                      trace = FALSE))
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_de, return.col = "mean", risk.col = "StdDev",
                     chart.assets = TRUE)
  )
})

test_that("DE chart.RiskReward with custom xlim/ylim", {
  skip_if_not_installed("DEoptim")

  set.seed(1843)
  opt_de <- optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                               search_size = 500, trace = TRUE,
                               DEoptim.control = list(itermax = 5,
                                                      trace = FALSE))
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_de, return.col = "mean", risk.col = "StdDev",
                     xlim = c(0, 0.05), ylim = c(0, 0.01))
  )
})

# ===========================================================================
# B. RP chart.Weight.RP with neighbors
# ===========================================================================

set.seed(7382)
opt_rp <- optimize.portfolio(R5, portf, optimize_method = "random",
                             search_size = 500, trace = TRUE)

test_that("RP chart.Weights with single-number neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_rp, neighbors = 3))
})

test_that("RP chart.Weights with vector of portfolio indices", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_rp, neighbors = c(1, 5, 10)))
})

test_that("RP chart.Weights with matrix neighbors", {
  xtract <- extractStats(opt_rp)
  nb_mat <- head(xtract, 5)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_rp, neighbors = nb_mat))
})

test_that("RP chart.RiskReward with matrix neighbors", {
  xtract <- extractStats(opt_rp)
  nb_mat <- head(xtract, 5)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_rp, return.col = "mean", risk.col = "StdDev",
                     neighbors = nb_mat)
  )
})

# ===========================================================================
# C. chart.Weights.opt.list — line and barplot modes
# ===========================================================================

set.seed(2931)
opt1 <- optimize.portfolio(R5, portf, optimize_method = "random",
                           search_size = 200, trace = TRUE)
set.seed(3047)
opt2 <- optimize.portfolio(R5, portf, optimize_method = "random",
                           search_size = 200, trace = TRUE)
opt_list <- combine.optimizations(list(opt1 = opt1, opt2 = opt2))

test_that("chart.Weights.opt.list line mode runs", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_list, plot.type = "line"))
})

test_that("chart.Weights.opt.list barplot mode runs", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_list, plot.type = "barplot"))
})

test_that("chart.Weights.opt.list line with custom colorset and legend", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Weights(opt_list, plot.type = "line",
                  colorset = c("red", "blue"),
                  legend.loc = "topright")
  )
})

test_that("chart.Weights.opt.list line with main='' and legend.loc=NULL", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Weights(opt_list, plot.type = "line",
                  main = "", legend.loc = NULL)
  )
})

test_that("chart.Weights.opt.list rejects wrong class", {
  mock <- list(1, 2)
  class(mock) <- "not_opt_list"
  expect_error(chart.Weights(mock, plot.type = "line"), "no applicable method")
})

# ===========================================================================
# D. chart.RiskReward.opt.list
# ===========================================================================

test_that("chart.RiskReward.opt.list runs without error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_list, risk.col = "StdDev", return.col = "mean")
  )
})

test_that("chart.RiskReward.opt.list with chart.assets=TRUE", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_list, risk.col = "StdDev", return.col = "mean",
                     chart.assets = TRUE)
  )
})

test_that("chart.RiskReward.opt.list with custom colorset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_list, risk.col = "StdDev", return.col = "mean",
                     colorset = c("red", "blue"))
  )
})

test_that("chart.RiskReward.opt.list errors on invalid risk.col", {
  expect_error(
    chart.RiskReward(opt_list, risk.col = "NonExistent", return.col = "mean"),
    "not in column names"
  )
})

# ===========================================================================
# E. chart.Weights.optimize.portfolio.rebalancing
# ===========================================================================

test_that("chart.Weights for rebalancing object runs without error", {
  set.seed(4219)
  opt_rebal <- optimize.portfolio.rebalancing(
    R5, portf, optimize_method = "random",
    search_size = 200, rebalance_on = "quarters",
    training_period = 24, trace = TRUE
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_rebal))
})
