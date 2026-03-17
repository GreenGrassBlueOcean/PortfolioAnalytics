##### test_charts_DE_coverage2.R #####
# Phase 3 coverage: chart.Weight.DE edge cases + chart.Scatter.DE neighbors

library(testthat)
library(PortfolioAnalytics)

skip_if_not_installed("DEoptim")

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "risk", name = "ES")
portf <- add.objective(portf, type = "return", name = "mean")

set.seed(7341)
opt_de <- suppressWarnings(suppressMessages(
  optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                     search_size = 500, trace = TRUE,
                     DEoptim.control = list(itermax = 25, trace = FALSE))
))

# ===========================================================================
# A. chart.Weight.DE — styling edge cases
# ===========================================================================

test_that("chart.Weight.DE with xlab triggers non-NULL margin path", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_de, xlab = "Assets"))
})

test_that("chart.Weight.DE with main='' triggers empty title margin", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_de, main = ""))
})

test_that("chart.Weight.DE with infinite constraints uses weight-based ylim", {
  opt_inf <- opt_de
  # Remove box constraint so get_constraints returns -Inf/Inf bounds
  opt_inf$portfolio$constraints <- opt_inf$portfolio$constraints[1]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_inf))
})

# ===========================================================================
# B. chart.Scatter.DE — neighbors (3 input formats)
# ===========================================================================

test_that("chart.Scatter.DE with single-number neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_de, neighbors = 3))
})

test_that("chart.Scatter.DE with vector-of-indices neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_de, neighbors = c(1, 2, 5)))
})

test_that("chart.Scatter.DE with matrix neighbors", {
  xtract <- extractStats(opt_de)
  nb <- xtract[1:3, , drop = FALSE]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_de, neighbors = nb))
})
