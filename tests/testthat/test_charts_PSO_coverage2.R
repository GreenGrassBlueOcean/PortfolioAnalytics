##### test_charts_PSO_coverage2.R #####
# Phase 3 coverage: chart.Weight.pso edge cases + chart.Scatter.pso chart.assets


skip_if_not_installed("pso")

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "risk", name = "ES")
portf <- add.objective(portf, type = "return", name = "mean")

set.seed(5813)
opt_pso <- suppressWarnings(
  optimize.portfolio(R5, portf, optimize_method = "pso",
                     search_size = 500, trace = TRUE)
)

# ===========================================================================
# A. chart.Weight.pso — styling edge cases
# ===========================================================================

test_that("chart.Weight.pso with xlab triggers non-NULL margin path", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso, xlab = "Assets"))
})

test_that("chart.Weight.pso with main='' triggers empty title margin", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso, main = ""))
})

test_that("chart.Weight.pso with las=0 triggers low-las margin path", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso, las = 0))
})

test_that("chart.Weight.pso with infinite constraints uses weight-based ylim", {
  opt_inf <- opt_pso
  opt_inf$portfolio$constraints <- opt_inf$portfolio$constraints[1]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_inf))
})

# ===========================================================================
# B. chart.Scatter.pso — chart.assets=TRUE
# ===========================================================================

test_that("chart.Scatter.pso with chart.assets=TRUE plots asset scatter", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, chart.assets = TRUE))
})
