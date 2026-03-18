##### test_charts_PSO_coverage3.R #####
# Coverage: charts.PSO.R remaining uncovered paths

skip_if_not_installed("pso")

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
colnames(R3) <- c("A", "B", "C")

portf <- portfolio.spec(assets = colnames(R3))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
portf <- add.objective(portf, type = "risk", name = "ES")
portf <- add.objective(portf, type = "return", name = "mean")

set.seed(8234)
opt_pso <- suppressWarnings(suppressMessages(
  optimize.portfolio(R3, portf, optimize_method = "pso",
                     search_size = 500, trace = TRUE)
))

# ===========================================================================
# 1. Input validation
# ===========================================================================

test_that("chart.Weight.pso errors on wrong class", {
  expect_error(chart.Weight.pso(list()), "class")
})

test_that("chart.Scatter.pso errors on wrong class", {
  expect_error(chart.Scatter.pso(list()), "class")
})

test_that("chart.Scatter.pso errors when R is NULL", {
  obj <- opt_pso
  obj$R <- NULL
  expect_error(chart.Scatter.pso(obj), "Returns")
})

# ===========================================================================
# 2. chart.assets=TRUE for scatter
# ===========================================================================

test_that("chart.Scatter.pso with chart.assets=TRUE", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, chart.assets = TRUE))
})

# ===========================================================================
# 3. Neighbors — single integer and vector
# ===========================================================================

test_that("chart.Scatter.pso with single-number neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, neighbors = 3))
})

test_that("chart.Scatter.pso with vector neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, neighbors = c(1, 2)))
})

# ===========================================================================
# 4. Styling edge cases
# ===========================================================================

test_that("chart.Weight.pso with main='' and las=0", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso, main = "", las = 0))
})

test_that("chart.Weight.pso with xlab triggers non-NULL margin", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso, xlab = "Assets"))
})

# ===========================================================================
# 5. Long column names — truncation path
# ===========================================================================

test_that("chart.Weight.pso truncates long column names", {
  obj <- opt_pso
  long_names <- paste0("VeryLongAssetNameNumber_", 1:3)
  names(obj$weights) <- long_names
  obj$portfolio$assets <- setNames(rep(1/3, 3), long_names)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(obj))
})
