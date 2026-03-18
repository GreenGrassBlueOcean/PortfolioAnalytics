##### test_charts_PSO_coverage4.R #####
# Coverage: charts.PSO.R — applyFUN fallback, chart.assets, optimal point
#           fallback, constrained_objective slot, neighbors (bug fix #3)

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

set.seed(6712)
opt_pso <- suppressWarnings(suppressMessages(
  optimize.portfolio(R3, portf, optimize_method = "pso",
                     search_size = 500, trace = TRUE)
))

# ===========================================================================
# 1. applyFUN fallback path (lines 108-136)
# ===========================================================================

test_that("chart.Scatter.pso applyFUN fallback for non-matching risk.col", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(
    chart.RiskReward(opt_pso, risk.col = "StdDev"),
    "do  not match"
  )
})

# ===========================================================================
# 2. chart.assets=TRUE argument extraction (lines 137-156)
# ===========================================================================

test_that("chart.Scatter.pso chart.assets=TRUE extracts scatterFUN arguments", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_pso, chart.assets = TRUE)
  )
})

# ===========================================================================
# 3. Optimal point applyFUN fallback (lines 181-193)
# ===========================================================================

test_that("chart.Scatter.pso optimal point applyFUN when objective_measures unmatched", {
  obj <- opt_pso
  obj$objective_measures <- list(custom_metric = 42)
  obj$constrained_objective <- NULL
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(obj))
})

# ===========================================================================
# 4. constrained_objective slot dispatch (lines 162-166)
# ===========================================================================

test_that("chart.Scatter.pso uses constrained_objective slot when present", {
  obj <- opt_pso
  obj$constrained_objective <- obj$objective_measures
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(obj))
})

# ===========================================================================
# 5. Neighbors — single integer (regression test for bug fix #3)
# ===========================================================================

test_that("chart.Scatter.pso with single-number neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_pso, neighbors = 3)
  )
})

# ===========================================================================
# 6. Neighbors — vector (regression test for bug fix #3)
# ===========================================================================

test_that("chart.Scatter.pso with vector neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_pso, neighbors = c(1, 2))
  )
})

# ===========================================================================
# 7. Neighbors — matrix (regression test for bug fix #3)
# ===========================================================================

test_that("chart.Scatter.pso with matrix neighbors", {
  xtract <- extractStats(opt_pso)
  nb <- xtract[1:3, , drop = FALSE]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_pso, neighbors = nb)
  )
})
