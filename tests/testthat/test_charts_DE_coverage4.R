##### test_charts_DE_coverage4.R #####
# Coverage: charts.DE.R — applyFUN fallback for return.col, matrix neighbors
#           (regression test for bug fix #4), chart.assets argument dedup

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

set.seed(5637)
opt_de <- suppressWarnings(suppressMessages(
  optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                     search_size = 500, trace = TRUE,
                     traceDE = 0,
                     DEoptim.control = list(itermax = 25, trace = FALSE))
))

# ===========================================================================
# 1. applyFUN fallback — non-matching return.col (lines 126-154)
#    coverage3 test 3 triggers via risk.col="StdDev"; this tests return.col
# ===========================================================================

test_that("chart.Scatter.DE applyFUN fallback for non-matching return.col", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # "StdDev" is not in extractStats (which has mean + ES), triggers applyFUN
  # for risk.col. return.col="mean" should still match directly.
  # Pass both non-matching to hit both applyFUN branches.
  expect_warning(
    chart.RiskReward(opt_de, return.col = "VaR", risk.col = "StdDev"),
    "do  not match"
  )
})

# ===========================================================================
# 2. Neighbors with matrix/data.frame (lines 199-210)
#    Regression test for bug fix #4 (rsc assignment)
# ===========================================================================

test_that("chart.Scatter.DE with matrix neighbors plots correctly", {
  xtract <- extractStats(opt_de)
  nb <- xtract[1:3, , drop = FALSE]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_de, neighbors = nb)
  )
})

# ===========================================================================
# 3. Both return.col and risk.col non-matching → optimal point fallback
#    (lines 289-301)
# ===========================================================================

test_that("chart.Scatter.DE optimal point applyFUN for both metrics", {
  obj <- opt_de
  # Replace objective_measures with names that won't match return.col/risk.col
  # in the optimal point lookup (lines 270-301). The extractStats output
  # still has the real columns so the scatter plot itself works, but the
  # optimal point code (objcols) will fall through to applyFUN.
  obj$objective_measures <- list(custom_metric = 42)
  obj$constrained_objective <- NULL
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # No warning expected here — the extractStats fallback path already
  # matched. We just want no error from the optimal point applyFUN path.
  expect_no_error(chart.RiskReward(obj))
})

# ===========================================================================
# 4. chart.assets=TRUE with argument extraction + dedup (lines 157-176)
# ===========================================================================

test_that("chart.Scatter.DE chart.assets=TRUE extracts arguments from objectives", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # This exercises tmp.args extraction, dedup, and scatterFUN calls
  expect_no_error(
    chart.RiskReward(opt_de, chart.assets = TRUE)
  )
})
