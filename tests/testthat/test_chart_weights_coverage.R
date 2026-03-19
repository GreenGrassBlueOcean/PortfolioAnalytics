context("chart.Weights.R coverage")

# --- barplotWeights edge cases (called indirectly via chart.Weights barplot) ------

test_that("barplotWeights with xlab covers minmargin=5 path", {
  data(edhec, package = "PerformanceAnalytics")
  R <- edhec[1:36, 1:4]
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R, portf, optimize_method = "ROI")

  png(tf <- tempfile(fileext = ".png"))
  on.exit({ dev.off(); unlink(tf) })
  expect_no_error(chart.Weights(opt, plot.type = "bar", xlab = "Assets"))
})

test_that("barplotWeights with las=0 covers bottommargin=minmargin path", {
  data(edhec, package = "PerformanceAnalytics")
  R <- edhec[1:36, 1:4]
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R, portf, optimize_method = "ROI")

  png(tf <- tempfile(fileext = ".png"))
  on.exit({ dev.off(); unlink(tf) })
  expect_no_error(chart.Weights(opt, plot.type = "bar", las = 0))
})

test_that("barplotWeights with extremely long names covers bottommargin>10 clip", {
  data(edhec, package = "PerformanceAnalytics")
  R <- edhec[1:36, 1:3]
  long_names <- paste0(strrep("VeryLongAssetName_", 17), colnames(R))
  colnames(R) <- long_names
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.1, max = 0.8)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R, portf, optimize_method = "ROI")

  png(tf <- tempfile(fileext = ".png"), width = 1200, height = 1200, res = 72)
  on.exit({ dev.off(); unlink(tf) })
  expect_no_error(chart.Weights(opt, plot.type = "bar", las = 3))
})

# --- chart.Weights.optimize.portfolio.rebalancing --------------------------------

test_that("chart.Weights.optimize.portfolio.rebalancing works", {
  data(edhec, package = "PerformanceAnalytics")
  R <- edhec[1:36, 1:3]
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.1, max = 0.8)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt_rebal <- optimize.portfolio.rebalancing(
    R, portf,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24
  )

  png(tf <- tempfile(fileext = ".png"))
  on.exit({ dev.off(); unlink(tf) })
  expect_no_error(chart.Weights(opt_rebal, main = "Rebal Weights"))
})
