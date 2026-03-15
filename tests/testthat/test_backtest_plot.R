##### test_backtest_plot.R #####
# Phase 3D: backtest.plot coverage
# Targets: backtest.plot.R from 0% → ~80%
#   - plotType: 'both', 'cumRet', 'drawdown'
#   - log_return: TRUE/FALSE
#   - drawdown_on: NULL, integer
#   - Multi-asset overlay (n > 1)
#   - Custom style parameters (colorSet, ltySet, lwdSet)
#   - Custom main title

library(testthat)
library(PortfolioAnalytics)
library(xts)

# Generate simple return data for testing
data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:60, 1:3]
colnames(R3) <- c("Port1", "Port2", "Port3")
R1 <- R3[, 1, drop = FALSE]

# ===========================================================================
# A. Basic single-asset plots
# ===========================================================================

test_that("backtest.plot 'both' mode single asset runs without error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R1))
})

test_that("backtest.plot 'cumRet' mode single asset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R1, plotType = "cumRet"))
})

test_that("backtest.plot 'drawdown' mode single asset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R1, plotType = "drawdown"))
})

# ===========================================================================
# B. Multi-asset overlay (n > 1)
# ===========================================================================

test_that("backtest.plot 'both' mode multi-asset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R3))
})

test_that("backtest.plot 'cumRet' mode multi-asset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R3, plotType = "cumRet"))
})

test_that("backtest.plot 'drawdown' mode multi-asset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R3, plotType = "drawdown"))
})

# ===========================================================================
# C. log_return = TRUE
# ===========================================================================

test_that("backtest.plot with log_return=TRUE both mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R3, log_return = TRUE, plotType = "both"))
})

test_that("backtest.plot with log_return=TRUE cumRet mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R3, log_return = TRUE, plotType = "cumRet"))
})

# ===========================================================================
# D. drawdown_on parameter
# ===========================================================================

test_that("backtest.plot with drawdown_on=NULL suppresses shading", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R3, drawdown_on = NULL))
})

test_that("backtest.plot with drawdown_on=2 tracks second portfolio", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R3, drawdown_on = 2))
})

# ===========================================================================
# E. Custom styling
# ===========================================================================

test_that("backtest.plot with custom colorSet", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    backtest.plot(R3, colorSet = c("red", "blue", "green"))
  )
})

test_that("backtest.plot with custom ltySet and lwdSet", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    backtest.plot(R3, ltySet = c(1, 2, 3), lwdSet = c(1, 2, 3))
  )
})

test_that("backtest.plot with custom main title", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    backtest.plot(R3, main = "My Backtest")
  )
})

# ===========================================================================
# F. Return value
# ===========================================================================

test_that("backtest.plot returns a plot object", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  p <- backtest.plot(R3, plotType = "cumRet")
  expect_true(!is.null(p))
})

test_that("backtest.plot with drawdown_on=NULL and both mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  p <- backtest.plot(R3, drawdown_on = NULL, plotType = "both")
  expect_true(!is.null(p))
})

# ===========================================================================
# G. Alternative plotType aliases
# ===========================================================================

test_that("backtest.plot with plotType='ret' alias", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R1, plotType = "ret"))
})

test_that("backtest.plot with plotType='cumret' alias", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(backtest.plot(R1, plotType = "cumret"))
})
