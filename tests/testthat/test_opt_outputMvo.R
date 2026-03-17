context("opt.outputMvo coverage (0% -> full)")

library(testthat)
library(PortfolioAnalytics)

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "full_investment")
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "risk", name = "var")
portf <- add.objective(portf, type = "return", name = "mean")

opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")

# ============================================================================
# A. Basic call with default (monthly) annualization
# ============================================================================

test_that("opt.outputMvo returns correct structure", {
  res <- opt.outputMvo(opt, R5)
  expect_type(res, "list")
  expect_named(res, c("Wgts", "Mean", "StdDev", "SR"))
  expect_equal(length(res$Wgts), 5)
})

# ============================================================================
# B. Annualization factors
# ============================================================================

test_that("opt.outputMvo monthly annualization is correct", {
  res_ann <- opt.outputMvo(opt, R5, frequency = "monthly", annualize = TRUE)
  res_raw <- opt.outputMvo(opt, R5, annualize = FALSE)
  
  expect_equal(res_ann$Mean, res_raw$Mean * 12, tolerance = 1e-10)
  expect_equal(res_ann$StdDev, res_raw$StdDev * sqrt(12), tolerance = 1e-10)
})

test_that("opt.outputMvo weekly annualization is correct", {
  res_ann <- opt.outputMvo(opt, R5, frequency = "weekly", annualize = TRUE)
  res_raw <- opt.outputMvo(opt, R5, annualize = FALSE)
  
  expect_equal(res_ann$Mean, res_raw$Mean * 52, tolerance = 1e-10)
  expect_equal(res_ann$StdDev, res_raw$StdDev * sqrt(52), tolerance = 1e-10)
})

test_that("opt.outputMvo daily annualization is correct", {
  res_ann <- opt.outputMvo(opt, R5, frequency = "daily", annualize = TRUE)
  res_raw <- opt.outputMvo(opt, R5, annualize = FALSE)
  
  expect_equal(res_ann$Mean, res_raw$Mean * 260, tolerance = 1e-10)
  expect_equal(res_ann$StdDev, res_raw$StdDev * sqrt(260), tolerance = 1e-10)
})

# ============================================================================
# C. annualize = FALSE
# ============================================================================

test_that("opt.outputMvo with annualize=FALSE returns raw values", {
  res <- opt.outputMvo(opt, R5, annualize = FALSE)
  expect_type(res$Mean, "double")
  expect_type(res$StdDev, "double")
  expect_true(res$StdDev > 0)
})

# ============================================================================
# D. digits parameter
# ============================================================================

test_that("opt.outputMvo rounds with digits parameter", {
  res3 <- opt.outputMvo(opt, R5, digits = 3, annualize = FALSE)
  res_raw <- opt.outputMvo(opt, R5, annualize = FALSE)
  
  expect_equal(res3$Mean, round(res_raw$Mean, 3))
  expect_equal(res3$StdDev, round(res_raw$StdDev, 3))
})

# ============================================================================
# E. Non-zero risk-free rate
# ============================================================================

test_that("opt.outputMvo with non-zero rf changes Sharpe", {
  res_0 <- opt.outputMvo(opt, R5, rf = 0.0, annualize = FALSE)
  res_rf <- opt.outputMvo(opt, R5, rf = 0.001, annualize = FALSE)
  
  # With positive rf, SR should decrease (numerator decreases)
  expect_true(res_rf$SR < res_0$SR)
})

# ============================================================================
# F. xts vs matrix input
# ============================================================================

test_that("opt.outputMvo gives same results for xts and matrix", {
  res_xts <- opt.outputMvo(opt, R5, annualize = FALSE)
  res_mat <- opt.outputMvo(opt, as.matrix(R5), annualize = FALSE)
  
  expect_equal(res_xts$Mean, res_mat$Mean, tolerance = 1e-10)
  expect_equal(res_xts$StdDev, res_mat$StdDev, tolerance = 1e-10)
  expect_equal(res_xts$SR, res_mat$SR, tolerance = 1e-10)
})

# ============================================================================
# G. Unknown frequency (no annualization multiplier)
# ============================================================================

test_that("opt.outputMvo with unrecognized frequency defaults to daily", {
  res_unk <- opt.outputMvo(opt, R5, frequency = "quarterly", annualize = TRUE)
  res_raw <- opt.outputMvo(opt, R5, annualize = FALSE)
  
  # Unrecognized frequency hits the else branch, which uses daily (260)
  expect_equal(res_unk$Mean, res_raw$Mean * 260, tolerance = 1e-10)
  expect_equal(res_unk$StdDev, res_raw$StdDev * sqrt(260), tolerance = 1e-10)
})
