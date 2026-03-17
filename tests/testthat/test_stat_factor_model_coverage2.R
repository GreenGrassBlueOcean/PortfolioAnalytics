##### test_stat_factor_model_coverage2.R #####
# Phase 3C coverage: stat.factor.model.R — input validation, center(),
#   dimension mismatch errors, non-xts input

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]

# ===========================================================================
# A. statistical.factor.model with data.frame input (non-xts coercion)
# ===========================================================================

test_that("statistical.factor.model handles data.frame input", {
  # data.frame without time index -> hits try(as.xts(R)) -> try-error -> stop

  df <- as.data.frame(coredata(R4))
  expect_error(
    PortfolioAnalytics::statistical.factor.model(df, k = 1),
    "xts object"
  )
})

# ===========================================================================
# B. statistical.factor.model with m < N
# ===========================================================================

test_that("statistical.factor.model errors when m < N", {
  # 3 observations, 4 assets: m < N
  R_short <- R4[1:3, ]
  expect_error(
    PortfolioAnalytics::statistical.factor.model(R_short, k = 1),
    "observations|rows|m must|larger"
  )
})

# ===========================================================================
# C. statistical.factor.model with k=0
# ===========================================================================

test_that("statistical.factor.model errors when k <= 0", {
  expect_error(
    PortfolioAnalytics::statistical.factor.model(R4, k = 0),
    "k must|positive|factors"
  )
})

# ===========================================================================
# D. center() function
# ===========================================================================

test_that("center() function centers a matrix", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  centered <- PortfolioAnalytics:::center(mat)
  # Column means should be ~0 after centering
  expect_true(all(abs(colMeans(centered)) < 1e-10))
})

# ===========================================================================
# E. covarianceSF with dimension mismatch
# ===========================================================================

test_that("covarianceSF errors on mismatched dimensions", {
  # beta length 4, stockM2 length 3 -> dimension mismatch
  expect_error(
    PortfolioAnalytics:::covarianceSF(
      beta = 1:4,
      stockM2 = 1:3,
      factorM2 = 1.0
    ),
    "dimensions do not match"
  )
})

# ===========================================================================
# F. covarianceMF with non-matrix beta
# ===========================================================================

test_that("covarianceMF errors when beta is not a matrix", {
  expect_error(
    PortfolioAnalytics:::covarianceMF(
      beta = c(0.1, 0.2, 0.3),
      stockM2 = c(0.01, 0.02, 0.03),
      factorM2 = matrix(1)
    ),
    "beta must be a matrix"
  )
})
