library(testthat)
library(PortfolioAnalytics)

context("Statistical Factor Model: PCA-based comoment estimation")

data(edhec)
R <- edhec[1:60, 1:4]

# ============================================================================
# A. center()
# ============================================================================

test_that("center produces zero-mean columns", {
  x <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  cx <- center(x)
  expect_equal(colMeans(cx), c(0, 0), tolerance = 1e-12)
  expect_equal(nrow(cx), 3)
  expect_equal(ncol(cx), 2)
})

test_that("center requires a matrix", {
  expect_error(center(1:5), "must be a matrix")
})

# ============================================================================
# B. statistical.factor.model()
# ============================================================================

test_that("statistical.factor.model works with k=1", {
  model <- statistical.factor.model(R, k = 1)
  expect_s3_class(model, "stfm")

  # k=1 drops to vector; check length instead of dim
  expect_equal(length(model$factor_loadings), 4)
  expect_equal(NROW(model$factor_realizations), 60)
  expect_equal(NCOL(model$factor_realizations), 1)
  expect_equal(dim(model$residuals), c(60, 4))
  expect_equal(model$m, 60)
  expect_equal(model$k, 1L)
  expect_equal(model$N, 4)
})

test_that("statistical.factor.model works with k=2", {
  model <- statistical.factor.model(R, k = 2)
  expect_s3_class(model, "stfm")

  expect_equal(ncol(model$factor_loadings), 2)
  expect_equal(ncol(model$factor_realizations), 2)
  expect_equal(model$k, 2L)
})

test_that("statistical.factor.model rejects invalid inputs", {
  expect_error(statistical.factor.model(R, k = 0), "k must be a positive integer")
  # fewer obs than assets
  expect_error(statistical.factor.model(R[1:3, ]), "fewer observations than assets")
})

# ============================================================================
# C. Single-factor comoment functions
# ============================================================================

test_that("covarianceSF produces correct N x N matrix", {
  beta <- c(0.8, 0.6, 0.4, 0.3)
  stockM2 <- c(0.01, 0.02, 0.015, 0.008)
  factorM2 <- 0.05

  result <- PortfolioAnalytics:::covarianceSF(beta, stockM2, factorM2)
  expect_equal(dim(result), c(4, 4))
  expect_true(isSymmetric(result))
  # Diagonal should be beta^2 * factorM2 + stockM2
  expected_diag <- beta^2 * factorM2 + stockM2
  expect_equal(diag(result), expected_diag, tolerance = 1e-10)
})

test_that("covarianceSF rejects mismatched dimensions", {
  expect_error(
    PortfolioAnalytics:::covarianceSF(c(0.5, 0.3), c(0.01), 0.05),
    "dimensions do not match"
  )
})

test_that("coskewnessSF produces correct N x N^2 matrix", {
  beta <- c(0.8, 0.6, 0.4)
  stockM3 <- c(0.001, -0.002, 0.0005)
  factorM3 <- 0.003

  result <- PortfolioAnalytics:::coskewnessSF(beta, stockM3, factorM3)
  expect_equal(dim(result), c(3, 9))
})

test_that("cokurtosisSF produces correct N x N^3 matrix via C code", {
  beta <- c(0.8, 0.6, 0.4)
  stockM2 <- c(0.01, 0.02, 0.015)
  stockM4 <- c(0.001, 0.003, 0.002)
  factorM2 <- 0.05
  factorM4 <- 0.008

  result <- PortfolioAnalytics:::cokurtosisSF(beta, stockM2, stockM4, factorM2, factorM4)
  expect_equal(dim(result), c(3, 27))
})

# ============================================================================
# D. Multi-factor comoment functions
# ============================================================================

test_that("covarianceMF produces correct N x N matrix", {
  N <- 4; k <- 2
  beta <- matrix(rnorm(N * k), nrow = N, ncol = k)
  stockM2 <- abs(rnorm(N)) * 0.01
  factorM2 <- cov(matrix(rnorm(100 * k), ncol = k))

  result <- PortfolioAnalytics:::covarianceMF(beta, stockM2, factorM2)
  expect_equal(dim(result), c(N, N))
  expect_true(isSymmetric(result))
})

test_that("covarianceMF rejects non-matrix beta", {
  expect_error(
    PortfolioAnalytics:::covarianceMF(c(0.5, 0.3), c(0.01, 0.02), matrix(0.05)),
    "beta must be a matrix"
  )
})

test_that("coskewnessMF produces correct N x N^2 matrix", {
  set.seed(8234)
  N <- 3; k <- 2
  beta <- matrix(rnorm(N * k), nrow = N, ncol = k)
  stockM3 <- rnorm(N) * 0.001
  f <- matrix(rnorm(100 * k), ncol = k)
  factorM3 <- PerformanceAnalytics::M3.MM(f)

  result <- PortfolioAnalytics:::coskewnessMF(beta, stockM3, factorM3)
  expect_equal(dim(result), c(N, N^2))
})

test_that("cokurtosisMF produces correct N x N^3 matrix via C code", {
  set.seed(4561)
  N <- 3; k <- 2
  beta <- matrix(rnorm(N * k), nrow = N, ncol = k)
  stockM2 <- abs(rnorm(N)) * 0.01
  stockM4 <- abs(rnorm(N)) * 0.001
  f <- matrix(rnorm(100 * k), ncol = k)
  factorM2 <- cov(f)
  factorM4 <- PerformanceAnalytics::M4.MM(f)

  result <- PortfolioAnalytics:::cokurtosisMF(beta, stockM2, stockM4, factorM2, factorM4)
  expect_equal(dim(result), c(N, N^3))
})

# ============================================================================
# E. High-level extractors
# ============================================================================

test_that("extractCovariance works for k=1", {
  model <- statistical.factor.model(R, k = 1)
  covmat <- extractCovariance(model)
  expect_equal(dim(covmat), c(4, 4))
  expect_true(isSymmetric(covmat))
  # All diagonal elements should be positive (variances)
  expect_true(all(diag(covmat) > 0))
})

test_that("extractCovariance works for k=2", {
  model <- statistical.factor.model(R, k = 2)
  covmat <- extractCovariance(model)
  expect_equal(dim(covmat), c(4, 4))
  expect_true(isSymmetric(covmat))
})

test_that("extractCoskewness works for k=1", {
  model <- statistical.factor.model(R, k = 1)
  m3 <- extractCoskewness(model)
  expect_equal(dim(m3), c(4, 16))
})

test_that("extractCoskewness works for k=2", {
  model <- statistical.factor.model(R, k = 2)
  m3 <- extractCoskewness(model)
  expect_equal(dim(m3), c(4, 16))
})

test_that("extractCokurtosis works for k=1", {
  model <- statistical.factor.model(R, k = 1)
  m4 <- extractCokurtosis(model)
  expect_equal(dim(m4), c(4, 64))
})

test_that("extractCokurtosis works for k=2", {
  model <- statistical.factor.model(R, k = 2)
  m4 <- extractCokurtosis(model)
  expect_equal(dim(m4), c(4, 64))
})

test_that("extractors reject non-stfm objects", {
  expect_error(extractCovariance(list(a = 1)), "must be of class")
  expect_error(extractCoskewness(list(a = 1)), "must be of class")
  expect_error(extractCokurtosis(list(a = 1)), "must be of class")
})
