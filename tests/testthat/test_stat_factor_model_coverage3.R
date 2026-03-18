##### test_stat_factor_model_coverage3.R #####
# Coverage: stat.factor.model.R — single-factor (k=1) extract paths
# Regression: residualcokurtosisMF bug fix (missing accumulation in i==k && j==l case)

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]

# ===========================================================================
# 1. Single-factor model (k=1) — all extract methods succeed
# ===========================================================================

test_that("statistical.factor.model k=1 extract methods work", {
  sfm <- statistical.factor.model(R4, k = 1)
  expect_no_error(extractCovariance(sfm))
  expect_no_error(extractCoskewness(sfm))
  expect_no_error(extractCokurtosis(sfm))
})

# ===========================================================================
# 2. Multi-factor model (k=2) — all extract methods succeed
# ===========================================================================

test_that("statistical.factor.model k=2 extract methods work", {
  sfm2 <- statistical.factor.model(R4, k = 2)
  expect_no_error(extractCovariance(sfm2))
  expect_no_error(extractCoskewness(sfm2))
  expect_no_error(extractCokurtosis(sfm2))
})

# ===========================================================================
# 3. Regression: SF and MF cokurtosis agree when k=1
#    (catches the missing-accumulation bug in residualcokurtosisMF.c)
# ===========================================================================

test_that("SF and MF cokurtosis paths agree when k=1", {
  sfm <- statistical.factor.model(R4, k = 1)
  beta <- sfm$factor_loadings
  f <- sfm$factor_realizations
  res <- sfm$residuals
  m <- sfm$m; k <- sfm$k; N <- sfm$N
  denom <- m - k - 1

  stockM2 <- colSums(res^2) / denom
  stockM4 <- colSums(res^4) / denom
  factorM2 <- cov(f)
  factorM4 <- PerformanceAnalytics::M4.MM(f)

  # SF path (k=1 specialization)
  ku_sf <- cokurtosisSF(beta, stockM2, stockM4, factorM2, factorM4)

  # MF path (general case, should give identical result)
  ku_mf <- cokurtosisMF(
    matrix(beta, ncol = 1), stockM2, stockM4, factorM2, factorM4
  )

  expect_equal(ku_sf, ku_mf, tolerance = 1e-12, check.attributes = FALSE)
})
