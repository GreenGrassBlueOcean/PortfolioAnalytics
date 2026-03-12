
require(testthat)
require(PortfolioAnalytics)

context("test CRRA objective")

data(edhec)

ret <- edhec[, 1:4]

test_that("crra.moments returns correctly structured output", {
  test_Moments <- crra.moments(ret)

  # mu: named numeric vector of length 4
  expect_true(is.numeric(test_Moments$mu))
  expect_equal(length(test_Moments$mu), 4)
  expect_equal(names(test_Moments$mu), colnames(ret))

  # sigma: 4x4 symmetric positive-definite matrix
  expect_true(is.matrix(test_Moments$sigma))
  expect_equal(dim(test_Moments$sigma), c(4, 4))
  expect_equal(test_Moments$sigma, t(test_Moments$sigma), tolerance = 1e-12)
  expect_true(all(eigen(test_Moments$sigma, only.values = TRUE)$values > 0))

  # m3: 4 x 16 coskewness matrix
  expect_true(is.matrix(test_Moments$m3) || is.numeric(test_Moments$m3))
  expect_equal(dim(test_Moments$m3), c(4, 16))

  # m4: 4 x 64 cokurtosis matrix
  expect_true(is.matrix(test_Moments$m4) || is.numeric(test_Moments$m4))
  expect_equal(dim(test_Moments$m4), c(4, 64))
})

test_that("crra objective function returns a scalar matrix", {
  Moments <- crra.moments(ret)
  Equalweights <- rep(1 / ncol(ret), ncol(ret))
  CRRA_test <- CRRA(R = ret, weights = Equalweights, lambda = 5,
                    sigma = Moments$sigma, m3 = Moments$m3, m4 = Moments$m4)

  expect_true(is.matrix(CRRA_test))
  expect_equal(dim(CRRA_test), c(1, 1))
  # CRRA utility should be negative for typical portfolio returns with lambda > 1
  expect_true(CRRA_test[1, 1] < 0)
})

test_that("crra.RobustMoments function returns valid output", {
  Moments2 <- crra.RobustMoments(R = ret, k = 3)
  Equalweights <- rep(1 / ncol(ret), ncol(ret))
  CRRA_test <- CRRA(R = ret, weights = Equalweights, lambda = 5,
                    sigma = Moments2$sigma, m3 = Moments2$m3, m4 = Moments2$m4)

  expect_true(is.matrix(CRRA_test))
  expect_equal(dim(CRRA_test), c(1, 1))
  expect_true(CRRA_test[1, 1] < 0)
})

test_that("CRRA returns different values for different value of Lambda", {
  data("edhec")
  Moments <- crra.moments(edhec[, 1:4])
  Equalweights <- rep(1 / ncol(edhec[, 1:4]), ncol(edhec[, 1:4]))
  Lambda5 <- CRRA(R = edhec, weights = Equalweights, lambda = 5,
                  sigma = Moments$sigma, m3 = Moments$m3, m4 = Moments$m4)
  Lambda10 <- CRRA(R = edhec, weights = Equalweights, lambda = 10,
                   sigma = Moments$sigma, m3 = Moments$m3, m4 = Moments$m4)

  # Both should be negative scalars
  expect_true(Lambda5[1, 1] < 0)
  expect_true(Lambda10[1, 1] < 0)
  # Higher lambda = more risk aversion = lower (more negative) utility
  expect_true(Lambda10[1, 1] < Lambda5[1, 1])
})
