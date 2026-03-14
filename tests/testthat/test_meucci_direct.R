library(testthat)
library(PortfolioAnalytics)

context("Meucci Fully Flexible Views: moments and ranking")

skip_if_not_installed("nloptr")

data(edhec)
R4 <- edhec[1:60, 1:4]

# ============================================================================
# A. meucci.moments()
# ============================================================================

test_that("meucci.moments with uniform priors returns sample moments", {
  J <- nrow(R4)
  p_uniform <- rep(1/J, J)
  result <- meucci.moments(R4, p_uniform)

  expect_true(is.list(result))
  expect_true("mu" %in% names(result))
  expect_true("sigma" %in% names(result))

  # mu should be close to sample means
  expect_equal(as.numeric(result$mu), as.numeric(colMeans(R4)),
               tolerance = 1e-10)

  # sigma should be N x N and approximately symmetric positive semi-definite

  expect_equal(dim(result$sigma), c(4, 4))
  expect_true(isSymmetric(unname(result$sigma), tol = 1e-10))
  eigenvalues <- eigen(result$sigma, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-10))
})

test_that("meucci.moments with tilted priors shifts mu", {
  J <- nrow(R4)
  # Overweight the first half of observations
  p_tilted <- c(rep(2/J, J/2), rep(0, J/2))
  p_tilted <- p_tilted / sum(p_tilted)

  result <- meucci.moments(R4, p_tilted)

  # mu should equal means of first half only
  expected_mu <- as.numeric(colMeans(R4[1:(J/2), ]))
  expect_equal(as.numeric(result$mu), expected_mu, tolerance = 1e-10)
})

# ============================================================================
# B. meucci.ranking()
# ============================================================================

test_that("meucci.ranking returns mu and sigma", {
  J <- nrow(R4)
  p <- rep(1/J, J)
  # View: R[,2] < R[,3] < R[,1] < R[,4]
  result <- meucci.ranking(R4, p, order = c(2, 3, 1, 4))

  expect_true(is.list(result))
  expect_true("mu" %in% names(result))
  expect_true("sigma" %in% names(result))
  expect_equal(nrow(result$mu), 4)
  expect_equal(dim(result$sigma), c(4, 4))
})

test_that("meucci.ranking shifts posterior means toward the view", {
  J <- nrow(R4)
  p <- rep(1/J, J)
  # View: R[,2] < R[,3] < R[,1] < R[,4]
  result <- meucci.ranking(R4, p, order = c(2, 3, 1, 4))

  mu <- as.numeric(result$mu)
  # Asset 4 (ranked highest) should have higher posterior mu than asset 2 (ranked lowest)
  expect_true(mu[4] > mu[2])
})

test_that("meucci.ranking sigma is positive semi-definite", {
  J <- nrow(R4)
  p <- rep(1/J, J)
  result <- meucci.ranking(R4, p, order = c(3, 1, 4, 2))

  eigenvalues <- eigen(result$sigma, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-10))
})
