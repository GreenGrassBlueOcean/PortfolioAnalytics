library(testthat)
library(PortfolioAnalytics)

context("EntropyProg: Entropy Pooling probability reweighting")

skip_if_not_installed("nloptr")

# ============================================================================
# A. Equality constraints only (mean view)
# ============================================================================

test_that("EntropyProg tilts probabilities to match mean view (equality only)", {
  # 100 equally likely scenarios for a single asset
  set.seed(7392)
  J <- 100
  p <- rep(1/J, J)
  X <- rnorm(J, mean = 0, sd = 0.05)

  # View: mean return should be 0.02
  Aeq <- rbind(rep(1, J), X)
  beq <- c(1, 0.02)


  result <- EntropyProg(p = p, Aeq = Aeq, beq = beq)

  expect_true(is.list(result))
  expect_true("p_" %in% names(result))
  expect_equal(sum(result$p_), 1, tolerance = 1e-3)
  # Posterior mean should match the view
  expect_equal(as.numeric(t(result$p_) %*% X), 0.02, tolerance = 1e-4)
  # All probabilities should be non-negative
  expect_true(all(result$p_ >= -1e-10))
})

# ============================================================================
# B. Equality + inequality constraints
# ============================================================================

test_that("EntropyProg handles inequality constraints", {
  set.seed(5816)
  J <- 80
  p <- rep(1/J, J)
  X <- rnorm(J, mean = 0, sd = 0.04)

  # Equality: probabilities sum to 1
  Aeq <- matrix(rep(1, J), nrow = 1)
  beq <- 1

  # Inequality: mean >= 0.01 is A %*% p_ <= b  =>  -X' p_ <= -0.01
  A <- matrix(-X, nrow = 1)
  b <- matrix(-0.01, nrow = 1)

  result <- EntropyProg(p = p, A = A, b = b, Aeq = Aeq, beq = beq)

  expect_equal(sum(result$p_), 1, tolerance = 1e-3)
  posterior_mean <- as.numeric(t(result$p_) %*% X)
  expect_gte(posterior_mean, 0.01 - 1e-4)
})

# ============================================================================
# C. Input validation
# ============================================================================

test_that("EntropyProg rejects non-unit prior probabilities", {
  expect_error(
    EntropyProg(p = c(0.3, 0.3), Aeq = matrix(1, 1, 2), beq = 1),
    "sum of probabilities"
  )
})

test_that("EntropyProg rejects zero constraints", {
  expect_error(
    EntropyProg(p = c(0.5, 0.5), Aeq = matrix(nrow = 0, ncol = 2), beq = numeric(0)),
    "at least one"
  )
})

test_that("EntropyProg rejects mismatched constraint dimensions", {
  expect_error(
    EntropyProg(
      p = c(0.5, 0.5),
      Aeq = matrix(1, nrow = 1, ncol = 2),
      beq = c(1, 2)
    ),
    "number of .* constraints"
  )
})

# ============================================================================
# D. pHist (histogram helper)
# ============================================================================

test_that("pHist returns frequency and midpoints", {
  set.seed(2947)
  X <- rnorm(50)
  p <- rep(1/50, 50)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- PortfolioAnalytics:::pHist(X, p, nBins = 10)
  expect_true(is.list(result))
  expect_true("f" %in% names(result))
  expect_true("x" %in% names(result))
  expect_true(all(result$f >= 0))
})
