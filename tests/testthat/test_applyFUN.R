library(testthat)
library(PortfolioAnalytics)

context("applyFUN, scatterFUN, and objectiveFUN utilities")

data(edhec)
R4 <- edhec[1:60, 1:4]
colnames(R4) <- c("A", "B", "C", "D")
w_eq <- rep(0.25, 4)

# ============================================================================
# A. turnover()
# ============================================================================

test_that("turnover returns 0 when weights are unchanged", {
  expect_equal(turnover(w_eq, wts.init = w_eq), 0)
})

test_that("turnover computes correctly from equal-weight init", {
  w_new <- c(0.4, 0.3, 0.2, 0.1)
  # Default wts.init = rep(1/4, 4)
  expected <- sum(abs(w_eq - w_new)) / 4
  expect_equal(turnover(w_new), expected)
})

test_that("turnover uses custom wts.init", {
  w_new <- c(0.5, 0.3, 0.1, 0.1)
  w_init <- c(0.4, 0.2, 0.2, 0.2)
  expected <- sum(abs(w_init - w_new)) / 4
  expect_equal(turnover(w_new, wts.init = w_init), expected)
})

test_that("turnover errors on length mismatch", {
  expect_error(turnover(c(0.5, 0.5), wts.init = c(0.3, 0.3, 0.4)),
               "not the same length")
})

# ============================================================================
# B. var.portfolio()
# ============================================================================

test_that("var.portfolio returns scalar portfolio variance", {
  pvar <- var.portfolio(R4, w_eq)
  expect_length(pvar, 1)
  expect_true(is.numeric(pvar))
  expect_true(pvar > 0)
  # Cross-check with manual computation
  expected <- as.numeric(t(w_eq) %*% var(R4) %*% w_eq)
  expect_equal(pvar, expected)
})

# ============================================================================
# C. HHI()
# ============================================================================

test_that("HHI returns scalar for equal weights without groups", {
  hhi <- HHI(w_eq)
  # For equal weights: sum(0.25^2) = 4 * 0.0625 = 0.25
  expect_equal(hhi, 0.25)
})

test_that("HHI returns scalar for concentrated weights", {
  w_conc <- c(1, 0, 0, 0)
  expect_equal(HHI(w_conc), 1)
})

test_that("HHI returns list with group HHI when groups specified", {
  groups <- list(G1 = 1:2, G2 = 3:4)
  result <- HHI(w_eq, groups = groups)
  expect_true(is.list(result))
  expect_true("HHI" %in% names(result))
  expect_true("Groups_HHI" %in% names(result))
  expect_equal(result$HHI, 0.25)
  # Each group: sum(0.25^2, 0.25^2) = 0.125
  expect_equal(as.numeric(result$Groups_HHI), c(0.125, 0.125))
  expect_equal(names(result$Groups_HHI), c("G1", "G2"))
})

# ============================================================================
# D. port.mean()
# ============================================================================

test_that("port.mean computes weighted mean return", {
  mu <- matrix(colMeans(R4), ncol = 1)
  pm <- PortfolioAnalytics:::port.mean(w_eq, mu)
  expect_length(pm, 1)
  expect_equal(pm, as.numeric(crossprod(w_eq, mu)))
})

# ============================================================================
# E. applyFUN — single weights vector
# ============================================================================

test_that("applyFUN computes mean for single weight vector", {
  result <- applyFUN(R4, weights = w_eq, FUN = "mean", arguments = list())
  expect_length(result, 1)
  expect_true(is.numeric(result))
})

test_that("applyFUN computes StdDev for single weight vector", {
  result <- applyFUN(R4, weights = w_eq, FUN = "StdDev", arguments = list())
  expect_length(result, 1)
  expect_true(result > 0)
})

test_that("applyFUN computes ES for single weight vector", {
  result <- applyFUN(R4, weights = w_eq, FUN = "ES", arguments = list())
  expect_length(result, 1)
  expect_true(is.numeric(result))
})

# ============================================================================
# F. applyFUN — matrix of weights
# ============================================================================

test_that("applyFUN computes mean for matrix of weights", {
  set.seed(3478)
  w_mat <- matrix(runif(12), nrow = 3, ncol = 4)
  w_mat <- w_mat / rowSums(w_mat)
  result <- applyFUN(R4, weights = w_mat, FUN = "mean", arguments = list())
  expect_length(result, 3)
  expect_true(all(is.numeric(result)))
})

test_that("applyFUN computes StdDev for matrix of weights", {
  set.seed(6192)
  w_mat <- matrix(runif(8), nrow = 2, ncol = 4)
  w_mat <- w_mat / rowSums(w_mat)
  result <- applyFUN(R4, weights = w_mat, FUN = "StdDev", arguments = list())
  expect_length(result, 2)
  expect_true(all(result > 0))
})

# ============================================================================
# G. scatterFUN — per-asset metric
# ============================================================================

test_that("scatterFUN computes mean per asset", {
  result <- scatterFUN(R4, FUN = "mean")
  expect_length(result, 4)
  expect_equal(result, as.numeric(colMeans(R4)), tolerance = 1e-10)
})

test_that("scatterFUN computes var per asset", {
  result <- scatterFUN(R4, FUN = "var")
  expect_length(result, 4)
  expect_equal(result, as.numeric(apply(R4, 2, var)), tolerance = 1e-10)
})

test_that("scatterFUN computes StdDev per asset", {
  result <- scatterFUN(R4, FUN = "StdDev")
  expect_length(result, 4)
  expect_true(all(result > 0))
})

test_that("scatterFUN computes ES per asset", {
  result <- scatterFUN(R4, FUN = "ES")
  expect_length(result, 4)
  expect_true(all(is.numeric(result)))
})
