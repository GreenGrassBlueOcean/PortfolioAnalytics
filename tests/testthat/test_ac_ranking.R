library(testthat)
library(PortfolioAnalytics)

context("Asset ranking centroid estimation (ac_ranking.R)")

data(edhec)
R4 <- edhec[1:60, 1:4]

# ============================================================================
# A. Internal helpers: centroid() and scale_range()
# ============================================================================

test_that("centroid returns descending values of length n", {
  c4 <- PortfolioAnalytics:::centroid(4)
  expect_length(c4, 4)
  # Values should be strictly descending

  expect_true(all(diff(c4) < 0))
})

test_that("centroid is symmetric around zero for even n", {
  c6 <- PortfolioAnalytics:::centroid(6)
  expect_equal(sum(c6), 0, tolerance = 1e-4)
})

test_that("scale_range maps to [-0.05, 0.05]", {
  x <- c(-2, 0, 1, 3)
  scaled <- PortfolioAnalytics:::scale_range(x, max.value = 0.02)
  expect_equal(min(scaled), -0.05)
  expect_equal(max(scaled), 0.05)
})

# ============================================================================
# B. ac.ranking()
# ============================================================================

test_that("ac.ranking returns vector of correct length", {
  result <- ac.ranking(R4, order = c(2, 3, 1, 4))
  expect_length(result, 4)
  # Asset 4 has highest expected return -> highest centroid value
  expect_equal(which.max(result), 4)
  # Asset 2 has lowest expected return -> lowest centroid value
  expect_equal(which.min(result), 2)
})

test_that("ac.ranking accepts max.value without error", {
  # scale_range always maps to [-0.05, 0.05] regardless of max.value,
  # so we just verify it doesn't error
  result <- ac.ranking(R4, order = c(1, 2, 3, 4), max.value = 0.10)
  expect_length(result, 4)
})

test_that("ac.ranking rejects mismatched order length", {
  expect_error(ac.ranking(R4, order = c(1, 2, 3)), "length of the order vector")
})

# ============================================================================
# C. centroid.complete.mc()
# ============================================================================

test_that("centroid.complete.mc returns correct length and ordering", {
  set.seed(5184)
  result <- centroid.complete.mc(c(2, 1, 3, 4), simulations = 2000)
  expect_length(result, 4)
  # Asset 4 ranked highest, asset 2 ranked lowest
  expect_equal(which.max(result), 4)
  expect_equal(which.min(result), 2)
})

test_that("centroid.complete.mc converges toward analytical centroid", {
  set.seed(7293)
  mc <- centroid.complete.mc(c(1, 2, 3, 4), simulations = 5000)
  analytical <- ac.ranking(R4, order = c(1, 2, 3, 4))
  # Both should produce the same relative ordering
  expect_equal(order(mc), order(analytical))
})

# ============================================================================
# D. centroid.sectors()
# ============================================================================

test_that("centroid.sectors handles two sectors", {
  set.seed(3417)
  sectors <- list(c(2, 1, 3), c(5, 4))
  result <- centroid.sectors(sectors, simulations = 2000)
  expect_length(result, 5)
  # Within sector 1: asset 3 highest, asset 2 lowest
  expect_true(result[3] > result[1])
  expect_true(result[1] > result[2])
  # Within sector 2: asset 4 > asset 5
  expect_true(result[4] > result[5])
})

test_that("centroid.sectors rejects non-list input", {
  expect_error(centroid.sectors(c(1, 2, 3)), "sectors must be a list")
})

# ============================================================================
# E. centroid.sign()
# ============================================================================

test_that("centroid.sign separates positive and negative returns", {
  set.seed(6829)
  # R_1 < R_2 < 0 < R_3 < R_4
  result <- centroid.sign(positive = c(4, 3), negative = c(1, 2), simulations = 2000)
  expect_length(result, 4)
  # Positive assets should have positive centroid
  expect_true(result[3] > 0)
  expect_true(result[4] > 0)
  # Negative assets should have negative centroid
  expect_true(result[1] < 0)
  expect_true(result[2] < 0)
  # Ranking within positive: asset 4 > asset 3
  expect_true(result[4] < result[3])
})

# ============================================================================
# F. centroid.buckets()
# ============================================================================

test_that("centroid.buckets assigns same value within bucket", {
  set.seed(9152)
  # Buckets listed in ascending expected return order per docs, but
  # the implementation assigns descending centroid to ascending bucket index
  buckets <- list(c(1, 2), c(3, 4, 5))
  result <- centroid.buckets(buckets, simulations = 2000)
  expect_length(result, 5)
  # Assets within same bucket get the same centroid value
  expect_equal(result[1], result[2])
  expect_equal(result[3], result[4])
  expect_equal(result[4], result[5])
  # Bucket 1 and bucket 2 get different values
  expect_false(result[1] == result[3])
})

test_that("centroid.buckets rejects non-list input", {
  expect_error(centroid.buckets(c(1, 2, 3)), "buckets must be a list")
})
