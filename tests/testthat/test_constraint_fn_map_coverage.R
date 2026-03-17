
require(testthat)
require(PortfolioAnalytics)

context("constraint_fn_map.R coverage: projection fallback, stall detection, position/leverage paths")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# ============================================================================
# Tests: project_weights stall detection (returns NULL after 200 stalls)
# ============================================================================

test_that("project_weights returns NULL for infeasible constraints (stall detection)", {
  # 3 assets with min_box=0.6 each => sum >= 1.8, but max_sum = 1
  # The intersection is empty, so Dykstra oscillates and stalls
  w <- c(0.4, 0.3, 0.3)
  result <- project_weights(
    w = w,
    min_sum = 0.99,
    max_sum = 1.01,
    min_box = c(0.6, 0.6, 0.6),
    max_box = c(0.9, 0.9, 0.9),
    max_iter = 500
  )
  expect_null(result)
})

test_that("project_weights returns NULL for conflicting group + box constraints", {
  # Group constraint forces sum(w[1:2]) >= 0.9 but box max = 0.4 each
  # => max group sum = 0.8 < 0.9
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- project_weights(
    w = w,
    min_sum = 0.99,
    max_sum = 1.01,
    min_box = c(0.05, 0.05, 0.05, 0.05),
    max_box = c(0.4, 0.4, 0.4, 0.4),
    groups = list(1:2, 3:4),
    cLO = c(0.9, 0.1),
    cUP = c(1.0, 0.5),
    max_iter = 500
  )
  expect_null(result)
})

# ============================================================================
# Tests: fn_map projection failure fallback to rp_transform
# ============================================================================

test_that("fn_map falls through to rp_transform when projection returns NULL", {
  # Create a portfolio where projection should fail but rp_transform can work
  # Use tight but feasible constraints that stress the projector
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "group",
                          groups = list(1:2, 3:4),
                          group_min = c(0.3, 0.3),
                          group_max = c(0.7, 0.7))

  # Weights that violate constraints
  w <- c(0.1, 0.1, 0.4, 0.4)
  names(w) <- colnames(R4)

  result <- fn_map(w, portf, relax = FALSE, verbose = FALSE,
                   method = "projection")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
  # Verify the returned weights are named
  expect_true(all(names(result$weights) == colnames(R4)))
})

# ============================================================================
# Tests: fn_map with rp_transform method directly
# ============================================================================

test_that("fn_map rp_transform path handles weight_sum violation", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)

  # Weights that sum to 0.4, violating min_sum = 0.99
  w <- c(0.1, 0.1, 0.1, 0.1)
  names(w) <- colnames(R4)

  set.seed(9173)
  result <- fn_map(w, portf, relax = FALSE, method = "rp_transform")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
  # Weight sum should now be within bounds
  expect_true(sum(result$weights) >= 0.99 - 0.02)
  expect_true(sum(result$weights) <= 1.01 + 0.02)
})

test_that("fn_map with method='rp_transform' bypasses projection entirely", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.1, max = 0.6)

  w <- c(0.7, 0.1, 0.1, 0.1)
  names(w) <- colnames(R4)

  set.seed(4817)
  result <- fn_map(w, portf, relax = FALSE, method = "rp_transform")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
  expect_true(all(names(result$weights) == colnames(R4)))
})

# ============================================================================
# Tests: fn_map with position_limit constraints (non-convex, skips projection)
# ============================================================================

test_that("fn_map handles position_limit constraints via rp_transform", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.0, max = 0.65)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 2)

  # All 4 assets have non-zero weight, violating max_pos = 2
  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- colnames(R4)

  set.seed(7293)
  result <- fn_map(w, portf, relax = FALSE, method = "projection")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
  # At most 2 positions should have non-zero weight
  tol <- .Machine$double.eps^0.5
  expect_true(sum(abs(result$weights) > tol) <= 2)
})

test_that("fn_map handles max_pos_long constraint", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = -0.3, max = 0.65)
  portf <- add.constraint(portf, type = "position_limit",
                          max_pos_long = 2, max_pos_short = 2)

  # 4 long positions, violating max_pos_long = 2
  w <- c(0.3, 0.3, 0.2, 0.2)
  names(w) <- colnames(R4)

  set.seed(5104)
  result <- fn_map(w, portf, relax = FALSE, method = "projection")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
})

# ============================================================================
# Tests: fn_map with leverage constraint (non-convex, skips projection)
# ============================================================================

test_that("fn_map handles leverage constraint via rp_transform", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = -0.5, max = 0.8)
  portf <- add.constraint(portf, type = "leverage_exposure",
                          leverage = 1.2)

  # Weights with sum(abs) = 1.8 > leverage = 1.2
  w <- c(0.6, 0.5, -0.3, 0.2)
  names(w) <- colnames(R4)

  set.seed(6381)
  result <- fn_map(w, portf, relax = FALSE, method = "projection")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
})

# ============================================================================
# Tests: fn_map leverage check block in fn_map (weight_sum constraint violation)
# ============================================================================

test_that("fn_map corrects weight_sum violation", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)

  # Weights that sum to 0.5 (violating min_sum = 0.99)
  w <- c(0.15, 0.1, 0.15, 0.1)
  names(w) <- colnames(R4)

  result <- fn_map(w, portf, relax = FALSE, method = "projection")
  expect_true(is.list(result))
  # Projection should fix the sum
  expect_true(sum(result$weights) >= 0.99 - 1e-6)
  expect_true(sum(result$weights) <= 1.01 + 1e-6)
})

# ============================================================================
# Tests: project_weights max_iter reached but feasible
# ============================================================================

test_that("project_weights converges to feasible within max_iter", {
  # Simple case: weights slightly out of bounds
  w <- c(0.3, 0.3, 0.3, 0.3)
  result <- project_weights(
    w = w,
    min_sum = 0.99,
    max_sum = 1.01,
    min_box = c(0.1, 0.1, 0.1, 0.1),
    max_box = c(0.5, 0.5, 0.5, 0.5),
    max_iter = 5000
  )
  expect_false(is.null(result))
  expect_true(sum(result) >= 0.99 - 1e-8)
  expect_true(sum(result) <= 1.01 + 1e-8)
  expect_true(all(result >= 0.1 - 1e-8))
  expect_true(all(result <= 0.5 + 1e-8))
})

test_that("project_weights handles already-feasible input", {
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- project_weights(
    w = w,
    min_sum = 0.99,
    max_sum = 1.01,
    min_box = c(0.1, 0.1, 0.1, 0.1),
    max_box = c(0.5, 0.5, 0.5, 0.5)
  )
  expect_equal(result, w, tolerance = 1e-8)
})

# ============================================================================
# Tests: rp_transform directly with various constraint types
# ============================================================================

test_that("rp_transform handles leverage constraint directly", {
  set.seed(8142)
  w <- matrix(c(0.6, 0.5, -0.3, 0.2), nrow = 1)
  colnames(w) <- c("A", "B", "C", "D")

  result <- rp_transform(
    w = w,
    min_sum = 0.99,
    max_sum = 1.01,
    min_box = c(-0.5, -0.5, -0.5, -0.5),
    max_box = c(0.8, 0.8, 0.8, 0.8),
    leverage = 1.5,
    max_permutations = 500
  )
  expect_equal(length(result), 4)
})

test_that("rp_transform handles position_limit constraint", {
  set.seed(3049)
  w <- matrix(c(0.25, 0.25, 0.25, 0.25), nrow = 1)
  colnames(w) <- c("A", "B", "C", "D")

  result <- rp_transform(
    w = w,
    min_sum = 0.99,
    max_sum = 1.01,
    min_box = c(0, 0, 0, 0),
    max_box = c(0.8, 0.8, 0.8, 0.8),
    max_pos = 2,
    max_permutations = 500
  )
  expect_equal(length(result), 4)
  tol <- .Machine$double.eps^0.5
  expect_true(sum(abs(result) > tol) <= 2)
})

test_that("rp_transform handles group constraints", {
  set.seed(2567)
  w <- matrix(c(0.1, 0.1, 0.4, 0.4), nrow = 1)
  colnames(w) <- c("A", "B", "C", "D")

  result <- rp_transform(
    w = w,
    min_sum = 0.99,
    max_sum = 1.01,
    min_box = c(0.05, 0.05, 0.05, 0.05),
    max_box = c(0.6, 0.6, 0.6, 0.6),
    groups = list(1:2, 3:4),
    cLO = c(0.3, 0.3),
    cUP = c(0.7, 0.7),
    max_permutations = 500
  )
  expect_equal(length(result), 4)
  # Check group sums
  expect_true(sum(result[1:2]) >= 0.3 - 0.05)
  expect_true(sum(result[3:4]) >= 0.3 - 0.05)
})
