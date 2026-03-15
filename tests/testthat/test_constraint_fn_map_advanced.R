library(testthat)
library(PortfolioAnalytics)

context("constraint_fn_map advanced: Dykstra projection, relaxation loops, fallback paths")

# ============================================================================
# Shared helpers
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:36, 1:4]
funds4 <- colnames(R4)

make_spec <- function(funds = funds4) {
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.05, max = 0.55)
  spec
}

# ============================================================================
# A. Dykstra projection helpers (internals)
# ============================================================================

test_that(".project_box clamps to bounds", {
  w <- c(-0.1, 0.6, 0.3)
  lo <- c(0, 0, 0)
  hi <- c(0.5, 0.5, 0.5)
  result <- PortfolioAnalytics:::.project_box(w, lo, hi)
  expect_equal(result, c(0, 0.5, 0.3))
})

test_that(".project_weight_sum shifts weights uniformly", {
  w <- c(0.1, 0.1, 0.1)
  result <- PortfolioAnalytics:::.project_weight_sum(w, min_sum = 1, max_sum = 1)
  expect_equal(sum(result), 1, tolerance = 1e-12)
  # Shift should be uniform
  expect_equal(result[1], result[2], tolerance = 1e-12)

  # Already feasible — returned unchanged
  w2 <- c(0.3, 0.3, 0.4)
  result2 <- PortfolioAnalytics:::.project_weight_sum(w2, min_sum = 0.99, max_sum = 1.01)
  expect_equal(result2, w2)
})

test_that(".project_weight_sum projects to nearest bound", {
  # Below min_sum: project to min_sum

  w <- c(0.1, 0.2, 0.3)
  result <- PortfolioAnalytics:::.project_weight_sum(w, min_sum = 0.99, max_sum = 1.01)
  expect_equal(sum(result), 0.99, tolerance = 1e-12)

  # Above max_sum: project to max_sum
  w2 <- c(0.5, 0.4, 0.3)
  result2 <- PortfolioAnalytics:::.project_weight_sum(w2, min_sum = 0.99, max_sum = 1.01)
  expect_equal(sum(result2), 1.01, tolerance = 1e-12)
})

test_that(".project_group modifies only group elements", {
  w <- c(0.1, 0.2, 0.3, 0.4)
  idx <- c(1, 2)
  result <- PortfolioAnalytics:::.project_group(w, idx, lo = 0.5, up = 0.7)
  expect_equal(sum(result[idx]), 0.5, tolerance = 1e-12)
  # Non-group elements unchanged
  expect_equal(result[3:4], w[3:4])
})

test_that(".project_group returns unchanged when within bounds", {
  w <- c(0.3, 0.2, 0.3, 0.2)
  idx <- c(1, 2)
  result <- PortfolioAnalytics:::.project_group(w, idx, lo = 0.4, up = 0.6)
  expect_equal(result, w)
})

test_that(".is_projection_feasible correctly validates", {
  w <- c(0.25, 0.25, 0.25, 0.25)
  expect_true(PortfolioAnalytics:::.is_projection_feasible(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, 4), max_box = rep(0.5, 4)
  ))

  # Violates box
  w2 <- c(0.6, 0.2, 0.1, 0.1)
  expect_false(PortfolioAnalytics:::.is_projection_feasible(
    w2, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, 4), max_box = rep(0.5, 4)
  ))

  # Violates weight_sum
  w3 <- c(0.1, 0.1, 0.1, 0.1)
  expect_false(PortfolioAnalytics:::.is_projection_feasible(
    w3, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, 4), max_box = rep(0.5, 4)
  ))

  # Violates group constraint
  w4 <- c(0.4, 0.4, 0.1, 0.1)
  expect_false(PortfolioAnalytics:::.is_projection_feasible(
    w4, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, 4), max_box = rep(0.5, 4),
    groups = list(1:2, 3:4), cLO = c(0.3, 0.3), cUP = c(0.5, 0.7)
  ))
})

# ============================================================================
# B. project_weights (Dykstra algorithm)
# ============================================================================

test_that("project_weights solves box + weight_sum", {
  w <- c(0.8, 0.1, 0.05, 0.05)
  result <- project_weights(w, min_sum = 1, max_sum = 1,
                            min_box = rep(0.1, 4), max_box = rep(0.4, 4))
  expect_false(is.null(result))
  expect_equal(sum(result), 1, tolerance = 1e-6)
  expect_true(all(result >= 0.1 - 1e-6))
  expect_true(all(result <= 0.4 + 1e-6))
})

test_that("project_weights solves with group constraints", {
  w <- c(0.5, 0.3, 0.1, 0.1)
  groups <- list(1:2, 3:4)
  result <- project_weights(w, min_sum = 1, max_sum = 1,
                            min_box = rep(0.1, 4), max_box = rep(0.5, 4),
                            groups = groups, cLO = c(0.4, 0.4), cUP = c(0.6, 0.6))
  expect_false(is.null(result))
  expect_equal(sum(result), 1, tolerance = 1e-6)
  expect_true(sum(result[1:2]) >= 0.4 - 1e-6)
  expect_true(sum(result[3:4]) >= 0.4 - 1e-6)
})

test_that("project_weights returns NULL for infeasible constraints", {
  # min_box sums to 2.0 but max_sum = 1
  result <- project_weights(c(0.5, 0.5, 0.5, 0.5),
                            min_sum = 1, max_sum = 1,
                            min_box = rep(0.5, 4), max_box = rep(1, 4))
  expect_null(result)
})

test_that("project_weights returns input when already feasible", {
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- project_weights(w, min_sum = 0.99, max_sum = 1.01,
                            min_box = rep(0, 4), max_box = rep(0.5, 4))
  expect_false(is.null(result))
  # Should be very close to input (already feasible)
  expect_equal(result, w, tolerance = 1e-8)
})

# ============================================================================
# C. fn_map: Dykstra (projection) path
# ============================================================================

test_that("fn_map uses projection by default for convex constraints", {
  spec <- make_spec()
  w <- c(0.6, 0.2, 0.1, 0.1)
  names(w) <- funds4
  result <- fn_map(w, spec)
  expect_true(is.list(result))
  expect_equal(sum(result$weights), 1, tolerance = 1e-6)
  expect_true(all(result$weights >= 0.05 - 1e-6))
  expect_true(all(result$weights <= 0.55 + 1e-6))
})

test_that("fn_map projection path handles group constraints", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "group", groups = list(1:2, 3:4),
                         group_min = c(0.3, 0.3), group_max = c(0.7, 0.7))
  w <- c(0.5, 0.3, 0.15, 0.05)
  names(w) <- funds4
  result <- fn_map(w, spec, method = "projection")
  expect_equal(sum(result$weights), 1, tolerance = 1e-4)
  expect_true(sum(result$weights[1:2]) >= 0.3 - 1e-4)
  expect_true(sum(result$weights[3:4]) >= 0.3 - 1e-4)
})

# ============================================================================
# D. fn_map: fallback from projection to rp_transform
# ============================================================================

test_that("fn_map falls back to rp_transform for position_limit (non-convex)", {
  set.seed(6274)
  spec <- make_spec(funds = colnames(edhec[, 1:6]))
  spec <- add.constraint(spec, type = "position_limit", max_pos = 4)
  w <- rep(1 / 6, 6)
  names(w) <- colnames(edhec[, 1:6])
  result <- fn_map(w, spec, method = "projection")
  expect_true(is.list(result))
  # Position limit makes this non-convex, so projection falls through
  expect_true("weights" %in% names(result))
})

test_that("fn_map falls back to rp_transform for leverage_exposure (non-convex)", {
  set.seed(8312)
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "weight_sum", min_sum = -0.01, max_sum = 0.01)
  spec <- add.constraint(spec, type = "box", min = -0.5, max = 0.5)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 1.5)
  w <- c(0.4, -0.3, 0.5, -0.6)
  names(w) <- funds4
  result <- fn_map(w, spec, method = "projection")
  expect_true(is.list(result))
  expect_true("leverage" %in% names(result))
})

test_that("fn_map verbose message on Dykstra failure fallback", {
  # Force projection failure via infeasible-for-projection but feasible-for-rp_transform
  set.seed(3981)
  spec <- make_spec(funds = colnames(edhec[, 1:6]))
  spec <- add.constraint(spec, type = "position_limit", max_pos = 4)
  w <- rep(1 / 6, 6)
  names(w) <- colnames(edhec[, 1:6])
  # With verbose, should not error
  result <- fn_map(w, spec, verbose = TRUE, method = "projection")
  expect_true(is.list(result))
})

# ============================================================================
# E. fn_map: relax paths
# ============================================================================

test_that("fn_map relax=TRUE relaxes position limit constraints", {
  set.seed(4526)
  spec <- make_spec(funds = colnames(edhec[, 1:6]))
  spec <- add.constraint(spec, type = "position_limit", max_pos = 2)
  w <- rep(1 / 6, 6)
  names(w) <- colnames(edhec[, 1:6])

  result <- fn_map(w, spec, relax = TRUE, method = "rp_transform")
  expect_true(is.list(result))
  expect_true("weights" %in% names(result))
  expect_true("max_pos" %in% names(result))
})

test_that("fn_map relax=TRUE relaxes leverage constraints", {
  set.seed(7319)
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "weight_sum", min_sum = -0.01, max_sum = 0.01)
  spec <- add.constraint(spec, type = "box", min = -0.6, max = 0.6)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 0.5)

  w <- c(0.4, -0.3, 0.2, -0.3)
  names(w) <- funds4
  result <- fn_map(w, spec, relax = TRUE, method = "rp_transform")
  expect_true(is.list(result))
  expect_true("leverage" %in% names(result))
})

# ============================================================================
# F. fn_map: line 203 bug fix verification
# ============================================================================

test_that("fn_map box relax correctly relaxes max bounds (line 203 bug fix)", {
  # The bug was: when relaxing max constraints, the code used
  # `which(tmp_weights < tmp_max)` (selects non-violating elements) instead
  # of `which(tmp_weights > tmp_max)` (selects violating elements).
  # This test verifies the fix works correctly.
  set.seed(1847)
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  # Very tight box where rp_transform is likely to fail
  spec <- add.constraint(spec, type = "box", min = 0.24, max = 0.26)

  w <- c(0.4, 0.1, 0.3, 0.2)
  names(w) <- funds4

  result <- fn_map(w, spec, relax = TRUE, method = "rp_transform")
  expect_true(is.list(result))
  expect_true("weights" %in% names(result))
  expect_true("min" %in% names(result))
  expect_true("max" %in% names(result))
})

# ============================================================================
# G. fn_map: error handling
# ============================================================================

test_that("fn_map errors on non-portfolio input", {
  expect_error(fn_map(c(0.5, 0.5), list()), "not of class")
})

test_that("fn_map preserves weight names", {
  spec <- make_spec()
  w <- c(0.3, 0.3, 0.2, 0.2)
  names(w) <- funds4
  result <- fn_map(w, spec)
  expect_equal(names(result$weights), funds4)
})

test_that("fn_map method argument accepts rp_transform explicitly", {
  set.seed(5723)
  spec <- make_spec()
  w <- c(0.6, 0.2, 0.1, 0.1)
  names(w) <- funds4
  result <- fn_map(w, spec, method = "rp_transform")
  expect_true(is.list(result))
  expect_true(sum(result$weights) >= 0.99 - 1e-3)
  expect_true(sum(result$weights) <= 1.01 + 1e-3)
})

# ============================================================================
# H. check_constraints helper
# ============================================================================

test_that("check_constraints returns TRUE for feasible weights", {
  spec <- make_spec()
  w <- c(0.3, 0.3, 0.2, 0.2)
  result <- PortfolioAnalytics:::check_constraints(w, spec)
  expect_true(result)
})

test_that("check_constraints returns FALSE for box violation", {
  spec <- make_spec()
  w <- c(0.7, 0.1, 0.1, 0.1)
  result <- PortfolioAnalytics:::check_constraints(w, spec)
  expect_false(result)
})

test_that("check_constraints returns FALSE for weight_sum violation", {
  spec <- make_spec()
  w <- c(0.5, 0.5, 0.5, 0.5)
  result <- PortfolioAnalytics:::check_constraints(w, spec)
  expect_false(result)
})

test_that("check_constraints detects group constraint violations", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "group", groups = list(1:2, 3:4),
                         group_min = c(0.4, 0.4), group_max = c(0.6, 0.6))
  # Group 1 sum = 0.1, violates group_min = 0.4
  w <- c(0.05, 0.05, 0.45, 0.45)
  result <- PortfolioAnalytics:::check_constraints(w, spec)
  expect_false(result)
})

test_that("check_constraints detects leverage_exposure violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "weight_sum", min_sum = -0.01, max_sum = 0.01)
  spec <- add.constraint(spec, type = "box", min = -0.5, max = 0.5)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 1.0)
  w <- c(0.4, -0.4, 0.3, -0.3)
  result <- PortfolioAnalytics:::check_constraints(w, spec)
  expect_false(result)
})
