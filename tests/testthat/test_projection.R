library(testthat)
library(PortfolioAnalytics)

context("Proposal #14: Deterministic Constraint Repair (Dykstra's Projection)")

# Helper: check all constraints satisfied within tolerance
check_feasible <- function(w, min_sum, max_sum, min_box, max_box,
                           groups = NULL, cLO = NULL, cUP = NULL,
                           tol = 1e-8) {
  expect_true(sum(w) >= min_sum - tol, label = "min_sum satisfied")
  expect_true(sum(w) <= max_sum + tol, label = "max_sum satisfied")
  expect_true(all(w >= min_box - tol), label = "min_box satisfied")
  expect_true(all(w <= max_box + tol), label = "max_box satisfied")
  if (!is.null(groups)) {
    for (g in seq_along(groups)) {
      gs <- sum(w[groups[[g]]])
      expect_true(gs >= cLO[g] - tol, label = paste("group", g, "lower satisfied"))
      expect_true(gs <= cUP[g] + tol, label = paste("group", g, "upper satisfied"))
    }
  }
}

# ============================================================================
# A. Low-level projection operators
# ============================================================================

test_that(".project_box clamps correctly", {
  w <- c(-0.1, 0.5, 0.8, 0.3)
  result <- PortfolioAnalytics:::.project_box(w, min_box = rep(0, 4), max_box = rep(0.6, 4))
  expect_equal(result, c(0, 0.5, 0.6, 0.3))
})

test_that(".project_weight_sum shifts to nearest boundary", {
  # Below min_sum
  w <- c(0.1, 0.1, 0.1)
  result <- PortfolioAnalytics:::.project_weight_sum(w, min_sum = 0.99, max_sum = 1.01)
  expect_equal(sum(result), 0.99, tolerance = 1e-12)

  # Above max_sum
  w <- c(0.5, 0.4, 0.3)
  result <- PortfolioAnalytics:::.project_weight_sum(w, min_sum = 0.99, max_sum = 1.01)
  expect_equal(sum(result), 1.01, tolerance = 1e-12)

  # Already feasible
  w <- c(0.33, 0.33, 0.34)
  result <- PortfolioAnalytics:::.project_weight_sum(w, min_sum = 0.99, max_sum = 1.01)
  expect_equal(result, w)
})

test_that(".project_group adjusts single group", {
  w <- c(0.1, 0.1, 0.5, 0.3)
  groups <- list(1:2, 3:4)
  # Group 1 sum = 0.2, below cLO = 0.3
  result <- PortfolioAnalytics:::.project_group(w, idx = 1:2, lo = 0.3, up = 0.5)
  expect_equal(sum(result[1:2]), 0.3, tolerance = 1e-12)
  # Group 2 unchanged
  expect_equal(result[3:4], w[3:4])
})

# ============================================================================
# B. project_weights — box only
# ============================================================================

test_that("project_weights handles box-only constraints", {
  w <- c(-0.1, 0.6, 0.3, 0.2)
  result <- project_weights(
    w, min_sum = -Inf, max_sum = Inf,
    min_box = rep(0, 4), max_box = rep(0.5, 4)
  )
  expect_true(all(result >= 0 - 1e-10))
  expect_true(all(result <= 0.5 + 1e-10))
})

# ============================================================================
# C. project_weights — weight_sum only
# ============================================================================

test_that("project_weights handles weight_sum only", {
  w <- c(0.1, 0.1, 0.1, 0.1)
  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(-Inf, 4), max_box = rep(Inf, 4)
  )
  expect_equal(sum(result), 0.99, tolerance = 1e-10)
})

# ============================================================================
# D. project_weights — box + weight_sum
# ============================================================================

test_that("project_weights handles box + weight_sum (Dykstra convergence)", {
  # Start far from feasible
  w <- c(0.8, 0.6, 0.5, 0.3)  # sum = 2.2, box max = 0.4
  min_box <- rep(0.05, 4)
  max_box <- rep(0.40, 4)
  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box
  )
  expect_false(is.null(result))
  check_feasible(result, 0.99, 1.01, min_box, max_box)
})

test_that("project_weights returns nearest feasible point (distance property)", {
  w <- c(0.8, 0.6, 0.5, 0.3)
  min_box <- rep(0.05, 4)
  max_box <- rep(0.40, 4)
  proj <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box
  )
  dist_proj <- sqrt(sum((w - proj)^2))

  # Generate many random feasible points and verify none are closer
  set.seed(4817)
  for (i in 1:200) {
    rand_w <- runif(4, min_box, max_box)
    rand_w <- rand_w / sum(rand_w)  # normalize
    # Only check if this is truly feasible
    if (sum(rand_w) >= 0.99 && sum(rand_w) <= 1.01 &&
        all(rand_w >= 0.05) && all(rand_w <= 0.40)) {
      dist_rand <- sqrt(sum((w - rand_w)^2))
      expect_true(dist_proj <= dist_rand + 1e-6,
                  label = "projection is nearer than random feasible point")
    }
  }
})

# ============================================================================
# E. project_weights — box + weight_sum + group
# ============================================================================

test_that("project_weights handles box + weight_sum + group", {
  w <- c(0.5, 0.5, 0.1, 0.1)  # sum = 1.2
  min_box <- rep(0.05, 4)
  max_box <- rep(0.50, 4)
  groups <- list(1:2, 3:4)
  cLO <- c(0.3, 0.3)
  cUP <- c(0.6, 0.6)

  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box,
    groups = groups, cLO = cLO, cUP = cUP
  )
  expect_false(is.null(result))
  check_feasible(result, 0.99, 1.01, min_box, max_box, groups, cLO, cUP)
})

test_that("project_weights handles overlapping groups", {
  w <- c(0.1, 0.1, 0.1, 0.1, 0.1)  # sum = 0.5
  min_box <- rep(0, 5)
  max_box <- rep(0.5, 5)
  # Asset 2 belongs to both groups
  groups <- list(c(1, 2), c(2, 3, 4), c(5))
  cLO <- c(0.2, 0.3, 0.1)
  cUP <- c(0.5, 0.6, 0.3)

  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box,
    groups = groups, cLO = cLO, cUP = cUP
  )
  expect_false(is.null(result))
  check_feasible(result, 0.99, 1.01, min_box, max_box, groups, cLO, cUP)
})

# ============================================================================
# F. Identity property (already feasible)
# ============================================================================

test_that("project_weights returns identical weights when already feasible", {
  w <- c(0.25, 0.25, 0.25, 0.25)
  min_box <- rep(0, 4)
  max_box <- rep(0.5, 4)
  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box
  )
  expect_equal(result, w, tolerance = 1e-10)
})

test_that("project_weights preserves feasibility with groups", {
  w <- c(0.3, 0.2, 0.3, 0.2)
  min_box <- rep(0.1, 4)
  max_box <- rep(0.4, 4)
  groups <- list(1:2, 3:4)
  cLO <- c(0.3, 0.3)
  cUP <- c(0.7, 0.7)
  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box,
    groups = groups, cLO = cLO, cUP = cUP
  )
  expect_equal(result, w, tolerance = 1e-10)
})

# ============================================================================
# G. Convergence and empty intersection
# ============================================================================

test_that("project_weights converges within iteration limit", {
  # Large problem: 20 assets
  set.seed(7293)
  n <- 20
  w <- runif(n, -0.1, 0.6)
  min_box <- rep(0, n)
  max_box <- rep(0.15, n)
  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box
  )
  expect_false(is.null(result))
  check_feasible(result, 0.99, 1.01, min_box, max_box)
})

test_that("project_weights returns NULL for empty intersection", {
  # min_box sums to 2.0 but max_sum = 1.01 — impossible
  result <- project_weights(
    w = rep(0.5, 4),
    min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.5, 4), max_box = rep(1, 4),
    max_iter = 100
  )
  expect_null(result)
})

# ============================================================================
# H. fn_map integration
# ============================================================================

test_that("fn_map uses projection by default for convex problems", {
  data(edhec)
  R <- edhec[, 1:4]
  funds <- colnames(R)
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.05, max = 0.65)

  w <- c(0.8, 0.1, 0.05, 0.05)
  names(w) <- funds
  result <- fn_map(w, spec)
  expect_true(sum(result$weights) >= 0.99 - 1e-8)
  expect_true(sum(result$weights) <= 1.01 + 1e-8)
  expect_true(all(result$weights >= 0.05 - 1e-8))
  expect_true(all(result$weights <= 0.65 + 1e-8))
})

test_that("fn_map falls back to rp_transform for non-convex (position limit)", {
  data(edhec)
  R <- edhec[, 1:4]
  funds <- colnames(R)
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.65)
  spec <- add.constraint(spec, type = "position_limit", max_pos = 3)

  w <- c(0.3, 0.3, 0.2, 0.2)
  names(w) <- funds
  # Should not error — falls back to rp_transform
  result <- fn_map(w, spec)
  expect_true(is.list(result))
  expect_true("weights" %in% names(result))
})

test_that("fn_map method='rp_transform' skips projection", {
  data(edhec)
  R <- edhec[, 1:4]
  funds <- colnames(R)
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.05, max = 0.65)

  w <- c(0.8, 0.1, 0.05, 0.05)
  names(w) <- funds
  result <- fn_map(w, spec, method = "rp_transform")
  expect_true(is.list(result))
  expect_true("weights" %in% names(result))
  # Should still produce feasible weights
  expect_true(sum(result$weights) >= 0.99 - 1e-6)
  expect_true(sum(result$weights) <= 1.01 + 1e-6)
})

test_that("fn_map with groups uses projection", {
  data(edhec)
  R <- edhec[, 1:4]
  funds <- colnames(R)
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.5)
  spec <- add.constraint(spec, type = "group", groups = list(1:2, 3:4),
                         group_min = c(0.3, 0.3), group_max = c(0.7, 0.7))

  w <- c(0.6, 0.2, 0.1, 0.1)
  names(w) <- funds
  result <- fn_map(w, spec)
  expect_true(sum(result$weights) >= 0.99 - 1e-8)
  expect_true(sum(result$weights) <= 1.01 + 1e-8)
  g1 <- sum(result$weights[1:2])
  g2 <- sum(result$weights[3:4])
  expect_true(g1 >= 0.3 - 1e-8 && g1 <= 0.7 + 1e-8)
  expect_true(g2 >= 0.3 - 1e-8 && g2 <= 0.7 + 1e-8)
})

# ============================================================================
# I. Edge cases
# ============================================================================

test_that("project_weights handles single-asset portfolio", {
  result <- project_weights(
    w = 0.5, min_sum = 0.99, max_sum = 1.01,
    min_box = 0, max_box = 1
  )
  expect_equal(result, 0.99, tolerance = 1e-10)
})

test_that("project_weights handles tight box constraints", {
  # All weights forced to exactly 0.25
  w <- c(0.1, 0.3, 0.4, 0.2)
  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.25, 4), max_box = rep(0.25, 4)
  )
  expect_false(is.null(result))
  expect_equal(result, rep(0.25, 4), tolerance = 1e-8)
})

test_that("project_weights handles short-selling (negative bounds)", {
  w <- c(0.3, 0.3, 0.3, 0.3)
  min_box <- c(-0.3, 0, 0, 0)
  max_box <- rep(0.8, 4)
  result <- project_weights(
    w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box
  )
  expect_false(is.null(result))
  check_feasible(result, 0.99, 1.01, min_box, max_box)
})
