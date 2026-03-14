library(testthat)
library(PortfolioAnalytics)

context("constraint_fn_map helpers: violation detectors, rp_transform, weight adjusters")

# ============================================================================
# A. Violation detectors
# ============================================================================

test_that("min_sum_fail detects violation and handles NULL", {
  expect_true(PortfolioAnalytics:::min_sum_fail(c(0.1, 0.2, 0.3), min_sum = 0.99))
  expect_false(PortfolioAnalytics:::min_sum_fail(c(0.4, 0.3, 0.3), min_sum = 0.99))
  expect_false(PortfolioAnalytics:::min_sum_fail(c(0.5, 0.5), min_sum = NULL))
})

test_that("max_sum_fail detects violation and handles NULL", {
  expect_true(PortfolioAnalytics:::max_sum_fail(c(0.5, 0.4, 0.3), max_sum = 1.01))
  expect_false(PortfolioAnalytics:::max_sum_fail(c(0.3, 0.3, 0.3), max_sum = 1.01))
  expect_false(PortfolioAnalytics:::max_sum_fail(c(0.5, 0.5), max_sum = NULL))
})

test_that("leverage_fail detects violation and handles NULL", {
  expect_true(PortfolioAnalytics:::leverage_fail(c(0.5, -0.5, 0.8), leverage = 1.5))
  expect_false(PortfolioAnalytics:::leverage_fail(c(0.3, -0.2, 0.5), leverage = 1.5))
  expect_false(PortfolioAnalytics:::leverage_fail(c(0.5, 0.5), leverage = NULL))
})

test_that("group_fail returns logical vector per group", {
  weights <- c(0.1, 0.2, 0.4, 0.3)
  groups <- list(1:2, 3:4)
  # Group 1 sum = 0.3, Group 2 sum = 0.7

  # Group 1 violates cLO = 0.4

  result <- PortfolioAnalytics:::group_fail(weights, groups, cLO = c(0.4, 0.3), cUP = c(0.6, 0.8))
  expect_equal(result, c(TRUE, FALSE))

  # Both groups within bounds
  result2 <- PortfolioAnalytics:::group_fail(weights, groups, cLO = c(0.2, 0.5), cUP = c(0.5, 0.8))
  expect_equal(result2, c(FALSE, FALSE))

  # Returns FALSE when groups is NULL
  expect_false(PortfolioAnalytics:::group_fail(weights, NULL, c(0.3, 0.3), c(0.7, 0.7)))
})

test_that("group_fail detects group_pos violations", {
  weights <- c(0.1, 0.2, 0.3, 0.4)
  groups <- list(1:2, 3:4)
  # Both groups have 2 non-zero weights; set group_pos = 1 for group 1
  result <- PortfolioAnalytics:::group_fail(weights, groups,
                                            cLO = c(0, 0), cUP = c(1, 1),
                                            group_pos = c(1, 2))
  expect_true(result[1])
  expect_false(result[2])
})

test_that("pos_limit_fail detects max_pos violation", {
  w <- c(0.3, 0.3, 0.2, 0.2)
  expect_true(pos_limit_fail(w, max_pos = 3, max_pos_long = NULL, max_pos_short = NULL))
  expect_false(pos_limit_fail(w, max_pos = 4, max_pos_long = NULL, max_pos_short = NULL))
})

test_that("pos_limit_fail detects max_pos_long and max_pos_short", {
  w <- c(0.3, 0.3, -0.1, -0.1)
  # 2 long, 2 short
  expect_true(pos_limit_fail(w, max_pos = NULL, max_pos_long = 1, max_pos_short = NULL))
  expect_true(pos_limit_fail(w, max_pos = NULL, max_pos_long = NULL, max_pos_short = 1))
  expect_false(pos_limit_fail(w, max_pos = NULL, max_pos_long = 2, max_pos_short = 2))
})

test_that("pos_limit_fail returns FALSE when all limits are NULL", {
  expect_false(pos_limit_fail(c(0.5, 0.5), max_pos = NULL, max_pos_long = NULL, max_pos_short = NULL))
})

# ============================================================================
# B. Weight adjustment helpers (stochastic — need seeds)
# ============================================================================

test_that("rp_increase raises weight sum toward min_sum", {
  set.seed(6142)
  w <- c(0.1, 0.1, 0.1)
  weight_seq <- seq(0, 0.6, by = 0.002)
  result <- PortfolioAnalytics:::rp_increase(
    weights = w, min_sum = 0.99,
    max_box = rep(0.6, 3), weight_seq = weight_seq
  )
  expect_gte(sum(result), sum(w))
})

test_that("rp_increase returns weights unchanged when already above min_sum", {
  w <- c(0.4, 0.3, 0.3)
  result <- PortfolioAnalytics:::rp_increase(
    weights = w, min_sum = 0.99,
    max_box = rep(0.6, 3), weight_seq = seq(0, 0.6, by = 0.002)
  )
  expect_equal(result, w)
})

test_that("rp_decrease lowers weight sum toward max_sum", {
  set.seed(8417)
  w <- c(0.5, 0.4, 0.3)
  weight_seq <- seq(0, 0.6, by = 0.002)
  result <- PortfolioAnalytics:::rp_decrease(
    weights = w, max_sum = 1.01,
    min_box = rep(0, 3), weight_seq = weight_seq
  )
  expect_lte(sum(result), sum(w))
})

test_that("rp_decrease returns weights unchanged when already below max_sum", {
  w <- c(0.3, 0.3, 0.3)
  result <- PortfolioAnalytics:::rp_decrease(
    weights = w, max_sum = 1.01,
    min_box = rep(0, 3), weight_seq = seq(0, 0.6, by = 0.002)
  )
  expect_equal(result, w)
})

test_that("rp_decrease_leverage reduces leverage exposure", {
  set.seed(3259)
  w <- c(0.5, -0.4, 0.6, -0.3)
  weight_seq <- seq(-0.6, 0.6, by = 0.002)
  result <- PortfolioAnalytics:::rp_decrease_leverage(
    weights = w, max_box = rep(0.6, 4), min_box = rep(-0.6, 4),
    leverage = 1.2, weight_seq = weight_seq
  )
  expect_lte(sum(abs(result)), sum(abs(w)))
})

test_that("rp_position_limit reduces long positions", {
  set.seed(5073)
  # 4 positive weights; max_pos_long = 2 means 2 need to become <= 0
  w <- c(0.3, 0.3, 0.2, 0.2)
  weight_seq <- c(seq(-0.3, 0.6, by = 0.002), 0)
  result <- PortfolioAnalytics:::rp_position_limit(
    weights = w, max_pos = NULL,
    max_pos_long = 2, max_pos_short = NULL,
    min_box = rep(-0.3, 4), max_box = rep(0.6, 4),
    weight_seq = weight_seq
  )
  tol <- .Machine$double.eps^0.5
  expect_lte(sum(result > tol), 2)
})

# ============================================================================
# C. rp_transform — direct tests
# ============================================================================

test_that("rp_transform repairs weight_sum violation (too low)", {
  set.seed(2784)
  w <- c(0.1, 0.1, 0.1, 0.1)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, 4), max_box = rep(0.6, 4),
    max_permutations = 500
  )
  expect_true(sum(result) >= 0.99)
  expect_true(sum(result) <= 1.01)
})

test_that("rp_transform repairs weight_sum violation (too high)", {
  set.seed(9316)
  w <- c(0.4, 0.4, 0.3, 0.3)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, 4), max_box = rep(0.6, 4),
    max_permutations = 500
  )
  expect_true(sum(result) >= 0.99)
  expect_true(sum(result) <= 1.01)
})

test_that("rp_transform respects group constraints", {
  set.seed(4189)
  w <- c(0.1, 0.1, 0.5, 0.3)
  groups <- list(1:2, 3:4)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.05, 4), max_box = rep(0.5, 4),
    groups = groups, cLO = c(0.3, 0.3), cUP = c(0.7, 0.7),
    max_permutations = 500
  )
  expect_true(sum(result) >= 0.99)
  expect_true(sum(result) <= 1.01)
  expect_true(sum(result[1:2]) >= 0.3 - 1e-6)
  expect_true(sum(result[3:4]) <= 0.7 + 1e-6)
})

test_that("rp_transform enforces position limit", {
  set.seed(7625)
  w <- c(0.3, 0.3, 0.2, 0.2)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, 4), max_box = rep(0.6, 4),
    max_pos = 3, max_permutations = 500
  )
  tol <- .Machine$double.eps^0.5
  expect_lte(sum(abs(result) > tol), 3)
  expect_true(sum(result) >= 0.99)
  expect_true(sum(result) <= 1.01)
})

test_that("rp_transform enforces leverage constraint", {
  set.seed(1483)
  w <- c(0.5, -0.3, 0.5, 0.3)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(-0.5, 4), max_box = rep(0.6, 4),
    leverage = 1.2, max_permutations = 500
  )
  expect_true(sum(result) >= 0.99)
  expect_true(sum(result) <= 1.01)
  expect_lte(sum(abs(result)), 1.2 + 1e-6)
})

test_that("rp_transform errors on truly infeasible problem", {
  # min_box sums to 2.0 but max_sum = 1.01
  expect_error(
    rp_transform(
      w = rep(0.5, 4), min_sum = 0.99, max_sum = 1.01,
      min_box = rep(0.5, 4), max_box = rep(1, 4),
      max_permutations = 50
    ),
    "Infeasible"
  )
})

# ============================================================================
# D. fn_map integration via rp_transform path
# ============================================================================

test_that("fn_map with rp_transform handles group constraints", {
  set.seed(3847)
  data(edhec)
  funds <- colnames(edhec)[1:4]
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.5)
  spec <- add.constraint(spec, type = "group", groups = list(1:2, 3:4),
                         group_min = c(0.3, 0.3), group_max = c(0.7, 0.7))

  w <- c(0.6, 0.2, 0.1, 0.1)
  names(w) <- funds
  result <- fn_map(w, spec, method = "rp_transform")

  expect_true(sum(result$weights) >= 0.99 - 1e-4)
  expect_true(sum(result$weights) <= 1.01 + 1e-4)
  expect_true(sum(result$weights[1:2]) >= 0.3 - 1e-4)
  expect_true(sum(result$weights[3:4]) <= 0.7 + 1e-4)
})

test_that("fn_map with rp_transform handles position limits", {
  set.seed(5294)
  data(edhec)
  funds <- colnames(edhec)[1:6]
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.65)
  spec <- add.constraint(spec, type = "position_limit", max_pos = 4)

  w <- rep(1/6, 6)
  names(w) <- funds
  result <- fn_map(w, spec, method = "rp_transform")

  expect_true(is.list(result))
  expect_true("weights" %in% names(result))
  tol <- .Machine$double.eps^0.5
  expect_lte(sum(abs(result$weights) > tol), 4)
})

test_that("fn_map with rp_transform handles leverage constraint", {
  set.seed(7128)
  data(edhec)
  funds <- colnames(edhec)[1:4]
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  spec <- add.constraint(spec, type = "box", min = -0.3, max = 0.6)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 1.3)

  w <- c(0.6, -0.3, 0.5, 0.2)
  names(w) <- funds
  result <- fn_map(w, spec, method = "rp_transform")

  expect_true(is.list(result))
  expect_true(sum(result$weights) >= 0.99 - 1e-3)
  expect_true(sum(result$weights) <= 1.01 + 1e-3)
})

test_that("fn_map with relax=TRUE relaxes box constraints", {
  set.seed(9461)
  data(edhec)
  funds <- colnames(edhec)[1:4]
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.2, max = 0.35)

  # Start with violating weights; box constraints are tight
  w <- c(0.5, 0.3, 0.1, 0.1)
  names(w) <- funds
  result <- fn_map(w, spec, relax = TRUE, method = "rp_transform")

  expect_true(is.list(result))
  expect_true("weights" %in% names(result))
  expect_true("min" %in% names(result))
  expect_true("max" %in% names(result))
})

test_that("fn_map with relax=TRUE relaxes group constraints", {
  set.seed(2637)
  data(edhec)
  funds <- colnames(edhec)[1:4]
  spec <- portfolio.spec(assets = funds)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.05, max = 0.5)
  # Very tight group constraints
  spec <- add.constraint(spec, type = "group", groups = list(1:2, 3:4),
                         group_min = c(0.49, 0.49), group_max = c(0.51, 0.51))

  w <- c(0.4, 0.1, 0.1, 0.4)
  names(w) <- funds
  result <- fn_map(w, spec, relax = TRUE, method = "rp_transform")

  expect_true(is.list(result))
  expect_true("cLO" %in% names(result))
  expect_true("cUP" %in% names(result))
})
