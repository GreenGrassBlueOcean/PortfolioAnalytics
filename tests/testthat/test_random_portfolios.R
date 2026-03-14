##### test_random_portfolios.R #####
# Coverage tests for random_portfolios.R
# Targets: generatesequence, randomize_portfolio (v2), random_portfolios
#          (sample / simplex / grid), rp_sample, rp_simplex, rp_grid,
#          eliminate path, output structure, constraint satisfaction

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]
funds <- colnames(R4)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_base_portf <- function() {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p
}

make_box_portf <- function() {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.55)
  p
}

# Validate constraint satisfaction for a matrix of portfolios
all_satisfy <- function(rp_mat, portfolio) {
  all(apply(rp_mat, 1, PortfolioAnalytics:::check_constraints,
            portfolio = portfolio))
}

# ===========================================================================
# 1. generatesequence
# ===========================================================================

test_that("generatesequence returns expected default sequence", {
  s <- generatesequence()
  expect_true(is.numeric(s))
  expect_true(length(s) > 0)
  expect_equal(s[1], 0.01)
  expect_equal(s[length(s)], 1)
  # step size should be min/max = 0.01
  expect_equal(s[2] - s[1], 0.01)
})

test_that("generatesequence respects custom min/max/by", {
  s <- generatesequence(min = 0.1, max = 0.5, by = 0.1)
  expect_equal(s, c(0.1, 0.2, 0.3, 0.4, 0.5))
})

test_that("generatesequence rounds to specified digits", {
  s <- generatesequence(min = 0.001, max = 0.01, by = 0.003, rounding = 4)
  expect_true(all(nchar(sub(".*\\.", "", as.character(s))) <= 4))
})

test_that("generatesequence with min = max returns single value", {
  s <- generatesequence(min = 0.5, max = 0.5, by = 0.1)
  expect_equal(s, 0.5)
})

# ===========================================================================
# 2. randomize_portfolio (v2) — single portfolio generation
# ===========================================================================

test_that("randomize_portfolio returns numeric vector of correct length", {
  set.seed(5821)
  p <- make_base_portf()
  w <- randomize_portfolio(p)
  expect_true(is.numeric(w))
  expect_length(w, length(funds))
})

test_that("randomize_portfolio produces weights satisfying leverage constraint", {
  set.seed(7104)
  p <- make_base_portf()
  w <- randomize_portfolio(p)
  expect_true(sum(w) >= 0.99 - 0.01)
  expect_true(sum(w) <= 1.01 + 0.01)
})

test_that("randomize_portfolio satisfies box constraints", {
  set.seed(6437)
  p <- make_box_portf()
  w <- randomize_portfolio(p)
  constraints <- PortfolioAnalytics:::get_constraints(p)
  expect_true(all(w >= constraints$min - 1e-6))
  expect_true(all(w <= constraints$max + 1e-6))
})

test_that("randomize_portfolio uses custom weight_seq", {
  p <- make_base_portf()
  p$weight_seq <- generatesequence(min = 0, max = 1, by = 0.25)
  set.seed(3289)
  w <- randomize_portfolio(p)
  expect_true(is.numeric(w))
  expect_length(w, length(funds))
})

test_that("randomize_portfolio_v2 is identical to randomize_portfolio", {
  expect_identical(randomize_portfolio, randomize_portfolio_v2)
})

# ===========================================================================
# 3. random_portfolios — sample method (default)
# ===========================================================================

test_that("random_portfolios (sample) returns matrix with correct dimensions", {
  set.seed(4981)
  p <- make_base_portf()
  rp <- random_portfolios(p, permutations = 20, rp_method = "sample",
                          eliminate = FALSE)
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), length(funds))
  expect_equal(colnames(rp), names(p$assets))
})

test_that("random_portfolios (sample) includes seed portfolio", {
  set.seed(8823)
  p <- make_base_portf()
  rp <- random_portfolios(p, permutations = 20, rp_method = "sample",
                          eliminate = FALSE)
  seed <- as.numeric(p$assets)
  # Seed should be present (typically row 1, but unique() may reorder)
  has_seed <- any(apply(rp, 1, function(r) isTRUE(all.equal(as.numeric(r), seed))))
  expect_true(has_seed)
})

test_that("random_portfolios (sample) with long-only produces non-negative weights", {
  set.seed(6215)
  p <- make_base_portf()
  rp <- random_portfolios(p, permutations = 50, rp_method = "sample",
                          eliminate = FALSE)
  expect_true(all(rp >= -1e-6))
})

test_that("random_portfolios (sample) with box constraints generates feasible portfolios", {
  set.seed(7732)
  p <- make_box_portf()
  rp <- random_portfolios(p, permutations = 50, rp_method = "sample",
                          eliminate = TRUE)
  if (nrow(rp) > 0) {
    expect_true(all_satisfy(rp, p))
  }
})

test_that("random_portfolios (sample) generates distinct portfolios", {
  set.seed(9016)
  p <- make_base_portf()
  rp <- random_portfolios(p, permutations = 50, rp_method = "sample",
                          eliminate = FALSE)
  # unique() is already applied inside rp_sample, but verify diversity
  expect_true(nrow(unique(rp)) > 1)
})

# ===========================================================================
# 4. random_portfolios — simplex method
# ===========================================================================

test_that("random_portfolios (simplex) returns matrix with correct dimensions", {
  set.seed(2458)
  p <- make_base_portf()
  rp <- random_portfolios(p, permutations = 50, rp_method = "simplex",
                          eliminate = FALSE)
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), length(funds))
})

test_that("random_portfolios (simplex) weights sum to 1", {
  set.seed(4601)
  p <- make_base_portf()
  rp <- random_portfolios(p, permutations = 100, rp_method = "simplex",
                          eliminate = FALSE)
  row_sums <- rowSums(rp)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("random_portfolios (simplex) satisfies min box constraints", {
  set.seed(3910)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 1)
  rp <- random_portfolios(p, permutations = 100, rp_method = "simplex",
                          eliminate = FALSE)
  constraints <- PortfolioAnalytics:::get_constraints(p)
  expect_true(all(rp >= constraints$min - 1e-6))
})

test_that("random_portfolios (simplex) with explicit fev passes eval.parent", {
  set.seed(5503)
  p <- make_base_portf()
  # Exercises the eval.parent(match.call()$fev) fix: upstream used raw
  # match.call()$fev which returned an unevaluated language object
  rp <- random_portfolios(p, permutations = 60, rp_method = "simplex",
                          fev = 0:2, eliminate = FALSE)
  expect_true(is.matrix(rp))
  expect_true(nrow(rp) >= 60)
})

test_that("random_portfolios (simplex) with fev passed as variable works", {
  set.seed(5503)
  p <- make_base_portf()
  my_fev <- 0:3
  rp <- random_portfolios(p, permutations = 40, rp_method = "simplex",
                          fev = my_fev, eliminate = FALSE)
  expect_true(is.matrix(rp))
  expect_true(nrow(rp) >= 40)
})

test_that("random_portfolios (grid) with normalize passed as variable works", {
  set.seed(4412)
  p <- make_box_portf()
  do_norm <- TRUE
  rp <- random_portfolios(p, permutations = 16, rp_method = "grid",
                          normalize = do_norm, eliminate = FALSE)
  expect_true(is.matrix(rp))
  expect_true(nrow(rp) >= 16)
})

test_that("rp_simplex with scalar fev works", {
  set.seed(1674)
  p <- make_base_portf()
  rp <- rp_simplex(p, permutations = 30, fev = 3)
  expect_true(is.matrix(rp))
  expect_true(nrow(rp) >= 30)
  expect_true(all(abs(rowSums(rp) - 1) < 1e-6))
})

test_that("rp_simplex higher fev concentrates weights", {
  set.seed(8291)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")

  rp_low <- rp_simplex(p, permutations = 200, fev = 0)
  rp_high <- rp_simplex(p, permutations = 200, fev = 8)

  # Higher fev should produce more concentrated portfolios (higher max weight)
  max_wt_low <- mean(apply(rp_low, 1, max))
  max_wt_high <- mean(apply(rp_high, 1, max))
  expect_true(max_wt_high > max_wt_low)
})

# ===========================================================================
# 5. random_portfolios — grid method
# ===========================================================================

test_that("random_portfolios (grid) returns matrix with correct dimensions", {
  set.seed(1203)
  p <- make_box_portf()
  rp <- random_portfolios(p, permutations = 50, rp_method = "grid",
                          eliminate = FALSE)
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), length(funds))
})

test_that("rp_grid with normalize=TRUE adjusts weight sums", {
  set.seed(2947)
  p <- make_box_portf()
  rp <- rp_grid(p, permutations = 50, normalize = TRUE)
  # With normalization, sums should be closer to target range
  constraints <- PortfolioAnalytics:::get_constraints(p)
  row_sums <- rowSums(rp)
  # All sums should be >= min_sum after normalization
  expect_true(all(row_sums >= constraints$min_sum - 1e-6))
})

test_that("rp_grid with normalize=FALSE returns raw grid", {
  set.seed(3761)
  p <- make_box_portf()
  rp_norm <- rp_grid(p, permutations = 50, normalize = TRUE)
  rp_raw <- rp_grid(p, permutations = 50, normalize = FALSE)
  # Dimensions should be the same
  expect_equal(ncol(rp_norm), ncol(rp_raw))
  # But the actual weights may differ
  expect_false(isTRUE(all.equal(rp_norm, rp_raw)))
})

test_that("rp_grid generates at least permutations rows", {
  p <- make_box_portf()
  rp <- rp_grid(p, permutations = 16, normalize = FALSE)
  expect_true(nrow(rp) >= 16)
})

test_that("rp_grid respects box constraints", {
  p <- make_box_portf()
  constraints <- PortfolioAnalytics:::get_constraints(p)
  rp <- rp_grid(p, permutations = 50, normalize = FALSE)
  expect_true(all(rp >= min(constraints$min) - 1e-6))
  expect_true(all(rp <= max(constraints$max) + 1e-6))
})

# ===========================================================================
# 6. eliminate parameter
# ===========================================================================

test_that("eliminate=TRUE removes infeasible portfolios", {
  set.seed(7045)
  p <- make_box_portf()
  rp_all <- random_portfolios(p, permutations = 50, rp_method = "sample",
                              eliminate = FALSE)
  rp_feas <- random_portfolios(p, permutations = 50, rp_method = "sample",
                               eliminate = TRUE)
  # Feasible set should be <= total set

  expect_true(nrow(rp_feas) <= nrow(rp_all))
  # All remaining portfolios should satisfy constraints
  if (nrow(rp_feas) > 0) {
    expect_true(all_satisfy(rp_feas, p))
  }
})

test_that("eliminate=TRUE with simplex removes infeasible portfolios", {
  set.seed(2158)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.10, max = 0.50)
  rp_feas <- random_portfolios(p, permutations = 100, rp_method = "simplex",
                               eliminate = TRUE)
  if (nrow(rp_feas) > 0) {
    expect_true(all_satisfy(rp_feas, p))
  }
})

# ===========================================================================
# 7. Group constraints with random portfolios
# ===========================================================================

test_that("random_portfolios (sample) with group constraints generates feasible portfolios", {
  set.seed(9321)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.55)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.2, 0.2),
                      group_max = c(0.8, 0.8))
  rp <- random_portfolios(p, permutations = 50, rp_method = "sample",
                          eliminate = TRUE)
  if (nrow(rp) > 0) {
    expect_true(all_satisfy(rp, p))
  }
})

# ===========================================================================
# 8. Position limit constraints with random portfolios
# ===========================================================================

test_that("random_portfolios (sample) with position limit constraints", {
  set.seed(4576)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 0.80)
  p <- add.constraint(p, type = "position_limit", max_pos = 3)
  rp <- random_portfolios(p, permutations = 50, rp_method = "sample",
                          eliminate = TRUE)
  if (nrow(rp) > 0) {
    # Each feasible portfolio should have at most 3 non-zero positions
    n_pos <- apply(rp, 1, function(w) sum(abs(w) > 1e-6))
    expect_true(all(n_pos <= 3))
  }
})

# ===========================================================================
# 9. Leverage exposure constraint
# ===========================================================================

test_that("random_portfolios (sample) with leverage exposure constraint", {
  set.seed(3082)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = -0.3, max = 0.65)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 1.6)
  rp <- random_portfolios(p, permutations = 50, rp_method = "sample",
                          eliminate = TRUE)
  if (nrow(rp) > 0) {
    lev <- apply(rp, 1, function(w) sum(abs(w)))
    expect_true(all(lev <= 1.6 + 1e-6))
  }
})

# ===========================================================================
# 10. random_portfolios_v2 alias
# ===========================================================================

test_that("random_portfolios_v2 is identical to random_portfolios", {
  expect_identical(random_portfolios, random_portfolios_v2)
})

# ===========================================================================
# 11. random_walk_portfolios wrapper
# ===========================================================================

test_that("random_walk_portfolios is a wrapper for random_portfolios", {
  # Just verify it calls through without error
  set.seed(8403)
  p <- make_base_portf()
  rp <- random_walk_portfolios(portfolio = p, permutations = 10,
                               rp_method = "sample", eliminate = FALSE)
  expect_true(is.matrix(rp))
  expect_true(nrow(rp) > 0)
})

# ===========================================================================
# 12. Edge cases
# ===========================================================================

test_that("random_portfolios works with 2-asset portfolio", {
  set.seed(6890)
  p <- portfolio.spec(assets = funds[1:2])
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  rp <- random_portfolios(p, permutations = 20, rp_method = "sample",
                          eliminate = FALSE)
  expect_equal(ncol(rp), 2)
  expect_equal(colnames(rp), names(p$assets))
})

test_that("rp_simplex works with 2-asset portfolio", {
  set.seed(5512)
  p <- portfolio.spec(assets = funds[1:2])
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  rp <- rp_simplex(p, permutations = 30, fev = 0:2)
  expect_equal(ncol(rp), 2)
  expect_true(all(abs(rowSums(rp) - 1) < 1e-6))
})

test_that("random_portfolios with tight leverage finds feasible portfolios", {
  set.seed(4017)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.10, max = 0.40)
  rp <- random_portfolios(p, permutations = 50, rp_method = "sample",
                          eliminate = TRUE)
  if (nrow(rp) > 0) {
    expect_true(all_satisfy(rp, p))
    row_sums <- rowSums(rp)
    expect_true(all(row_sums >= 0.99 - 1e-6))
    expect_true(all(row_sums <= 1.01 + 1e-6))
  }
})

# ===========================================================================
# 13. Full investment (min_sum = max_sum = 1) with sample method
# ===========================================================================

test_that("random_portfolios with full_investment constraint", {
  set.seed(1456)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  rp <- random_portfolios(p, permutations = 30, rp_method = "sample",
                          eliminate = FALSE)
  # full_investment internally gets expanded to min_sum and max_sum;
  # randomize_portfolio adds wiggle room, so sums should be close to 1
  row_sums <- rowSums(rp)
  expect_true(all(abs(row_sums - 1) < 0.02))
})

# ===========================================================================
# 14. Short-selling (negative weights)
# ===========================================================================

test_that("random_portfolios allows negative weights with short-selling box", {
  set.seed(2731)
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = -0.3, max = 0.65)
  rp <- random_portfolios(p, permutations = 100, rp_method = "sample",
                          eliminate = FALSE)
  # Some portfolios should have negative weights
  expect_true(any(rp < 0))
})

# ===========================================================================
# 15. rp_sample includes seed and equal-weight rows
# ===========================================================================

test_that("rp_sample includes seed and equal-weight in output", {
  set.seed(3349)
  # Use non-equal seed so seed row and equal-weight row are distinct
  p <- portfolio.spec(assets = c(CA = 0.4, CTAG = 0.3, DS = 0.2, EM = 0.1))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  rp <- rp_sample(p, permutations = 20)
  # Row 1 should be the seed
  expect_equal(as.numeric(rp[1, ]), as.numeric(p$assets), tolerance = 1e-10)
  # Row 2 should be equal weight
  eq_wt <- rep(1 / length(p$assets), length(p$assets))
  expect_equal(as.numeric(rp[2, ]), eq_wt, tolerance = 1e-10)
})

# ===========================================================================
# 16. rp_sample column names
# ===========================================================================

test_that("rp_sample sets correct column names", {
  set.seed(7634)
  p <- make_base_portf()
  rp <- rp_sample(p, permutations = 10)
  expect_equal(colnames(rp), names(p$assets))
})
