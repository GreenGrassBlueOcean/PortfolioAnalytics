
require(testthat)
require(PortfolioAnalytics)

context("Phase 1A: optimize.portfolio.R edge cases")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# Helper to build a standard long-only min-variance portfolio spec
make_lo_portf <- function(assets = colnames(R5)) {
  p <- portfolio.spec(assets = assets)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
}

# ============================================================================
# 1. Single-asset portfolio
# ============================================================================

test_that("ROI errors gracefully on single-asset portfolio (constraint matrix dims)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # ROI/quadprog can't handle N=1 because the constraint matrix collapses
  R1 <- R5[, 1, drop = FALSE]
  p <- portfolio.spec(assets = colnames(R1))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 1, max = 1)
  p <- add.objective(p, type = "risk", name = "StdDev")

  expect_error(optimize.portfolio(R1, p, optimize_method = "ROI"))
})

test_that("two-asset portfolio works as minimal viable size for ROI", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  R2 <- R5[, 1:2]
  p <- portfolio.spec(assets = colnames(R2))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R2, p, optimize_method = "ROI")
  expect_s3_class(result, "optimize.portfolio")
  w <- extractWeights(result)
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

# ============================================================================
# 2. R has more columns than portfolio assets (subsetting at line 876)
# ============================================================================

test_that("optimize.portfolio subsets R to portfolio assets when R has extra columns", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # Portfolio uses only 3 of the 5 columns
  p <- make_lo_portf(assets = c("A", "B", "C"))
  result <- optimize.portfolio(R5, p, optimize_method = "ROI")

  expect_s3_class(result, "optimize.portfolio")
  w <- extractWeights(result)
  expect_equal(length(w), 3)
  expect_equal(sort(names(w)), c("A", "B", "C"))
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

# ============================================================================
# 3. Regime switching edge cases
# ============================================================================

test_that("regime switching selects correct portfolio for regime index 1", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p_risk <- make_lo_portf()
  p_ret <- portfolio.spec(assets = colnames(R5))
  p_ret <- add.constraint(p_ret, type = "full_investment")
  p_ret <- add.constraint(p_ret, type = "long_only")
  p_ret <- add.objective(p_ret, type = "return", name = "mean")

  last_date <- index(R5)[nrow(R5)]
  regime_obj <- list(
    portfolio.list = list(p_risk, p_ret),
    regime = xts::xts(c(1), order.by = last_date)
  )
  class(regime_obj) <- "regime.portfolios"

  result <- optimize.portfolio(R5, regime_obj, optimize_method = "ROI")
  expect_equal(result$regime, 1)
})

test_that("regime switching with multiple regime dates picks value at last(R)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- make_lo_portf()
  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.constraint(p2, type = "long_only")
  p2 <- add.objective(p2, type = "return", name = "mean")

  dates <- index(R5)[c(24, nrow(R5))]
  regime_obj <- list(
    portfolio.list = list(p1, p2),
    regime = xts::xts(c(1, 2), order.by = dates)
  )
  class(regime_obj) <- "regime.portfolios"

  result <- optimize.portfolio(R5, regime_obj, optimize_method = "ROI")
  expect_equal(result$regime, 2)
})

# ============================================================================
# 4. normalize_portfolio_weights edge cases
# ============================================================================

test_that("normalize_portfolio_weights handles normal case", {
  constraints <- list(min_sum = 0.99, max_sum = 1.01)
  w <- c(0.6, 0.6)  # sum = 1.2 > max_sum
  result <- PortfolioAnalytics:::normalize_portfolio_weights(w, constraints)
  expect_equal(sum(result), 1.01, tolerance = 1e-10)
})

test_that("normalize_portfolio_weights handles weights below min_sum", {
  constraints <- list(min_sum = 0.99, max_sum = 1.01)
  w <- c(0.3, 0.3)  # sum = 0.6 < min_sum

  result <- PortfolioAnalytics:::normalize_portfolio_weights(w, constraints)
  expect_equal(sum(result), 0.99, tolerance = 1e-10)
})

test_that("normalize_portfolio_weights handles Inf max_sum", {
  constraints <- list(min_sum = 0.5, max_sum = Inf)
  w <- c(10, 10)  # sum = 20, but max_sum is Inf so no normalization
  result <- PortfolioAnalytics:::normalize_portfolio_weights(w, constraints)
  expect_equal(result, w)
})

test_that("normalize_portfolio_weights handles -Inf min_sum", {
  constraints <- list(min_sum = -Inf, max_sum = 2)
  w <- c(-5, -5)  # sum = -10 < min_sum (-Inf), no normalization for min
  result <- PortfolioAnalytics:::normalize_portfolio_weights(w, constraints)
  expect_equal(result, w)
})

test_that("normalize_portfolio_weights handles NULL constraints gracefully", {
  w <- c(0.3, 0.7)
  result <- PortfolioAnalytics:::normalize_portfolio_weights(w, list())
  expect_equal(result, w)
})

test_that("normalize_portfolio_weights with sum(weights)=0 and max_sum triggers division by zero", {
  # Documents the known division-by-zero vulnerability
  constraints <- list(min_sum = 0.99, max_sum = 1.01)
  w <- c(0.5, -0.5)  # sum = 0

  # When sum(weights) == 0: (max_sum / 0) * weights produces NaN/Inf
  # This is a known edge case—we just document the current behavior
  result <- PortfolioAnalytics:::normalize_portfolio_weights(w, constraints)
  # sum(w) == 0, which is < min_sum (0.99), so min_sum normalization triggers:
  # (0.99 / 0) * w => Inf/-Inf
  expect_true(any(!is.finite(result)))
})

# ============================================================================
# 5. Training period and rolling window defaults
# ============================================================================

test_that("rebalancing defaults training_period=36 when both NULL", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # Use enough data so that training_period=36 allows at least 1 rebalance
  R_long <- edhec[1:60, 1:3]
  colnames(R_long) <- c("A", "B", "C")
  p <- make_lo_portf(assets = colnames(R_long))

  result <- optimize.portfolio.rebalancing(
    R_long, p,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = NULL,
    rolling_window = NULL
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
  expect_true(length(result$opt_rebalancing) >= 1)
})

test_that("rebalancing uses nrow(R) as training_period when data < 36 rows", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  R_short <- R5[1:24, 1:3]
  colnames(R_short) <- c("A", "B", "C")
  p <- make_lo_portf(assets = colnames(R_short))

  result <- optimize.portfolio.rebalancing(
    R_short, p,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = NULL,
    rolling_window = NULL
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
})

# ============================================================================
# 6. Empty rebalancing schedule (training_period >= nrow(R))
# ============================================================================

test_that("rebalancing with training_period >= nrow(R) returns empty opt_rebalancing", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf(assets = c("A", "B", "C"))
  R_short <- R5[1:12, 1:3]
  colnames(R_short) <- c("A", "B", "C")

  result <- optimize.portfolio.rebalancing(
    R_short, p,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 100
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
  expect_equal(length(result$opt_rebalancing), 0)
})

# ============================================================================
# 7. trace=TRUE vs trace=FALSE
# ============================================================================

test_that("trace=FALSE does not store R in result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf()
  result <- optimize.portfolio(R5, p, optimize_method = "ROI", trace = FALSE)
  expect_null(result$R)
})

test_that("trace=TRUE stores R in result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf()
  result <- optimize.portfolio(R5, p, optimize_method = "ROI", trace = TRUE)
  expect_false(is.null(result$R))
  expect_equal(ncol(result$R), 5)
})

# ============================================================================
# 8. optimize_method edge cases
# ============================================================================

test_that("empty string optimize_method raises error", {
  p <- make_lo_portf()
  expect_error(
    optimize.portfolio(R5, p, optimize_method = "")
  )
})

test_that("single-element optimize_method vector works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf()
  result <- optimize.portfolio(R5, p, optimize_method = c("ROI"))
  expect_s3_class(result, "optimize.portfolio.ROI")
})

# ============================================================================
# 9. Explicit penalty numeric override
# ============================================================================

test_that("explicit numeric penalty is stored correctly", {
  p <- make_lo_portf()

  set.seed(4829)
  result <- optimize.portfolio(R5, p, optimize_method = "random",
                               search_size = 50, penalty = 500)
  expect_equal(result$penalty, 500)
})

# ============================================================================
# 10. Separate objectives insertion
# ============================================================================

test_that("objectives passed separately are inserted into portfolio", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")

  obj <- return_objective(name = "mean")
  result <- optimize.portfolio(R5, p, optimize_method = "ROI",
                               objectives = list(obj))
  expect_s3_class(result, "optimize.portfolio")
})

# ============================================================================
# 11. Rebalancing: warm_start with optimization failure recovery
# ============================================================================

test_that("warm_start=TRUE cold-restarts after optimization failure window", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  R_long <- edhec[1:60, 1:3]
  colnames(R_long) <- c("A", "B", "C")
  p <- make_lo_portf(assets = colnames(R_long))

  # This should run sequentially with warm start, and any failed window

  # should not crash the entire rebalancing
  result <- optimize.portfolio.rebalancing(
    R_long, p,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    warm_start = TRUE
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")

  # Check that warm start produced valid windows
  n_windows <- length(result$opt_rebalancing)
  expect_true(n_windows >= 1)

  valid_windows <- sum(sapply(result$opt_rebalancing, function(x) {
    inherits(x, "optimize.portfolio") && !is.optimization_failure(x)
  }))
  expect_true(valid_windows >= 1)
})

# ============================================================================
# 12. Rebalancing: parallel (foreach) path with rolling_window
# ============================================================================

test_that("rebalancing foreach path with rolling_window works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  R_long <- edhec[1:60, 1:3]
  colnames(R_long) <- c("A", "B", "C")
  p <- make_lo_portf(assets = colnames(R_long))

  result <- optimize.portfolio.rebalancing(
    R_long, p,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    rolling_window = 36,
    warm_start = FALSE
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
  expect_true(length(result$opt_rebalancing) >= 1)
})

test_that("rebalancing foreach path without rolling_window (expanding)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  R_long <- edhec[1:60, 1:3]
  colnames(R_long) <- c("A", "B", "C")
  p <- make_lo_portf(assets = colnames(R_long))

  result <- optimize.portfolio.rebalancing(
    R_long, p,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    rolling_window = NULL,
    warm_start = FALSE
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
  expect_true(length(result$opt_rebalancing) >= 1)
})

# ============================================================================
# 13. optimize.portfolio.parallel with nodes=1
# ============================================================================

test_that("optimize.portfolio.parallel with nodes=1 works", {
  p <- make_lo_portf()

  set.seed(6284)
  result <- optimize.portfolio.parallel(
    R5, p,
    optimize_method = "random",
    search_size = 50,
    nodes = 1
  )
  expect_s3_class(result, "optimize.portfolio.parallel")
  expect_equal(length(result$optimizations), 1)
  expect_s3_class(result$optimizations[[1]], "optimize.portfolio")
})

# ============================================================================
# 14. Class naming: verify solver-specific class is applied
# ============================================================================

test_that("result class matches optimize_method", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf()

  r_roi <- optimize.portfolio(R5, p, optimize_method = "ROI")
  expect_true("optimize.portfolio.ROI" %in% class(r_roi))
  expect_true("optimize.portfolio" %in% class(r_roi))

  set.seed(3718)
  r_rp <- optimize.portfolio(R5, p, optimize_method = "random", search_size = 50)
  expect_true("optimize.portfolio.random" %in% class(r_rp))
  expect_true("optimize.portfolio" %in% class(r_rp))
})

# ============================================================================
# 15. Data summary fields are populated correctly
# ============================================================================

test_that("data_summary contains first and last rows of R", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf()
  result <- optimize.portfolio(R5, p, optimize_method = "ROI")

  expect_true(is.list(result$data_summary))
  expect_equal(nrow(result$data_summary$first), 1)
  expect_equal(nrow(result$data_summary$last), 1)
  expect_equal(as.character(index(result$data_summary$first)),
               as.character(index(R5)[1]))
  expect_equal(as.character(index(result$data_summary$last)),
               as.character(index(R5)[nrow(R5)]))
})

# ============================================================================
# 16. Rebalancing with random solver generates rp matrix
# ============================================================================

test_that("rebalancing with random solver auto-generates rp matrix", {
  R_long <- edhec[1:60, 1:3]
  colnames(R_long) <- c("A", "B", "C")
  p <- make_lo_portf(assets = colnames(R_long))

  set.seed(8192)
  result <- optimize.portfolio.rebalancing(
    R_long, p,
    optimize_method = "random",
    search_size = 50,
    rebalance_on = "quarters",
    training_period = 24
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
  expect_true(length(result$opt_rebalancing) >= 1)
})

# ============================================================================
# 17. Penalty is stored on every result
# ============================================================================

test_that("penalty field is present on result for all solver types", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf()

  # ROI defaults to penalty="auto" → 1e4 for non-stochastic
  r_roi <- optimize.portfolio(R5, p, optimize_method = "ROI")
  expect_true(!is.null(r_roi$penalty))
  expect_equal(r_roi$penalty, 1e4)

  # Random with auto penalty
  set.seed(5539)
  r_rp <- optimize.portfolio(R5, p, optimize_method = "random",
                              search_size = 50, penalty = "auto")
  expect_true(is.numeric(r_rp$penalty))
  expect_true(r_rp$penalty > 0)
})

# ============================================================================
# 18. Warm-start with unnamed vector (names=NULL) passes validation
# ============================================================================

test_that("warm_start with unnamed vector of correct length is accepted", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_portf()
  ws <- rep(0.2, 5)  # unnamed, correct length

  # Should NOT warn
  expect_no_warning(
    result <- optimize.portfolio(R5, p, optimize_method = "ROI", warm_start = ws)
  )
  expect_s3_class(result, "optimize.portfolio")
})
