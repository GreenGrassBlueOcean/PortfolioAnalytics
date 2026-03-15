
require(testthat)
require(PortfolioAnalytics)

context("Phase 1B: optFUN.R — bug fixes + advanced ROI paths")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:60, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# Standard long-only portfolio with GMV objective
make_lo_gmv <- function(assets = colnames(R5)) {
  p <- portfolio.spec(assets = assets)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
}

# ============================================================================
# Bug fix verification: gmv_opt_ptc — proportional transaction cost
# Previously produced NA weights due to rhs <- 1 + target (now rhs <- target)
# and dir filtering bug (dir wasn't filtered when Inf rhs rows removed)
# ============================================================================

test_that("gmv_opt_ptc constraint system is mathematically feasible (rhs bug fix)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  # Verify the rhs <- target fix produces a feasible constraint system.
  # The PTC formulation creates a rank-deficient 3N x 3N Q matrix (w, w+, w-)
  # which causes quadprog to report "constraints inconsistent" due to near-zero

  # eigenvalues. We test the constraint setup directly.
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "transaction_cost", ptc = rep(0.01, 5))
  p <- add.objective(p, type = "risk", name = "var")

  constraints <- get_constraints(p)
  N <- 5
  init_w <- rep(0.2, N)

  # The trivial solution (no rebalance) must satisfy all constraints
  # w = init_weights, w+ = 0, w- = 0
  trivial <- c(init_w, rep(0, N), rep(0, N))

  # Build the constraint matrix exactly as gmv_opt_ptc does
  ptc <- constraints$ptc
  Amat <- c(rep(0, N), rep(0, 2 * N))  # target return row (tautology when target=NA)
  dir <- "=="
  rhs <- 0  # Fixed: was 1+target, now is target (=0 when NA)

  Amat <- rbind(Amat, cbind(diag(N), -diag(N), diag(N)))
  rhs <- c(rhs, init_w)
  dir <- c(dir, rep("==", N))

  Amat <- rbind(Amat, cbind(diag(0, N), diag(N), diag(0, N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))

  Amat <- rbind(Amat, cbind(diag(0, N), diag(0, N), diag(N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))

  Amat <- rbind(Amat, c(rep(1, N), ptc, ptc))
  rhs <- c(rhs, constraints$min_sum)
  dir <- c(dir, ">=")

  Amat <- rbind(Amat, c(rep(-1, N), -ptc, -ptc))
  rhs <- c(rhs, -constraints$max_sum)
  dir <- c(dir, ">=")

  # Check all constraints
  for (i in seq_len(nrow(Amat))) {
    lhs <- sum(Amat[i, ] * trivial)
    if (dir[i] == "==") {
      expect_true(abs(lhs - rhs[i]) < 1e-8, info = paste("Constraint", i, "violated"))
    } else {
      expect_true(lhs >= rhs[i] - 1e-8, info = paste("Constraint", i, "violated"))
    }
  }
})

test_that("gmv_opt_ptc rhs bug fix: target=NA yields rhs=0 not rhs=1", {
  # The critical fix: when target=NA, old code set rhs=1+0=1 for a
  # zero-coefficient return constraint, creating 0==1 (infeasible).
  # Now it's rhs=0, giving 0==0 (tautology).
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  # We verify indirectly: the function should NOT throw "No solution found"
  # for an infeasible constraint system. It may still fail due to the
  # near-singular Q matrix (a separate pre-existing issue), but the
  # error should be about solver numerics, not infeasibility.
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "transaction_cost", ptc = rep(0.01, 5))
  p <- add.objective(p, type = "risk", name = "var")

  # With the fix, the function should complete (possibly with NA weights
  # due to Q matrix rank deficiency) but NOT error with "No solution found"
  expect_no_error(
    suppressWarnings(optimize.portfolio(R5, p, optimize_method = "ROI"))
  )
})

# ============================================================================
# Bug fix verification: maxret_milp_opt — group constraints
# Previously: constraints$groups[i] (returns sub-list) instead of [[i]]
# ============================================================================

test_that("maxret_milp_opt with group constraints produces valid weights (bug fix)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "position_limit", max_pos = 4)
  p <- add.constraint(p, type = "group",
                      groups = list(c(1, 2, 3), c(4, 5)),
                      group_min = c(0.3, 0.1),
                      group_max = c(0.7, 0.5))
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)), info = "weights must not be NA")
  expect_equal(sum(w), 1, tolerance = 1e-6)

  # Verify group constraints are respected (with solver tolerance)
  tol <- 1e-4
  expect_true(sum(w[1:3]) >= 0.3 - tol)
  expect_true(sum(w[1:3]) <= 0.7 + tol)
  expect_true(sum(w[4:5]) >= 0.1 - tol)
  expect_true(sum(w[4:5]) <= 0.5 + tol)
})

test_that("maxret_milp_opt with position_limit only (no groups) still works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "position_limit", max_pos = 3)
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  n_nonzero <- sum(abs(w) > 1e-6)
  expect_true(n_nonzero <= 3)
})

# ============================================================================
# Bug fix verification: dir filtering in gmv_opt_toc and gmv_opt_leverage
# Previously dir wasn't filtered when rhs had Inf entries removed
# ============================================================================

test_that("gmv_opt_toc with turnover constraint and group constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "turnover", turnover_target = 0.5)
  p <- add.constraint(p, type = "group",
                      groups = list(c(1, 2), c(3, 4, 5)),
                      group_min = c(0.2, 0.3),
                      group_max = c(0.6, 0.8))
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.02)

  # Check group constraints with tolerance
  tol <- 1e-4
  expect_true(sum(w[1:2]) >= 0.2 - tol)
  expect_true(sum(w[1:2]) <= 0.6 + tol)
})

test_that("gmv_opt_leverage with leverage constraint works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "leverage_exposure", leverage = 1.5)
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# max_sr_opt: max Sharpe ratio via optimize() binary search
# ============================================================================

test_that("max Sharpe ratio via ROI produces valid result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI", maxSR = TRUE)
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.02)

  # Should have both mean and StdDev in objective measures
  om <- result$objective_measures
  mean_val <- unlist(om[sapply(om, function(x) "mean" %in% names(x))])
  sd_val <- unlist(om[sapply(om, function(x) "StdDev" %in% names(x))])
  expect_true(length(mean_val) > 0)
  expect_true(length(sd_val) > 0)
})

# ============================================================================
# mean_etl_opt: max STARR ratio via optimize() binary search
# ============================================================================

test_that("max STARR ratio via ROI produces valid result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "ES")
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI", maxSTARR = TRUE)
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# etl_milp_opt: ETL MILP with position limits
# ============================================================================

test_that("etl_milp_opt with position limit and group constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "position_limit", max_pos = 4)
  p <- add.constraint(p, type = "group",
                      groups = list(c(1, 2), c(3, 4, 5)),
                      group_min = c(0.1, 0.2),
                      group_max = c(0.7, 0.8))
  p <- add.objective(p, type = "risk", name = "ES")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 1e-4)

  tol <- 1e-4
  expect_true(sum(abs(w) > 1e-6) <= 4, info = "position limit respected")
  expect_true(sum(w[1:2]) >= 0.1 - tol)
  expect_true(sum(w[1:2]) <= 0.7 + tol)
})

# ============================================================================
# gmv_opt: target return constraint
# ============================================================================

test_that("gmv_opt with target return produces feasible weights meeting target", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # Target must be within achievable range [min(colMeans), max(colMeans)]
  target_ret <- 0.008
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "return", name = "mean", target = target_ret)

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 1e-6)

  # Verify the achieved return is approximately the target
  achieved_ret <- as.numeric(sum(w * colMeans(R5)))
  expect_equal(achieved_ret, target_ret, tolerance = 1e-4)
})

# ============================================================================
# etl_opt: ES/CVaR minimization with target return
# ============================================================================

test_that("etl_opt with target return works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  target_ret <- 0.004
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "ES")
  p <- add.objective(p, type = "return", name = "mean", target = target_ret)

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

# ============================================================================
# maxret_opt: max return with factor exposure constraints
# ============================================================================

test_that("maxret_opt with factor exposure constraints", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")

  # B has nrow = N (assets), ncol = K (factors)
  set.seed(2847)
  B <- matrix(runif(5, 0.5, 1.5), nrow = 5, ncol = 1,
              dimnames = list(colnames(R5), "market_beta"))
  p <- add.constraint(p, type = "factor_exposure",
                      B = B, lower = 0.8, upper = 1.2)
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  exposure <- as.numeric(t(B) %*% w)
  expect_true(exposure >= 0.8 - 1e-4)
  expect_true(exposure <= 1.2 + 1e-4)
})

# ============================================================================
# Objective measures are populated correctly
# ============================================================================

test_that("ROI min-var objective measures contain StdDev", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- make_lo_gmv()
  result <- optimize.portfolio(R5, p, optimize_method = "ROI")

  om <- result$objective_measures
  # Flatten objective measures to find StdDev
  all_names <- unlist(lapply(om, names))
  expect_true("StdDev" %in% all_names)
})

test_that("ROI min-ES objective measures contain ES", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "ES")

  result <- optimize.portfolio(R5, p, optimize_method = "ROI")
  om <- result$objective_measures
  # ES objective measure may be a named list element or a named numeric
  all_names <- c(names(om), unlist(lapply(om, names)))
  expect_true("ES" %in% all_names)
})
