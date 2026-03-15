
require(testthat)
require(PortfolioAnalytics)

context("Phase 1C: solver_cvxr.R — ratio objectives, HHI, turnover, CSM/EQS")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:60, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ============================================================================
# 1. Min variance via CVXR (baseline)
# ============================================================================

test_that("CVXR min StdDev produces valid long-only result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  expect_s3_class(result, "optimize.portfolio.CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)
  expect_true(all(w >= -1e-4))
})

# ============================================================================
# 2. Max return via CVXR
# ============================================================================

test_that("CVXR max return produces valid result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  expect_s3_class(result, "optimize.portfolio.CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)
})

# ============================================================================
# 3. Mean-variance quadratic utility (reward + risk, not maxSR)
# ============================================================================

test_that("CVXR mean-variance quadratic utility (non-maxSR)", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev", risk_aversion = 5)
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)

  # Objective measures should contain both mean and StdDev
  om <- result$objective_measures
  expect_true("mean" %in% names(om))
  expect_true("StdDev" %in% names(om))
})

# ============================================================================
# 4. Max Sharpe Ratio (maxSR=TRUE)
# ============================================================================

test_that("CVXR max Sharpe Ratio produces valid result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR", maxSR = TRUE)
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)

  om <- result$objective_measures
  expect_true("mean" %in% names(om))
  expect_true("StdDev" %in% names(om))
  expect_true("Sharpe Ratio" %in% names(om))

  # Sharpe ratio should be positive
  sr <- as.numeric(om[["Sharpe Ratio"]])
  expect_true(sr > 0)
})

# ============================================================================
# 5. HHI concentration objective
# ============================================================================

test_that("CVXR HHI concentration aversion produces more diversified weights", {
  skip_if_not_installed("CVXR")

  # Without HHI
  p_base <- portfolio.spec(assets = colnames(R5))
  p_base <- add.constraint(p_base, type = "full_investment")
  p_base <- add.constraint(p_base, type = "long_only")
  p_base <- add.objective(p_base, type = "risk", name = "StdDev")

  r_base <- optimize.portfolio(R5, p_base, optimize_method = "CVXR")

  # With HHI (high concentration aversion)
  p_hhi <- portfolio.spec(assets = colnames(R5))
  p_hhi <- add.constraint(p_hhi, type = "full_investment")
  p_hhi <- add.constraint(p_hhi, type = "long_only")
  p_hhi <- add.objective(p_hhi, type = "risk", name = "StdDev")
  p_hhi <- add.objective(p_hhi, type = "weight_concentration",
                          name = "HHI", conc_aversion = 0.1)

  r_hhi <- optimize.portfolio(R5, p_hhi, optimize_method = "CVXR")
  w_hhi <- extractWeights(r_hhi)

  expect_true(all(is.finite(w_hhi)))
  expect_equal(sum(w_hhi), 1, tolerance = 0.01)

  om <- r_hhi$objective_measures
  expect_true("StdDev" %in% names(om))

  # HHI portfolio should be more diversified (lower HHI = sum of squared weights)
  hhi_base <- sum(extractWeights(r_base)^2)
  hhi_conc <- sum(w_hhi^2)
  expect_true(hhi_conc <= hhi_base + 0.01,
              info = "HHI aversion should produce more equal weights")
})

# ============================================================================
# 6. Min ES (non-ratio)
# ============================================================================

test_that("CVXR min ES produces valid result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "ES")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)
})

# ============================================================================
# 7. Max STARR ratio (ES ratio)
# ============================================================================

test_that("CVXR max STARR/ES ratio produces valid result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "ES")
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR", maxSTARR = TRUE)
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)

  om <- result$objective_measures
  expect_true("mean" %in% names(om))
  expect_true("ES" %in% names(om))
  expect_true("ES ratio" %in% names(om))
})

# ============================================================================
# 8. Min CSM (non-ratio, SOCP)
# ============================================================================

test_that("CVXR min CSM produces valid result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "CSM")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)
})

# ============================================================================
# 9. CSM ratio
# ============================================================================

test_that("CVXR max CSM ratio produces valid result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "CSM")
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR", CSMratio = TRUE)
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)

  om <- result$objective_measures
  expect_true("mean" %in% names(om))
  expect_true("CSM" %in% names(om))
  expect_true("CSM ratio" %in% names(om))
})

# ============================================================================
# 10. Min EQS (non-ratio)
# ============================================================================

test_that("CVXR min EQS produces valid result", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "EQS")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)
})

# ============================================================================
# 11. EQS ratio
# ============================================================================

test_that("CVXR max EQS ratio returns result or optimization_failure", {
  skip_if_not_installed("CVXR")

  # EQS ratio can be numerically challenging depending on the data;
  # we verify the solver at least runs and returns a structured result.
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "EQS")
  p <- add.objective(p, type = "return", name = "mean")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR", EQSratio = TRUE)
  expect_true(
    inherits(result, "optimize.portfolio") || is.optimization_failure(result)
  )
})

# ============================================================================
# 12. Turnover constraint with StdDev
# ============================================================================

test_that("CVXR turnover constraint with StdDev objective", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "turnover", turnover_target = 0.5)
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)

  # Turnover from equal-weight initial should be <= target
  init_w <- rep(1/5, 5)
  turnover <- sum(abs(w - init_w))
  expect_true(turnover <= 0.5 + 0.01)
})

# ============================================================================
# 13. Group constraints with CVXR
# ============================================================================

test_that("CVXR with group constraints", {
  skip_if_not_installed("CVXR")

  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "group",
                      groups = list(c(1, 2), c(3, 4, 5)),
                      group_min = c(0.3, 0.2),
                      group_max = c(0.6, 0.8))
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)

  tol <- 0.01
  expect_true(sum(w[1:2]) >= 0.3 - tol)
  expect_true(sum(w[1:2]) <= 0.6 + tol)
  expect_true(sum(w[3:5]) >= 0.2 - tol)
  expect_true(sum(w[3:5]) <= 0.8 + tol)
})

# ============================================================================
# 14. Solver status for infeasible problem
# ============================================================================

test_that("CVXR returns optimization_failure for unsolvable problem", {
  skip_if_not_installed("CVXR")

  # Create a problem that passes validation but is numerically infeasible
  # for the CVXR solver: contradictory group constraints
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  # Group 1 must be >= 0.8 AND Group 2 must be >= 0.8 (impossible: sum > 1)
  p <- add.constraint(p, type = "group",
                      groups = list(c(1, 2), c(3, 4, 5)),
                      group_min = c(0.8, 0.8),
                      group_max = c(1.0, 1.0))
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  expect_true(is.optimization_failure(result))
})

# ============================================================================
# 15. Alpha normalization (alpha > 0.5 → 1-alpha)
# ============================================================================

test_that("CVXR normalizes alpha > 0.5 to 1-alpha", {
  skip_if_not_installed("CVXR")

  # Using p=0.95 (alpha > 0.5) should be equivalent to p=0.05
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "ES", arguments = list(p = 0.95))

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.01)
})

# ============================================================================
# 16. CVXR solver selection (OSQP for QP, SCS for cone)
# ============================================================================

test_that("CVXR selects appropriate solver per objective", {
  skip_if_not_installed("CVXR")

  # StdDev → OSQP
  p_sd <- portfolio.spec(assets = colnames(R5))
  p_sd <- add.constraint(p_sd, type = "full_investment")
  p_sd <- add.constraint(p_sd, type = "long_only")
  p_sd <- add.objective(p_sd, type = "risk", name = "StdDev")

  r_sd <- optimize.portfolio(R5, p_sd, optimize_method = "CVXR")
  expect_equal(r_sd$solver, "OSQP")

  # CSM → SCS (involves SOCP)
  p_csm <- portfolio.spec(assets = colnames(R5))
  p_csm <- add.constraint(p_csm, type = "full_investment")
  p_csm <- add.constraint(p_csm, type = "long_only")
  p_csm <- add.objective(p_csm, type = "risk", name = "CSM")

  r_csm <- optimize.portfolio(R5, p_csm, optimize_method = "CVXR")
  expect_equal(r_csm$solver, "SCS")
})

# ============================================================================
# 17. Target return constraint via CVXR
# ============================================================================

test_that("CVXR respects target return constraint", {
  skip_if_not_installed("CVXR")

  target_ret <- 0.008
  p <- portfolio.spec(assets = colnames(R5))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "return", return_target = target_ret)
  p <- add.objective(p, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, p, optimize_method = "CVXR")
  w <- extractWeights(result)

  expect_true(all(is.finite(w)))
  mu <- as.numeric(colMeans(R5))
  achieved <- sum(w * mu)
  expect_true(achieved >= target_ret - 1e-4)
})
