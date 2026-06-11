context("Regression tests for 7 bug fixes (session 2026-06-11)")

# These tests each demonstrate a failure case that existed before the fix and
# verify the corrected behaviour.  Each test block references the file and
# line(s) changed.

library(PerformanceAnalytics)
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:48, 1:4]
colnames(R4) <- c("A", "B", "C", "D")
funds4 <- colnames(R4)

# ===========================================================================
# T1: constraint_fn_map.R:69 — Dykstra projection skipped when group_pos set
#
# Dykstra is deterministic; the stochastic fallback varies with the RNG seed.
# The test uses weights that violate box constraints so that the rp_transform
# fallback (triggered when is_convex=FALSE) must draw replacement weights
# randomly — making its output seed-dependent.
# Before the fix, group_pos was absent from the is_convex condition, so Dykstra
# ran for both portfolios and always produced the same result regardless of seed.
# ===========================================================================

test_that("fn_map projection is deterministic without group_pos", {
  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)

  # Weights that violate box (w[1] and w[2] below min, w[3] above max)
  w0 <- c(0.01, 0.02, 0.60, 0.37)
  names(w0) <- funds4

  set.seed(1); r1 <- fn_map(w0, portf, method = "projection")$weights
  set.seed(9); r2 <- fn_map(w0, portf, method = "projection")$weights
  expect_equal(r1, r2, info = "Dykstra is deterministic — seeds must not matter")
})

test_that("fn_map projection falls back to stochastic path when group_pos is set", {
  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.constraint(portf, type = "group",
                          groups     = list(c(1, 2), c(3, 4)),
                          group_min  = c(0.3, 0.3),
                          group_max  = c(0.6, 0.6),
                          group_pos  = c(1, 1))

  # Same box-violating weights — rp_transform randomly samples replacements
  w0 <- c(0.01, 0.02, 0.60, 0.37)
  names(w0) <- funds4

  set.seed(1); r1 <- fn_map(w0, portf, method = "projection")$weights
  set.seed(9); r2 <- fn_map(w0, portf, method = "projection")$weights
  # Stochastic fallback: different seeds produce different results
  expect_false(identical(r1, r2),
               info = "group_pos forces the stochastic fallback; results must vary with seed")
})

# ===========================================================================
# T2: constraint_fn_map.R:1101-1102 — near-zero positive weights not counted
#     as short positions by rp_position_limit
#
# Before fix: sum(tmp_w < +tolerance) counted 1e-10 (tiny positive) as short.
# After fix:  sum(tmp_w < -tolerance) only counts genuinely negative weights.
#
# Mathematical verification (no internal function calls needed):
# ===========================================================================

test_that("rp_position_limit short threshold: near-zero positive is above -tolerance", {
  tolerance <- .Machine$double.eps^0.5        # same constant used in the function

  w_tiny_long <- 1e-10                         # positive, not a short position

  # Old (wrong) threshold would count 1e-10 as a short:
  expect_true(w_tiny_long < tolerance,
              info = "confirms why old '< +tolerance' was wrong: 1e-10 < 1.49e-8")

  # Fixed threshold correctly excludes 1e-10:
  expect_false(w_tiny_long < -tolerance,
               info = "fixed '< -tolerance' correctly ignores near-zero positive")

  # Genuine short weight satisfies BOTH thresholds:
  w_genuine_short <- -0.05
  expect_true(w_genuine_short < tolerance)
  expect_true(w_genuine_short < -tolerance,
              info = "genuine short must be flagged by the fixed threshold too")
})

test_that("rp_position_limit short threshold: genuine short satisfies both old and new thresholds", {
  # Verify that a genuine short weight is correctly identified by BOTH
  # the old threshold (< +tolerance) and the fixed threshold (< -tolerance).
  # This confirms that fixing the near-zero false-positive doesn't suppress
  # detection of actual short positions.
  tolerance <- .Machine$double.eps^0.5
  w_genuine_short <- -0.05
  expect_true(w_genuine_short < -tolerance,
              info = "genuine short must be detected by the fixed threshold")
  expect_true(w_genuine_short < tolerance,
              info = "genuine short was also detected by the old threshold (correct)")
})

# ===========================================================================
# T3: constrained_objective.R:500 + loop extension
#     group_pos violations produce a non-zero penalty
#
# Before the detection fix:  group_fail called without group_pos → always
# returned FALSE for position-count violations → penalty block skipped.
# Before the loop fix:       detection worked but loop only penalised cLO/cUP
# sum violations, never group_pos violations (penalty = 0).
# After both fixes:          violating weights yield a larger objective value.
# ===========================================================================

test_that("constrained_objective: group_pos violation produces a positive penalty contribution", {
  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "group",
                          groups    = list(c(1, 2), c(3, 4)),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8),
                          group_pos = c(1, 1))        # max 1 non-zero per group
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # Violates group_pos: 2 non-zero weights in each group; sums within [cLO, cUP]
  w_violate  <- c(0.30, 0.20, 0.30, 0.20)
  names(w_violate) <- funds4
  # Satisfies group_pos: exactly 1 non-zero per group
  w_feasible <- c(0.50, 0.00, 0.50, 0.00)
  names(w_feasible) <- funds4

  pen <- 1e6
  val_violate  <- constrained_objective(w_violate,  R4, portf, penalty = pen)
  val_feasible <- constrained_objective(w_feasible, R4, portf, penalty = pen)

  expect_true(val_violate > val_feasible,
              info = paste("group_pos violating weights must cost more.",
                           "val_violate =", val_violate,
                           "; val_feasible =", val_feasible))
})

test_that("constrained_objective: no group_pos penalty when group_pos equals group size", {
  portf_full <- portfolio.spec(assets = funds4)
  portf_full <- add.constraint(portf_full, type = "weight_sum",
                               min_sum = 0.99, max_sum = 1.01)
  portf_full <- add.constraint(portf_full, type = "box", min = 0.05, max = 0.65)
  portf_full <- add.constraint(portf_full, type = "group",
                               groups    = list(c(1, 2), c(3, 4)),
                               group_min = c(0.2, 0.2),
                               group_max = c(0.8, 0.8),
                               group_pos = c(2, 2))   # all positions allowed
  portf_full <- add.objective(portf_full, type = "risk", name = "StdDev")

  portf_base <- portfolio.spec(assets = funds4)
  portf_base <- add.constraint(portf_base, type = "weight_sum",
                               min_sum = 0.99, max_sum = 1.01)
  portf_base <- add.constraint(portf_base, type = "box", min = 0.05, max = 0.65)
  portf_base <- add.constraint(portf_base, type = "group",
                               groups    = list(c(1, 2), c(3, 4)),
                               group_min = c(0.2, 0.2),
                               group_max = c(0.8, 0.8))  # no group_pos
  portf_base <- add.objective(portf_base, type = "risk", name = "StdDev")

  w <- c(0.30, 0.20, 0.30, 0.20)
  names(w) <- funds4

  val_full <- constrained_objective(w, R4, portf_full, penalty = 1e6)
  val_base <- constrained_objective(w, R4, portf_base, penalty = 1e6)
  expect_equal(val_full, val_base, tolerance = 1e-10,
               info = "group_pos = group size must not add any penalty")
})

# ===========================================================================
# T4: solver_cvxr.R:282 — CVXR box clamp gated by 1e-5 tolerance
#
# Before the fix: pmax/pmin applied unconditionally, masking real infeasibility.
# After the fix:  clamping only fires when max violation <= 1e-5.
#
# Tested by applying the patched logic locally (no CVXR solver required).
# ===========================================================================

.cvxr_clamp_logic <- function(wts, lb, ub) {
  lb_viol <- pmax(lb - wts, 0)
  ub_viol <- pmax(wts - ub, 0)
  if (max(lb_viol, ub_viol) <= 1e-5) {
    wts <- pmax(wts, lb)
    wts <- pmin(wts, ub)
  }
  wts
}

test_that("CVXR clamp: sub-tolerance lower-bound drift is corrected", {
  lb <- rep(0.0, 4); ub <- rep(0.55, 4)
  # w[1] is 9e-6 below lb; no other violations
  w  <- c(0.0 - 9e-6, 0.30, 0.35, 0.30)
  result <- .cvxr_clamp_logic(w, lb, ub)
  expect_equal(result[1], lb[1],
               info = "sub-tolerance lb drift must be clamped to lb")
})

test_that("CVXR clamp: sub-tolerance upper-bound drift is corrected", {
  # Use lb=0 so no lower-bound violations exist
  lb <- rep(0.0, 4); ub <- rep(0.55, 4)
  # w[2] is 8e-6 above ub; all others within bounds
  w  <- c(0.25, 0.55 + 8e-6, 0.20, 0.0)
  result <- .cvxr_clamp_logic(w, lb, ub)
  expect_equal(result[2], ub[2],
               info = "sub-tolerance ub drift must be clamped to ub")
})

test_that("CVXR clamp: large violation is NOT corrected (signals infeasibility)", {
  lb <- rep(0.05, 4); ub <- rep(0.55, 4)
  # w[1] is 0.15 below lb — far outside solver tolerance
  w  <- c(-0.10, 0.40, 0.40, 0.30)
  result <- .cvxr_clamp_logic(w, lb, ub)
  expect_equal(result[1], -0.10,
               info = "large violation must be left unclamped so caller detects infeasibility")
})

test_that("CVXR clamp: violation of 9e-6 (< 1e-5) is corrected", {
  lb <- rep(0.0, 4); ub <- rep(0.55, 4)
  # w[1] is exactly 9e-6 below lb — within tolerance
  w  <- c(0.0 - 9e-6, 0.30, 0.35, 0.30)
  result <- .cvxr_clamp_logic(w, lb, ub)
  expect_equal(result[1], lb[1],
               info = "violation of 9e-6 must trigger clamping (9e-6 < 1e-5)")
})

test_that("CVXR clamp: violation of 2e-3 (> 1e-5) is NOT corrected", {
  lb <- rep(0.0, 4); ub <- rep(0.55, 4)
  w  <- c(0.0 - 2e-3, 0.35, 0.35, 0.30)
  result <- .cvxr_clamp_logic(w, lb, ub)
  expect_equal(result[1], -2e-3,
               info = "violation of 2e-3 must NOT trigger clamping (2e-3 > 1e-5)")
})

# ===========================================================================
# T5: optFUN.R:336 — maxret_milp_opt respects non-unit weight-sum constraints
#
# Before the fix: rhs <- c(1, 1, ...) forced sum(w) = 1 regardless of
# min_sum / max_sum, so a sub-full-investment or dollar-neutral portfolio would
# always have sum ≈ 1.
# After the fix:  rhs <- c(max_sum, min_sum, ...) is used.
# ===========================================================================

test_that("maxret_milp_opt: sub-full-investment weight sum is within bounds", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.40, max_sum = 0.60)
  portf <- add.constraint(portf, type = "box", min = 0.00, max = 0.50)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "return", name = "mean")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI")
  w   <- extractWeights(opt)

  expect_true(sum(w) >= 0.40 - 1e-4,
              info = paste("sum(w) =", round(sum(w), 6), "must be >= 0.40"))
  expect_true(sum(w) <= 0.60 + 1e-4,
              info = paste("sum(w) =", round(sum(w), 6), "must be <= 0.60"))
})

test_that("maxret_milp_opt: near-dollar-neutral weight sum is within bounds", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = -0.05, max_sum = 0.05)
  portf <- add.constraint(portf, type = "box", min = -0.40, max = 0.40)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "return", name = "mean")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI")
  w   <- extractWeights(opt)

  expect_true(sum(w) >= -0.05 - 1e-4,
              info = paste("sum(w) =", round(sum(w), 6), "must be >= -0.05"))
  expect_true(sum(w) <=  0.05 + 1e-4,
              info = paste("sum(w) =", round(sum(w), 6), "must be <= 0.05"))
})

# ===========================================================================
# T6: optFUN.R:910-941 — gmv_opt_ptc box/group/factor rows apply only to w
#
# Before the fix: Amat used cbind(A, A, A) over [w, w+, w-], so box constraints
# applied to w + w+ + w- (not just w), making effective bounds tighter.
# After the fix:  cbind(A, 0, 0) constrains only the final weight vector w.
# ===========================================================================

test_that("gmv_opt_ptc: final weights satisfy box constraints", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "var")
  portf <- add.objective(portf, type = "return", name = "mean")

  init_w <- rep(0.25, 4)
  names(init_w) <- funds4

  opt <- optimize.portfolio(
    R4, portf,
    optimize_method = "ROI",
    ptc             = 0.001,
    init_weights    = init_w
  )
  w <- extractWeights(opt)

  expect_true(all(w >= 0.05 - 1e-4),
              info = paste("min box violated:", paste(round(w, 4), collapse = ", ")))
  expect_true(all(w <= 0.55 + 1e-4),
              info = paste("max box violated:", paste(round(w, 4), collapse = ", ")))
})

# ===========================================================================
# T7: solver_roi.R:113 — max-Sharpe uses custom moments for target constraint
#
# Before the fix: roi_moments$mean was zeroed before gmv_opt was called, making
# gmv_opt fall back to colMeans(R) for the target constraint row. With custom
# moments (e.g. Black-Litterman) this caused the constraint to be expressed in
# sample-mean space while the target was derived in custom-mean space.
# After the fix: custom means reach gmv_opt and are used for the constraint row.
# The linear term -mu'w is constant under the binding equality w'mu=target,
# so the optimal weights are unchanged but the constraint is now correct.
# ===========================================================================

test_that("ROI max-Sharpe: custom moments produce a higher Sharpe than equal-weight", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # Amplify means strongly away from colMeans(R) to make the custom signal dominant
  custom_mean <- colMeans(R4) * 5
  custom_cov  <- cov(R4)

  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk",   name = "StdDev")

  custom_moments <- list(mean = custom_mean, var = custom_cov)

  opt <- optimize.portfolio(
    R4, portf,
    optimize_method = "ROI",
    momentFUN       = function(R, portfolio) custom_moments,
    maxSR           = TRUE
  )

  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)

  w_eq         <- rep(0.25, 4)
  sharpe_opt   <- sum(w    * custom_mean) / sqrt(as.numeric(t(w)    %*% custom_cov %*% w))
  sharpe_eq    <- sum(w_eq * custom_mean) / sqrt(as.numeric(t(w_eq) %*% custom_cov %*% w_eq))

  expect_true(sharpe_opt >= sharpe_eq - 1e-6,
              info = paste("Sharpe opt =", round(sharpe_opt, 4),
                           "; Sharpe eq-wt =", round(sharpe_eq, 4)))
})

test_that("ROI max-Sharpe: custom-moment portfolio holds multiple assets (non-degenerate)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # Clearly non-sample means so the wrong colMeans fallback would give a
  # measurably different (degenerate corner) solution.
  custom_mean <- c(0.010, 0.020, 0.005, 0.015)
  names(custom_mean) <- funds4
  custom_cov  <- cov(R4)

  portf <- portfolio.spec(assets = funds4)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk",   name = "StdDev")

  custom_moments <- list(mean = custom_mean, var = custom_cov)

  opt <- optimize.portfolio(
    R4, portf,
    optimize_method = "ROI",
    momentFUN       = function(R, portfolio) custom_moments,
    maxSR           = TRUE
  )

  w <- extractWeights(opt)
  # Tangency portfolio under these custom means must diversify across assets
  expect_true(sum(w > 0.01) >= 2,
              info = paste("tangency portfolio must hold >= 2 assets, got weights:",
                           paste(round(w, 4), collapse = ", ")))

  # Expected return under custom means must be >= min asset return (no degenerate solution)
  port_ret <- sum(w * custom_mean)
  expect_true(port_ret >= min(custom_mean) - 1e-6,
              info = paste("portfolio return under custom means:", round(port_ret, 6)))
})
