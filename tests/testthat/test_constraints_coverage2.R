##### test_constraints_coverage2.R #####
# Phase 3C coverage: constrained_objective.R — remaining branches
#   verbose, NA/NaN warning, median objective, env=NULL moments,
#   disabled objective, calibrate_penalty edge cases


data(edhec, package = "PerformanceAnalytics")
R <- edhec[1:36, 1:4]

# Helper: basic portfolio with StdDev objective
base_portf <- function() {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
}

w_eq <- rep(0.25, 4)

# ===========================================================================
# 1. verbose=TRUE prints debug output
# ===========================================================================

test_that("constrained_objective with verbose=TRUE prints debug info", {
  p <- base_portf()
  env <- list(mu = matrix(colMeans(R), ncol = 1), sigma = cov(R))
  # verbose requires trace=TRUE so tmp_return is initialized
  expect_output(
    constrained_objective(w = w_eq, R = R, portfolio = p,
                          verbose = TRUE, trace = TRUE, env = env),
    "weights"
  )
})

# ===========================================================================
# 2. median objective dispatches correctly
# ===========================================================================

test_that("median objective computes in constrained_objective", {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "return", name = "median")
  env <- list(mu = matrix(colMeans(R), ncol = 1), sigma = cov(R))
  val <- constrained_objective(w = w_eq, R = R, portfolio = p,
                               env = env, trace = TRUE)
  expect_true(is.list(val))
  expect_true("median" %in% names(val$objective_measures))
})

# ===========================================================================
# 3. env=NULL triggers internal moment computation
# ===========================================================================

test_that("constrained_objective with env=NULL computes moments internally", {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "return", name = "mean")
  # env=NULL forces set.portfolio.moments to be called internally
  val <- constrained_objective(w = w_eq, R = R, portfolio = p,
                               env = NULL)
  expect_true(is.finite(val))
})

# ===========================================================================
# 4. disabled objective is skipped
# ===========================================================================

test_that("disabled objective is skipped in constrained_objective", {
  p <- base_portf()
  p$objectives[[1]]$enabled <- FALSE
  env <- list(mu = matrix(colMeans(R), ncol = 1), sigma = cov(R))
  val_disabled <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                        env = env)
  # Re-enable and compare
  p$objectives[[1]]$enabled <- TRUE
  val_enabled <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                       env = env)
  # Disabled should be 0 (no objectives contribute), enabled should be > 0
  expect_equal(val_disabled, 0)
  expect_true(val_enabled > 0)
})

# ===========================================================================
# 5. calibrate_penalty returns 1e4 when pilot random portfolios fail
# ===========================================================================

test_that("calibrate_penalty returns default 1e4 on rp failure", {
  # A 1-asset portfolio can't generate diverse pilot portfolios
  p1 <- portfolio.spec(assets = "A")
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")
  R1 <- R[, 1, drop = FALSE]
  penalty <- calibrate_penalty(R = R1, portfolio = p1, n_pilot = 2)
  expect_equal(penalty, 1e4)
})

# ===========================================================================
# 6. calibrate_penalty with obj_scale=0 returns 1e4
# ===========================================================================

test_that("calibrate_penalty returns 1e4 when objective values are all zero", {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  # No objectives -> constrained_objective returns 0 for all pilots
  penalty <- calibrate_penalty(R = R, portfolio = p)
  expect_equal(penalty, 1e4)
})

# ===========================================================================
# 7. risk_budget_objective target + prisk combined path
# ===========================================================================

test_that("risk_budget_objective with target and min/max_prisk", {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "risk_budget", name = "StdDev",
                     target = 0.001,
                     min_prisk = rep(0.10, 4),
                     max_prisk = rep(0.40, 4))
  env <- list(mu = matrix(colMeans(R), ncol = 1), sigma = cov(R))
  val <- constrained_objective(w = w_eq, R = R, portfolio = p,
                               trace = TRUE, env = env)
  expect_true(is.list(val))
  expect_true(val$out > 0)
})

# ===========================================================================
# 8. minmax_objective below min path
# ===========================================================================

test_that("minmax_objective penalizes when below min", {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  # add.objective doesn't support type="minmax", so construct manually
  p <- add.objective(p, type = "risk", name = "StdDev")
  p$objectives[[1]] <- structure(
    c(p$objectives[[1]], list(min = 0.50, max = 0.90)),
    class = c("minmax_objective", "objective")
  )
  env <- list(mu = matrix(colMeans(R), ncol = 1), sigma = cov(R))
  val <- constrained_objective(w = w_eq, R = R, portfolio = p,
                               env = env, penalty = 1e4)
  # StdDev ~0.02 < min=0.50, so penalty should fire
  expect_true(val > 0)
})

# ===========================================================================
# 9. VaR/ES risk_budget_objective sets portfolio_method='single'
# ===========================================================================

test_that("ES risk objective dispatches correctly", {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "risk", name = "ES")
  # ES with modified method needs m3/m4
  env <- list(
    mu = matrix(colMeans(R), ncol = 1),
    sigma = cov(R),
    m3 = PerformanceAnalytics::M3.MM(R),
    m4 = PerformanceAnalytics::M4.MM(R)
  )
  val <- constrained_objective(w = w_eq, R = R, portfolio = p,
                               trace = TRUE, env = env)
  expect_true(is.list(val))
  expect_true("ES" %in% names(val$objective_measures))
})
