##### test_optFUN_coverage3.R #####
# Phase 4 coverage: optFUN.R — cleanR, mean branch in gmv/toc/ptc,
#   target return, try-error handling

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]

# ===========================================================================
# 1. cleanR path in gmv_opt
# ===========================================================================

test_that("gmv_opt uses moments$cleanR when provided", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "risk", name = "StdDev")

  constraints <- get_constraints(p)
  R_clean <- R4 + 0.001
  moments <- list(
    mean = rep(0, 4),
    var = cov(R4),
    cleanR = R_clean
  )
  result <- gmv_opt(
    R = R4, constraints = constraints, moments = moments,
    lambda = 1, target = NA, lambda_hhi = NULL,
    conc_groups = NULL, solver = "quadprog"
  )
  expect_equal(length(result$weights), 4)
  expect_true(is.finite(result$out))
})

# ===========================================================================
# 2. gmv_opt with non-zero mean (mean branch)
# ===========================================================================

test_that("gmv_opt computes mean+StdDev when moments$mean != 0", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk", name = "StdDev")

  constraints <- get_constraints(p)
  moments <- list(
    mean = colMeans(R4),
    var = cov(R4)
  )
  result <- gmv_opt(
    R = R4, constraints = constraints, moments = moments,
    lambda = 1, target = NA, lambda_hhi = NULL,
    conc_groups = NULL, solver = "quadprog"
  )
  expect_true("mean" %in% names(result$obj_vals))
  expect_true("StdDev" %in% names(result$obj_vals))
})

# ===========================================================================
# 3. gmv_opt_toc with non-zero mean (turnover + mean branch)
# ===========================================================================

test_that("gmv_opt_toc computes mean+StdDev when moments$mean != 0", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.constraint(p, type = "turnover", turnover_target = 0.5)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk", name = "StdDev")

  constraints <- get_constraints(p)
  moments <- list(
    mean = colMeans(R4),
    var = cov(R4)
  )
  init_w <- rep(0.25, 4)
  result <- gmv_opt_toc(
    R = R4, constraints = constraints, moments = moments,
    lambda = 1, target = NA, init_weights = init_w,
    solver = "quadprog"
  )
  expect_true("mean" %in% names(result$obj_vals))
  expect_true("StdDev" %in% names(result$obj_vals))
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 4. gmv_opt_ptc with non-zero mean (PTC + mean branch)
# ===========================================================================

test_that("gmv_opt_ptc computes mean+StdDev when moments$mean != 0", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.constraint(p, type = "transaction_cost", ptc = rep(0.001, 4))
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk", name = "StdDev")

  constraints <- get_constraints(p)
  moments <- list(
    mean = colMeans(R4),
    var = cov(R4)
  )
  init_w <- rep(0.25, 4)
  result <- gmv_opt_ptc(
    R = R4, constraints = constraints, moments = moments,
    lambda = 1, target = NA, init_weights = init_w,
    solver = "quadprog"
  )
  expect_true("mean" %in% names(result$obj_vals))
  expect_true("StdDev" %in% names(result$obj_vals))
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 5. cleanR path in maxret_opt
# ===========================================================================

test_that("maxret_opt uses moments$cleanR when provided", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "return", name = "mean")

  constraints <- get_constraints(p)
  R_clean <- R4 + 0.001
  moments <- list(
    mean = colMeans(R4),
    var = cov(R4),
    cleanR = R_clean
  )
  result <- maxret_opt(
    R = R4, moments = moments, constraints = constraints,
    target = NA, solver = "glpk"
  )
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 6. cleanR path in etl_opt
# ===========================================================================

test_that("etl_opt uses moments$cleanR when provided", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "risk", name = "ES")

  constraints <- get_constraints(p)
  R_clean <- R4 + 0.001
  moments <- list(
    mean = rep(0, 4),
    var = cov(R4),
    cleanR = R_clean,
    ES = NULL
  )
  result <- etl_opt(
    R = R4, constraints = constraints, moments = moments,
    target = NA, alpha = 0.05, solver = "glpk"
  )
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 7. gmv_opt with target return constraint
# ===========================================================================

test_that("gmv_opt respects target return constraint", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.objective(p, type = "risk", name = "StdDev")

  constraints <- get_constraints(p)
  moments <- list(
    mean = colMeans(R4),
    var = cov(R4)
  )
  result <- gmv_opt(
    R = R4, constraints = constraints, moments = moments,
    lambda = 0, target = 0.005, lambda_hhi = NULL,
    conc_groups = NULL, solver = "quadprog"
  )
  expect_equal(length(result$weights), 4)
  # With target, mean should be reported
  expect_true("mean" %in% names(result$obj_vals))
})

# ===========================================================================
# 8. ROI solver try-error on infeasible problem
# ===========================================================================

test_that("gmv_opt returns NA weights on infeasible problem", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p <- portfolio.spec(assets = colnames(R4))
  # Weights must sum to 2, but max per asset is 0.3 -> impossible
  p <- add.constraint(p, type = "weight_sum", min_sum = 2.0, max_sum = 2.0)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.30)
  p <- add.objective(p, type = "risk", name = "StdDev")

  constraints <- get_constraints(p)
  moments <- list(
    mean = rep(0, 4),
    var = cov(R4)
  )
  result <- gmv_opt(
    R = R4, constraints = constraints, moments = moments,
    lambda = 1, target = NA, lambda_hhi = NULL,
    conc_groups = NULL, solver = "quadprog"
  )
  # quadprog signals infeasibility via NA weights (ierr = 1), not an error
  expect_true(all(is.na(result$weights)))
})
