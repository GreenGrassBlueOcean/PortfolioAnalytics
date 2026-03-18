##### test_optFUN_coverage4.R #####
# Coverage: optFUN.R target-return-with-zero-mean paths

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("ROI.plugin.glpk")

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]

p <- portfolio.spec(assets = colnames(R4))
p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
p <- add.objective(p, type = "risk", name = "StdDev")
p <- add.objective(p, type = "return", name = "mean")

constraints <- get_constraints(p)

moments_zero <- list(
  mean = rep(0, 4),
  var = cov(R4)
)

# Use a realistic target based on actual mean returns
target <- mean(colMeans(R4))

# ===========================================================================
# 1. gmv_opt: target + zero mean
# ===========================================================================

test_that("gmv_opt with target and zero moments$mean computes means from R", {
  result <- gmv_opt(
    R = R4, constraints = constraints, moments = moments_zero,
    lambda = 1, target = target, lambda_hhi = NULL,
    conc_groups = NULL, solver = "quadprog"
  )
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 2. etl_opt: target + zero mean
# ===========================================================================

test_that("etl_opt with target and zero moments$mean computes means from R", {
  moments_etl <- list(
    mean = rep(0, 4),
    var = cov(R4),
    ES = NULL
  )
  result <- etl_opt(
    R = R4, constraints = constraints, moments = moments_etl,
    target = target, solver = "glpk", alpha = 0.05
  )
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 3. gmv_opt_toc: target + zero mean
# ===========================================================================

test_that("gmv_opt_toc with target and zero mean", {
  toc_constraints <- constraints
  toc_constraints$turnover_target <- 0.5
  moments_toc <- moments_zero
  moments_toc$pw <- rep(0.25, 4)
  result <- gmv_opt_toc(
    R = R4, constraints = toc_constraints, moments = moments_toc,
    lambda = 1, target = target, init_weights = rep(0.25, 4),
    solver = "quadprog"
  )
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 4. gmv_opt_ptc: target + zero mean
# ===========================================================================

test_that("gmv_opt_ptc with target and zero mean", {
  ptc_constraints <- constraints
  ptc_constraints$ptc <- rep(0.001, 4)
  moments_ptc <- moments_zero
  moments_ptc$pw <- rep(0.25, 4)
  result <- gmv_opt_ptc(
    R = R4, constraints = ptc_constraints, moments = moments_ptc,
    lambda = 1, target = target, init_weights = rep(0.25, 4),
    solver = "quadprog"
  )
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 5. gmv_opt_leverage: target + zero mean
# ===========================================================================

test_that("gmv_opt_leverage with target and zero mean", {
  lev_constraints <- constraints
  lev_constraints$leverage <- 1.6
  result <- gmv_opt_leverage(
    R = R4, constraints = lev_constraints, moments = moments_zero,
    lambda = 1, target = target, solver = "quadprog"
  )
  expect_equal(length(result$weights), 4)
})
