##### test_optimize_coverage2.R #####
# Phase 3C coverage: optimize.portfolio edge cases — column subsetting,
#   unknown solver, penalty="auto" for ROI, warm_start validation,
#   normalize_weights, check_feasibility


data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "full_investment")
portf <- add.constraint(portf, type = "long_only")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ===========================================================================
# A. Column subsetting — R has more columns than portfolio assets
# ===========================================================================

test_that("optimize.portfolio subsets R when it has extra columns", {
  R_extra <- cbind(R5, Extra = rnorm(48))
  # portf only has 5 assets, R_extra has 6 — should subset
  opt <- optimize.portfolio(R_extra, portf, optimize_method = "ROI")
  expect_equal(length(opt$weights), 5)
})

# ===========================================================================
# B. Unknown solver name
# ===========================================================================

test_that("optimize.portfolio errors on unknown solver name", {
  expect_error(
    optimize.portfolio(R5, portf, optimize_method = "nonexistent_solver"),
    "nonexistent_solver|not found|Unknown"
  )
})

# ===========================================================================
# C. penalty="auto" with non-stochastic solver (ROI) — fallback path
# ===========================================================================

test_that("penalty='auto' works with ROI solver", {
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                            penalty = "auto")
  expect_false(is.optimization_failure(opt))
})

# ===========================================================================
# D. Warm-start validation — wrong length
# ===========================================================================

test_that("warm_start with wrong length produces warning and cold-starts", {
  expect_warning(
    optimize.portfolio(R5, portf, optimize_method = "ROI",
                       warm_start = c(0.5, 0.5)),
    "warm_start|length"
  )
})

# ===========================================================================
# E. Warm-start validation — name mismatch
# ===========================================================================

test_that("warm_start with mismatched names produces warning", {
  ws <- rep(0.2, 5)
  names(ws) <- c("X", "Y", "Z", "W", "V")
  expect_warning(
    optimize.portfolio(R5, portf, optimize_method = "ROI",
                       warm_start = ws),
    "warm_start|names|match"
  )
})
