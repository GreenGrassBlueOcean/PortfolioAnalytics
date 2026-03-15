
require(testthat)
require(PortfolioAnalytics)

context("optimize.portfolio.R coverage: advanced code paths")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ============================================================================
# Tests: portfolio.list recursion (lines 780–801)
# ============================================================================

test_that("optimize.portfolio handles portfolio.list with recursive calls", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "long_only")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.constraint(p2, type = "long_only")
  p2 <- add.objective(p2, type = "return", name = "mean")

  plist <- combine.portfolios(list(p1, p2))
  result <- optimize.portfolio(R5, plist, optimize_method = "ROI")

  expect_s3_class(result, "opt.list")
  expect_length(result, 2)
  expect_s3_class(result[[1]], "optimize.portfolio")
  expect_s3_class(result[[2]], "optimize.portfolio")
})

test_that("optimize.portfolio portfolio.list with message=TRUE produces output", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "long_only")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  plist <- combine.portfolios(list(p1, p1))
  out <- capture.output(
    result <- optimize.portfolio(R5, plist, optimize_method = "ROI", message = TRUE)
  )
  expect_true(any(grepl("Starting optimization of portfolio", out)))
})

# ============================================================================
# Tests: regime switching detection (lines 804–818)
# ============================================================================

test_that("optimize.portfolio handles regime.portfolios with matching dates", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "long_only")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.constraint(p2, type = "long_only")
  p2 <- add.objective(p2, type = "return", name = "mean")

  # Regime index at last date of R5 → pick portfolio 2
  last_date <- index(R5)[nrow(R5)]
  regime_obj <- list(
    portfolio.list = list(p1, p2),
    regime = xts::xts(c(2), order.by = last_date)
  )
  class(regime_obj) <- "regime.portfolios"

  result <- optimize.portfolio(R5, regime_obj, optimize_method = "ROI")
  expect_s3_class(result, "optimize.portfolio")
  expect_equal(result$regime, 2)
})

test_that("optimize.portfolio regime.portfolios warns on date mismatch", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "long_only")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  # Regime index at a date NOT in R5
  regime_obj <- list(
    portfolio.list = list(p1, p1),
    regime = xts::xts(c(1), order.by = as.Date("2099-01-01"))
  )
  class(regime_obj) <- "regime.portfolios"

  expect_warning(
    result <- optimize.portfolio(R5, regime_obj, optimize_method = "ROI"),
    "Dates in regime and R do not match"
  )
  expect_equal(result$regime, 1)
})

# ============================================================================
# Tests: penalty calibration (lines 962–970)
# ============================================================================

test_that("penalty='auto' calibrates for stochastic solvers", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  set.seed(8437)
  result <- optimize.portfolio(R5, portf, optimize_method = "random",
                               search_size = 100, penalty = "auto")
  # penalty is stored in the result
  expect_true(is.numeric(result$penalty))
  expect_true(result$penalty > 0)
})

test_that("penalty='auto' defaults to 1e4 for non-stochastic solvers", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                               penalty = "auto")
  expect_equal(result$penalty, 1e4)
})

# ============================================================================
# Tests: warm-start validation (lines 972–984)
# ============================================================================

test_that("warm_start with wrong length triggers warning and is ignored", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  expect_warning(
    result <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                                 warm_start = c(0.5, 0.5)),
    "warm_start ignored"
  )
  expect_s3_class(result, "optimize.portfolio")
})

test_that("warm_start with mismatched names triggers warning", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  ws <- rep(0.2, 5)
  names(ws) <- c("X", "Y", "Z", "W", "V")
  expect_warning(
    result <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                                 warm_start = ws),
    "warm_start ignored.*asset names"
  )
})

# ============================================================================
# Tests: solver dispatch and optimize_method handling (lines 834, 987–1000)
# ============================================================================

test_that("unknown optimize_method raises error", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  expect_error(
    optimize.portfolio(R5, portf, optimize_method = "nonexistent_solver"),
    "Unknown optimize_method"
  )
})

test_that("two-element optimize_method selects the second element", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # Pass two methods; second should be used
  result <- optimize.portfolio(R5, portf, optimize_method = c("ignored", "ROI"))
  expect_s3_class(result, "optimize.portfolio.ROI")
})

# ============================================================================
# Tests: moment function error handling (lines 921–933)
# ============================================================================

test_that("broken momentFUN produces message about failure", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  broken_moment <- function(R, portfolio, ...) {
    stop("intentional moment failure")
  }

  set.seed(5174)
  # The message about failure should be emitted even if downstream errors occur
  expect_message(
    tryCatch(
      optimize.portfolio(R5, portf, optimize_method = "random",
                         search_size = 50, momentFUN = broken_moment),
      error = function(e) NULL
    ),
    "portfolio moment function failed"
  )
})

# ============================================================================
# Tests: constraints and objectives passed separately (lines 847–868)
# ============================================================================

test_that("constraints passed separately are inserted into portfolio", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # Pass constraints separately
  fi_constraint <- weight_sum_constraint(type = "weight_sum",
                                          min_sum = 0.99, max_sum = 1.01,
                                          enabled = TRUE)
  box_constraint <- box_constraint(type = "box", assets = portf$assets,
                                    min = rep(0, 5), max = rep(1, 5),
                                    enabled = TRUE)

  result <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                               constraints = list(fi_constraint, box_constraint))
  expect_s3_class(result, "optimize.portfolio")
  w <- extractWeights(result)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# Tests: check_feasibility flag (lines 1027–1034)
# ============================================================================

test_that("check_feasibility=TRUE attaches feasibility_report", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                               check_feasibility = TRUE)
  expect_true(!is.null(result$feasibility_report))
  expect_true(result$feasibility_report$feasible)
})

test_that("check_feasibility=FALSE omits feasibility_report", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                               check_feasibility = FALSE)
  expect_true(is.null(result$feasibility_report))
})

# ============================================================================
# Tests: data_summary, end_t, elapsed_time fields (lines 1015–1019)
# ============================================================================

test_that("optimize.portfolio attaches metadata fields", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R5, portf, optimize_method = "ROI", trace = TRUE)
  expect_true(!is.null(result$data_summary))
  expect_true(!is.null(result$elapsed_time))
  expect_true(!is.null(result$end_t))
  expect_true(!is.null(result$R))
  expect_true(!is.null(result$portfolio))
})

# ============================================================================
# Tests: optimize.portfolio.rebalancing — warm_start path (lines 1320–1369)
# ============================================================================

test_that("warm_start=TRUE in rebalancing runs sequentially", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  result <- optimize.portfolio.rebalancing(
    R5, portf,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    warm_start = TRUE
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
  expect_true(length(result$opt_rebalancing) > 0)
})

test_that("warm_start=TRUE with rolling_window works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  result <- optimize.portfolio.rebalancing(
    R5, portf,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    rolling_window = 36,
    warm_start = TRUE
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
})

# ============================================================================
# Tests: optimize.portfolio.rebalancing — portfolio.list recursion (lines 1210–1234)
# ============================================================================

test_that("optimize.portfolio.rebalancing handles portfolio.list", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "long_only")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.constraint(p2, type = "long_only")
  p2 <- add.objective(p2, type = "return", name = "mean")

  plist <- combine.portfolios(list(p1, p2))
  result <- optimize.portfolio.rebalancing(
    R5, plist,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24
  )
  expect_s3_class(result, "opt.rebal.list")
  expect_length(result, 2)
})

# ============================================================================
# Tests: optimize.portfolio.rebalancing — rolling_window and training_period
# ============================================================================

test_that("rebalancing with rolling_window uses proper windowing", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  result <- optimize.portfolio.rebalancing(
    R5, portf,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    rolling_window = 36
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
  expect_true(length(result$opt_rebalancing) >= 1)
})

test_that("training_period defaults from rolling_window when NULL", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # training_period=NULL, rolling_window set → training_period=rolling_window
  result <- optimize.portfolio.rebalancing(
    R5, portf,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = NULL,
    rolling_window = 24
  )
  expect_s3_class(result, "optimize.portfolio.rebalancing")
})

# ============================================================================
# Tests: optimize.portfolio.parallel (lines 1433–1470)
# ============================================================================

test_that("optimize.portfolio.parallel returns correct structure", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  set.seed(3491)
  result <- optimize.portfolio.parallel(
    R5, portf,
    optimize_method = "random",
    search_size = 50,
    trace = TRUE,
    nodes = 2
  )
  expect_s3_class(result, "optimize.portfolio.parallel")
  expect_equal(length(result$optimizations), 2)
  expect_true(!is.null(result$call))
  expect_true(!is.null(result$elapsed_time))
})

# ============================================================================
# Tests: solver-specific dispatch paths
# ============================================================================

test_that("DEoptim solver dispatch works", {
  skip_if_not_installed("DEoptim")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  set.seed(9213)
  result <- optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                               search_size = 500, trace = TRUE)
  expect_s3_class(result, "optimize.portfolio.DEoptim")
})

test_that("GenSA solver dispatch works", {
  skip_if_not_installed("GenSA")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  set.seed(2178)
  result <- optimize.portfolio(R5, portf, optimize_method = "GenSA", trace = TRUE)
  expect_s3_class(result, "optimize.portfolio.GenSA")
})

test_that("pso solver dispatch works", {
  skip_if_not_installed("pso")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  set.seed(4567)
  result <- optimize.portfolio(R5, portf, optimize_method = "pso", trace = TRUE)
  expect_s3_class(result, "optimize.portfolio.pso")
})

# ============================================================================
# Tests: validation
# ============================================================================

test_that("non-portfolio object is rejected", {
  expect_error(
    optimize.portfolio(R5, portfolio = "not a portfolio",
                       optimize_method = "random"),
    "class 'portfolio'"
  )
})
