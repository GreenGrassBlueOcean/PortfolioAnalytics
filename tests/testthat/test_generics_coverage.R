
require(testthat)
require(PortfolioAnalytics)

context("generics.R coverage: print/summary methods for all optimizer classes")

# ============================================================================
# Shared test data — computed once, reused across all tests
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ---------- Portfolio with group constraints and multiple objectives ---------
portf_groups <- portfolio.spec(assets = colnames(R5))
portf_groups <- add.constraint(portf_groups, type = "weight_sum",
                               min_sum = 0.99, max_sum = 1.01)
portf_groups <- add.constraint(portf_groups, type = "box", min = 0.05, max = 0.55)
portf_groups <- add.constraint(portf_groups, type = "group",
                               groups = list(c(1, 2), c(3, 4, 5)),
                               group_min = c(0.1, 0.15),
                               group_max = c(0.6, 0.85))
portf_groups <- add.objective(portf_groups, type = "return", name = "mean")
portf_groups <- add.objective(portf_groups, type = "risk", name = "StdDev")

# ---------- Portfolio with category labels -----------------------------------
portf_cat <- portfolio.spec(assets = colnames(R5),
                            category_labels = c("Growth", "Growth",
                                                "Value", "Value", "Value"))
portf_cat <- add.constraint(portf_cat, type = "full_investment")
portf_cat <- add.constraint(portf_cat, type = "long_only")
portf_cat <- add.objective(portf_cat, type = "risk", name = "StdDev")

# ---------- Portfolio with >10 assets for truncation branch ------------------
R13 <- edhec[1:48, ]
colnames(R13) <- paste0("Asset", seq_len(ncol(R13)))
portf_many <- portfolio.spec(assets = colnames(R13))
portf_many <- add.constraint(portf_many, type = "full_investment")
portf_many <- add.constraint(portf_many, type = "long_only")
portf_many <- add.objective(portf_many, type = "risk", name = "StdDev")

# ---------- Portfolio with short-selling box constraints ----------------------
portf_short <- portfolio.spec(assets = colnames(R5))
portf_short <- add.constraint(portf_short, type = "weight_sum",
                               min_sum = -0.01, max_sum = 0.01)
portf_short <- add.constraint(portf_short, type = "box", min = -0.5, max = 0.5)
portf_short <- add.objective(portf_short, type = "risk", name = "StdDev")

# ---------- Portfolio with unconstrained box ---------------------------------
portf_uncon <- portfolio.spec(assets = colnames(R5))
portf_uncon <- add.constraint(portf_uncon, type = "weight_sum",
                               min_sum = 0.99, max_sum = 1.01)
portf_uncon <- add.constraint(portf_uncon, type = "box", min = -Inf, max = Inf)
portf_uncon <- add.objective(portf_uncon, type = "risk", name = "StdDev")

# ---------- Portfolio with disabled constraints/objectives -------------------
portf_disabled <- portfolio.spec(assets = colnames(R5))
portf_disabled <- add.constraint(portf_disabled, type = "full_investment")
portf_disabled <- add.constraint(portf_disabled, type = "long_only")
portf_disabled <- add.constraint(portf_disabled, type = "group",
                                 groups = list(c(1, 2), c(3, 4, 5)),
                                 group_min = c(0.1, 0.15),
                                 group_max = c(0.6, 0.85),
                                 enabled = FALSE)
portf_disabled <- add.objective(portf_disabled, type = "return", name = "mean")
portf_disabled <- add.objective(portf_disabled, type = "risk", name = "StdDev",
                                enabled = FALSE)


# ============================================================================
# Tests: print.portfolio branches
# ============================================================================

test_that("print.portfolio shows category labels", {
  out <- capture.output(print(portf_cat))
  expect_true(any(grepl("Category Labels", out)))
  expect_true(any(grepl("Growth", out)))
  expect_true(any(grepl("Value", out)))
})

test_that("print.portfolio truncates >10 assets", {
  out <- capture.output(print(portf_many))
  expect_true(any(grepl("More than 10 assets", out)))
})

test_that("print.portfolio shows box (with shorting)", {
  out <- capture.output(print(portf_short))
  expect_true(any(grepl("with shorting", out)))
})

test_that("print.portfolio shows box (unconstrained)", {
  out <- capture.output(print(portf_uncon))
  expect_true(any(grepl("unconstrained", out)))
})

test_that("print.portfolio shows disabled constraints and objectives", {
  out <- capture.output(print(portf_disabled))
  expect_true(any(grepl("Disabled constraint types", out)))
  expect_true(any(grepl("Disabled objective names", out)))
})

test_that("print.portfolio shows group constraint type", {
  out <- capture.output(print(portf_groups))
  expect_true(any(grepl("group", out)))
})

# ============================================================================
# Tests: summary.portfolio
# ============================================================================

test_that("summary.portfolio separates enabled/disabled constraints", {
  s <- summary(portf_disabled)
  expect_s3_class(s, "summary.portfolio")
  expect_length(s$enabled_constraints, 2)
  expect_length(s$disabled_constraints, 1)
  expect_length(s$enabled_objectives, 1)
  expect_length(s$disabled_objectives, 1)
})

test_that("summary.portfolio includes category labels", {
  s <- summary(portf_cat)
  expect_true(!is.null(s$category_labels))
  expect_true("Growth" %in% names(s$category_labels))
})

# ============================================================================
# Tests: print.constraint
# ============================================================================

test_that("print.constraint delegates to print.default", {
  constr <- portf_groups$constraints[[1]]
  expect_output(print(constr))
})

# ============================================================================
# Tests: print methods for each optimizer class
# ============================================================================

# --- Random ---
test_that("print.optimize.portfolio.random covers objective loop", {
  set.seed(4281)
  opt <- optimize.portfolio(R5, portf_groups,
                            optimize_method = "random",
                            search_size = 200, trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("Objective Measures", out)))
})

# --- ROI ---
test_that("print.optimize.portfolio.ROI covers objective loop", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_roi <- portfolio.spec(assets = colnames(R5))
  portf_roi <- add.constraint(portf_roi, type = "full_investment")
  portf_roi <- add.constraint(portf_roi, type = "box", min = 0.05, max = 0.55)
  portf_roi <- add.objective(portf_roi, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R5, portf_roi, optimize_method = "ROI", trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("Objective Measure", out)))
})

test_that("print.optimize.portfolio.random covers nested risk budget measures", {
  # risk_budget produces nested objective_measures with contribution and pct_contrib
  portf_rb <- portfolio.spec(assets = colnames(R5))
  portf_rb <- add.constraint(portf_rb, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
  portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                            min_prisk = 0.05, max_prisk = 0.40)

  set.seed(2847)
  opt <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                            search_size = 200, trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("contribution", out) | grepl("pct_contrib", out)))
})

# --- DEoptim ---
test_that("print.optimize.portfolio.DEoptim covers objective loop", {
  skip_if_not_installed("DEoptim")

  set.seed(7893)
  opt <- optimize.portfolio(R5, portf_groups,
                            optimize_method = "DEoptim",
                            search_size = 500, trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("Objective Measures", out)))
})

# --- GenSA ---
test_that("print.optimize.portfolio.GenSA covers objective loop", {
  skip_if_not_installed("GenSA")

  set.seed(6145)
  opt <- optimize.portfolio(R5, portf_groups,
                            optimize_method = "GenSA", trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("Objective Measures", out)))
})

# --- PSO ---
test_that("print.optimize.portfolio.pso covers objective loop", {
  skip_if_not_installed("pso")

  set.seed(3782)
  opt <- optimize.portfolio(R5, portf_groups,
                            optimize_method = "pso", trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("Objective Measures", out)))
})

# --- CVXR ---
test_that("print.optimize.portfolio.CVXR covers objective loop", {
  skip_if_not_installed("CVXR")

  portf_cvxr <- portfolio.spec(assets = colnames(R5))
  portf_cvxr <- add.constraint(portf_cvxr, type = "full_investment")
  portf_cvxr <- add.constraint(portf_cvxr, type = "box", min = 0.05, max = 0.55)
  portf_cvxr <- add.objective(portf_cvxr, type = "risk", name = "StdDev")

  opt <- tryCatch(
    optimize.portfolio(R5, portf_cvxr, optimize_method = "CVXR", trace = TRUE),
    error = function(e) skip("CVXR solver not functional in this environment")
  )
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("Objective Measures", out)))
})

# ============================================================================
# Tests: summary.optimize.portfolio — constraint realization branches
# ============================================================================

test_that("summary.optimize.portfolio includes group constraints", {
  set.seed(4281)
  opt <- optimize.portfolio(R5, portf_groups,
                            optimize_method = "random",
                            search_size = 200, trace = TRUE)
  s <- summary(opt)
  expect_s3_class(s, "summary.optimize.portfolio")
  expect_true(!is.null(s$group_constraint))
  expect_true(!is.null(s$group_constraint$group_weights_actual))
  expect_true(!is.null(s$leverage_constraint))
  expect_true(!is.null(s$box_constraint))
  expect_true(!is.null(s$position_limit_constraint))
  expect_true(!is.null(s$diversification_constraint))
  expect_true(!is.null(s$turnover_constraint))
})

test_that("summary.optimize.portfolio includes category weights when no groups", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  opt <- optimize.portfolio(R5, portf_cat, optimize_method = "ROI", trace = TRUE)
  s <- summary(opt)
  expect_true(!is.null(s$category_weights))
  expect_true(!is.null(s$category_weights$category_weights_actual))
})

test_that("summary.optimize.portfolio includes factor exposure constraints", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_factor <- portfolio.spec(assets = colnames(R5))
  portf_factor <- add.constraint(portf_factor, type = "full_investment")
  portf_factor <- add.constraint(portf_factor, type = "box", min = 0.05, max = 0.55)
  B <- matrix(runif(10), nrow = 5, ncol = 2)
  colnames(B) <- c("Factor1", "Factor2")
  rownames(B) <- colnames(R5)
  portf_factor <- add.constraint(portf_factor, type = "factor_exposure",
                                 B = B,
                                 lower = c(0.1, 0.1),
                                 upper = c(0.9, 0.9))
  portf_factor <- add.objective(portf_factor, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R5, portf_factor,
                            optimize_method = "ROI", trace = TRUE)
  s <- summary(opt)
  expect_true(!is.null(s$factor_exposure_constraint))
  expect_true(!is.null(s$factor_exposure_constraint$exposure_actual))
  expect_equal(length(s$factor_exposure_constraint$exposure_actual), 2)
})

# ============================================================================
# Tests: print.summary.optimize.portfolio — all display branches
# ============================================================================

test_that("print.summary covers leverage, box, group, position limits, div, turnover", {
  set.seed(4281)
  opt <- optimize.portfolio(R5, portf_groups,
                            optimize_method = "random",
                            search_size = 200, trace = TRUE)
  s <- summary(opt)
  out <- capture.output(print(s))
  expect_true(any(grepl("Leverage Constraint", out)))
  expect_true(any(grepl("Box Constraints", out)))
  expect_true(any(grepl("Group Constraints", out)))
  expect_true(any(grepl("Group Weights", out)))
  expect_true(any(grepl("Position Limit", out)))
  expect_true(any(grepl("Diversification", out)))
  expect_true(any(grepl("Turnover", out)))
  expect_true(any(grepl("Objectives", out)))
  expect_true(any(grepl("Elapsed Time", out)))
})

test_that("print.summary covers factor exposure display", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_factor <- portfolio.spec(assets = colnames(R5))
  portf_factor <- add.constraint(portf_factor, type = "full_investment")
  portf_factor <- add.constraint(portf_factor, type = "box", min = 0.05, max = 0.55)
  B <- matrix(runif(10), nrow = 5, ncol = 2)
  colnames(B) <- c("Factor1", "Factor2")
  rownames(B) <- colnames(R5)
  portf_factor <- add.constraint(portf_factor, type = "factor_exposure",
                                 B = B,
                                 lower = c(0.1, 0.1),
                                 upper = c(0.9, 0.9))
  portf_factor <- add.objective(portf_factor, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R5, portf_factor,
                            optimize_method = "ROI", trace = TRUE)
  out <- capture.output(print(summary(opt)))
  expect_true(any(grepl("Factor Exposure", out)))
  expect_true(any(grepl("Factor1", out)))
})

test_that("print.summary handles NULL objective_values (ROI out path)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_roi <- portfolio.spec(assets = colnames(R5))
  portf_roi <- add.constraint(portf_roi, type = "full_investment")
  portf_roi <- add.constraint(portf_roi, type = "box", min = 0.05, max = 0.55)
  portf_roi <- add.objective(portf_roi, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R5, portf_roi, optimize_method = "ROI", trace = TRUE)
  s <- summary(opt)
  out <- capture.output(print(s))
  expect_true(any(grepl("Objective Measures", out)))
})

# ============================================================================
# Tests: print/summary for optimize.portfolio.rebalancing
# ============================================================================

test_that("print/summary rebalancing cover all display sections", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_rebal <- portfolio.spec(assets = colnames(R5))
  portf_rebal <- add.constraint(portf_rebal, type = "full_investment")
  portf_rebal <- add.constraint(portf_rebal, type = "box", min = 0.05, max = 0.55)
  portf_rebal <- add.objective(portf_rebal, type = "risk", name = "StdDev")

  opt_rebal <- optimize.portfolio.rebalancing(
    R5, portf_rebal,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    trace = TRUE
  )

  # print method
  out <- capture.output(print(opt_rebal))
  expect_true(any(grepl("Rebalancing", out)))
  expect_true(any(grepl("Annualized", out)))

  # summary method
  s <- summary(opt_rebal)
  expect_s3_class(s, "summary.optimize.portfolio.rebalancing")
  expect_true(!is.null(s$portfolio_returns))
  expect_true(!is.null(s$downside_risk))

  # print.summary method
  out2 <- capture.output(print(s))
  expect_true(any(grepl("Downside Risk", out2)))
  expect_true(any(grepl("Annualized Portfolio Standard Deviation", out2)))
})

# ============================================================================
# Tests: efficient frontier print/summary
# ============================================================================

test_that("print.efficient.frontier covers all sections", {
  # Construct a mock efficient.frontier object to test print/summary generics
  # without depending on CVXR (which extract_risk uses internally)
  portf_ef <- portfolio.spec(assets = colnames(R5))
  portf_ef <- add.constraint(portf_ef, type = "full_investment")
  portf_ef <- add.constraint(portf_ef, type = "box", min = 0.05, max = 0.55)
  portf_ef <- add.objective(portf_ef, type = "return", name = "mean")
  portf_ef <- add.objective(portf_ef, type = "risk", name = "StdDev")

  frontier_mat <- matrix(runif(50), nrow = 5, ncol = 10)
  colnames(frontier_mat) <- c(paste0("w.", colnames(R5)), "mean", "StdDev",
                               "out", "w.A", "w.B")
  # Fix: use proper column names for 5 assets + metrics
  frontier_mat <- matrix(runif(40), nrow = 5, ncol = 8)
  colnames(frontier_mat) <- c(paste0("w.", colnames(R5)), "mean", "StdDev", "out")

  ef <- structure(list(
    call = quote(create.EfficientFrontier(R5, portf_ef, type = "mean-StdDev")),
    frontier = frontier_mat,
    R = R5,
    portfolio = portf_ef
  ), class = "efficient.frontier")

  out <- capture.output(print(ef))
  expect_true(any(grepl("Efficient Frontier", out)))
  expect_true(any(grepl("Efficient Frontier Points: 5", out)))
})

test_that("summary.efficient.frontier returns weights and metrics", {
  portf_ef <- portfolio.spec(assets = colnames(R5))
  portf_ef <- add.constraint(portf_ef, type = "full_investment")
  portf_ef <- add.constraint(portf_ef, type = "box", min = 0.05, max = 0.55)
  portf_ef <- add.objective(portf_ef, type = "return", name = "mean")
  portf_ef <- add.objective(portf_ef, type = "risk", name = "StdDev")

  frontier_mat <- matrix(runif(40), nrow = 5, ncol = 8)
  colnames(frontier_mat) <- c(paste0("w.", colnames(R5)), "mean", "StdDev", "out")

  ef <- structure(list(
    call = quote(create.EfficientFrontier(R5, portf_ef, type = "mean-StdDev")),
    frontier = frontier_mat,
    R = R5,
    portfolio = portf_ef
  ), class = "efficient.frontier")

  out <- capture.output(s <- summary(ef))
  expect_true(any(grepl("Weights along", out)))
  expect_true(any(grepl("Risk and return", out)))
  expect_true(!is.null(s$weights))
  expect_true(!is.null(s$metrics))
  expect_equal(nrow(s$weights), 5)
})

# ============================================================================
# Tests: print.portfolio.list, print.opt.list, print.opt.rebal.list
# ============================================================================

test_that("print.portfolio.list iterates through portfolios", {
  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.objective(p2, type = "return", name = "mean")

  plist <- list(p1, p2)
  class(plist) <- "portfolio.list"

  out <- capture.output(print(plist))
  expect_true(any(grepl("Portfolio 1", out)))
  expect_true(any(grepl("Portfolio 2", out)))
})

test_that("print.opt.list iterates through optimizations", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf1 <- portfolio.spec(assets = colnames(R5))
  portf1 <- add.constraint(portf1, type = "full_investment")
  portf1 <- add.constraint(portf1, type = "long_only")
  portf1 <- add.objective(portf1, type = "risk", name = "StdDev")

  opt1 <- optimize.portfolio(R5, portf1, optimize_method = "ROI")
  olist <- list(opt1, opt1)
  class(olist) <- "opt.list"

  out <- capture.output(print(olist))
  expect_true(any(grepl("Optimization 1", out)))
  expect_true(any(grepl("Optimization 2", out)))
})

test_that("print.opt.rebal.list iterates", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf1 <- portfolio.spec(assets = colnames(R5))
  portf1 <- add.constraint(portf1, type = "full_investment")
  portf1 <- add.constraint(portf1, type = "long_only")
  portf1 <- add.objective(portf1, type = "risk", name = "StdDev")

  opt_r <- optimize.portfolio.rebalancing(
    R5, portf1,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24
  )
  rlist <- list(opt_r, opt_r)
  class(rlist) <- "opt.rebal.list"

  out <- capture.output(print(rlist))
  expect_true(any(grepl("Optimization 1", out)))
  expect_true(any(grepl("Optimization 2", out)))
})

# ============================================================================
# Tests: print.regime.portfolios
# ============================================================================

test_that("print.regime.portfolios displays regime specification", {
  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.objective(p2, type = "return", name = "mean")

  regime_obj <- list(
    portfolio.list = list(p1, p2),
    regime = xts::xts(c(1, 2, 1), order.by = index(R5)[c(12, 24, 36)])
  )
  class(regime_obj) <- "regime.portfolios"

  out <- capture.output(print(regime_obj))
  expect_true(any(grepl("Regime Switching", out)))
  expect_true(any(grepl("Regime 1", out)))
  expect_true(any(grepl("Regime 2", out)))
})

# ============================================================================
# Tests: optimize.portfolio.parallel print/summary
# ============================================================================

test_that("print/summary for optimize.portfolio.parallel work", {
  set.seed(6532)
  # Use random method with small search_size for speed
  opt_par <- optimize.portfolio.parallel(
    R5, portf_groups,
    optimize_method = "random",
    search_size = 100,
    trace = TRUE,
    nodes = 2
  )

  # print method calls summary internally
  out <- capture.output(print(opt_par))
  expect_true(any(grepl("Number of Optimizations", out)))
  expect_true(any(grepl("Objective Value Estimate", out)))
  expect_true(any(grepl("Percentiles", out)))
  expect_true(any(grepl("Elapsed Time", out)))

  # summary method
  s <- summary(opt_par)
  expect_s3_class(s, "summary.optimize.portfolio.parallel")
  expect_equal(s$n_optimizations, 2)
  expect_true(!is.null(s$obj_val))
  expect_true(!is.null(s$stats))
})
