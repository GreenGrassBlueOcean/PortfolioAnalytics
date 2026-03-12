library(testthat)
library(PortfolioAnalytics)

context("check_portfolio_feasibility")

# --- Helper: build a minimal portfolio spec ---
make_spec <- function(n = 4, asset_names = paste0("A", seq_len(n))) {
  spec <- portfolio.spec(assets = asset_names)
  spec
}

# ============================================================
# 1. Feasible weights — all constraints satisfied
# ============================================================

test_that("fully feasible weights produce clean report", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)

  expect_s3_class(report, "feasibility_report")
  expect_true(report$feasible)
  expect_equal(report$summary$total_violations, 0L)
  expect_length(report$summary$violated_types, 0L)
  expect_true(report$violations$weight_sum$feasible)
  expect_true(report$violations$box$feasible)
})

# ============================================================
# 2. Weight sum violation
# ============================================================

test_that("weight sum violation detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)

  w <- c(A1 = 0.5, A2 = 0.5, A3 = 0.5, A4 = 0.5)  # sum = 2.0
  report <- check_portfolio_feasibility(w, spec)

  expect_false(report$feasible)
  expect_true("weight_sum" %in% report$summary$violated_types)
  expect_false(report$violations$weight_sum$feasible)
  expect_equal(report$violations$weight_sum$value, 2.0)
  expect_true(report$violations$weight_sum$violation > 0)
})

# ============================================================
# 3. Box constraint violation
# ============================================================

test_that("box constraint violation detected per-asset", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.05, max = 0.40)

  w <- c(A1 = 0.60, A2 = 0.20, A3 = 0.10, A4 = 0.10)  # A1 exceeds 0.40
  report <- check_portfolio_feasibility(w, spec)

  expect_false(report$feasible)
  expect_true("box" %in% report$summary$violated_types)
  expect_false(report$violations$box$feasible)
  expect_equal(report$violations$box$n_violations, 1L)
  expect_true(report$violations$box$details["A1"] > 0)
})

# ============================================================
# 4. Group constraint violation
# ============================================================

test_that("group constraint violation detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = c(1, 2), g2 = c(3, 4)),
                         group_min = c(0.3, 0.3),
                         group_max = c(0.5, 0.5))

  w <- c(A1 = 0.8, A2 = 0.1, A3 = 0.05, A4 = 0.05)
  # g1 sum = 0.9 (exceeds 0.5), g2 sum = 0.1 (below 0.3)
  report <- check_portfolio_feasibility(w, spec)

  expect_false(report$feasible)
  expect_true("group" %in% report$summary$violated_types)
  expect_false(report$violations$group$feasible)
})

# ============================================================
# 5. Position limit violation
# ============================================================

test_that("position limit violation detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "position_limit", max_pos = 2)

  w <- c(A1 = 0.4, A2 = 0.3, A3 = 0.2, A4 = 0.1)  # 4 non-zero positions
  report <- check_portfolio_feasibility(w, spec)

  expect_false(report$feasible)
  expect_true("position_limit" %in% report$summary$violated_types)
  expect_false(report$violations$position_limit$feasible)
  expect_equal(report$violations$position_limit$details$max_pos$value, 4L)
})

# ============================================================
# 6. Leverage exposure violation
# ============================================================

test_that("leverage exposure violation detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "weight_sum", min_sum = -0.01, max_sum = 0.01)
  spec <- add.constraint(spec, type = "box", min = -1, max = 1)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 1.5)

  w <- c(A1 = 0.8, A2 = 0.5, A3 = -0.7, A4 = -0.6)  # sum(abs) = 2.6
  report <- check_portfolio_feasibility(w, spec)

  expect_false(report$feasible)
  expect_true("leverage_exposure" %in% report$summary$violated_types)
  expect_true(report$violations$leverage_exposure$violation > 0)
})

# ============================================================
# 7. Factor exposure violation
# ============================================================

test_that("factor exposure violation detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)

  B <- matrix(c(1.2, 0.8, 0.5, 1.5), ncol = 1, dimnames = list(NULL, "mkt"))
  spec <- add.constraint(spec, type = "factor_exposure",
                         B = B, lower = 0.5, upper = 0.9)

  w <- c(A1 = 0.6, A2 = 0.1, A3 = 0.1, A4 = 0.2)
  # exposure = 0.6*1.2 + 0.1*0.8 + 0.1*0.5 + 0.2*1.5 = 0.72+0.08+0.05+0.30 = 1.15
  report <- check_portfolio_feasibility(w, spec)

  expect_false(report$feasible)
  expect_true("factor_exposure" %in% report$summary$violated_types)
  expect_equal(report$violations$factor_exposure$n_violations, 1L)
})

# ============================================================
# 8. Diversification constraint violation
# ============================================================

test_that("diversification constraint violation detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "diversification", div_target = 0.7)

  w <- c(A1 = 0.9, A2 = 0.05, A3 = 0.03, A4 = 0.02)
  # diversification = 1 - (0.81 + 0.0025 + 0.0009 + 0.0004) = 1 - 0.8138 = 0.1862
  report <- check_portfolio_feasibility(w, spec)

  expect_false(report$feasible)
  expect_true("diversification" %in% report$summary$violated_types)
  expect_true(report$violations$diversification$value < 0.7)
})

# ============================================================
# 9. Unchecked constraints produce note entries
# ============================================================

test_that("turnover constraint produces 'not checked' entry", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "turnover", turnover_target = 0.1)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)

  expect_true(report$feasible)  # turnover not counted as a violation
  expect_true(is.na(report$violations$turnover$feasible))
  expect_true(grepl("Not checked", report$violations$turnover$note))
})

# ============================================================
# 10. Print method works
# ============================================================

test_that("print method runs for feasible and infeasible reports", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  w_ok <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  w_bad <- c(A1 = 0.7, A2 = 0.1, A3 = 0.1, A4 = 0.1)

  expect_output(print(check_portfolio_feasibility(w_ok, spec)), "ALL CONSTRAINTS SATISFIED")
  expect_output(print(check_portfolio_feasibility(w_bad, spec)), "VIOLATIONS DETECTED")
})

# ============================================================
# 11. Summary fields are correct
# ============================================================

test_that("summary fields are populated correctly", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.4)

  w <- c(A1 = 0.7, A2 = 0.1, A3 = 0.1, A4 = 0.1)  # box violated
  report <- check_portfolio_feasibility(w, spec)

  expect_equal(report$summary$total_checked, 2L)  # weight_sum + box
  expect_equal(report$summary$total_violations, 1L)  # only box
  expect_true(report$tolerance > 0)
})


# ============================================================
# 12. Binding detection — weight_sum
# ============================================================

test_that("weight_sum binding at upper bound detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)

  # sum = 1.01, exactly at upper bound
  w <- c(A1 = 0.30, A2 = 0.30, A3 = 0.20, A4 = 0.21)
  report <- check_portfolio_feasibility(w, spec)

  expect_equal(report$violations$weight_sum$status, "binding")
  expect_equal(report$violations$weight_sum$binding_bound, "upper")
  expect_equal(report$violations$weight_sum$slack, 0)
  expect_true(report$violations$weight_sum$feasible)
})

test_that("weight_sum binding at lower bound detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)

  # sum = 0.99, exactly at lower bound
  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.24)
  report <- check_portfolio_feasibility(w, spec)

  expect_equal(report$violations$weight_sum$status, "binding")
  expect_equal(report$violations$weight_sum$binding_bound, "lower")
})

# ============================================================
# 13. Binding detection — box
# ============================================================

test_that("box binding at upper limit for specific assets", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  # A1 at upper (0.5), A4 at lower (0.0)
  w <- c(A1 = 0.5, A2 = 0.25, A3 = 0.25, A4 = 0.0)
  report <- check_portfolio_feasibility(w, spec)

  expect_true(report$feasible)
  expect_equal(report$violations$box$status[["A1"]], "binding")
  expect_equal(report$violations$box$binding_bound[["A1"]], "upper")
  expect_equal(report$violations$box$status[["A4"]], "binding")
  expect_equal(report$violations$box$binding_bound[["A4"]], "lower")
  expect_equal(report$violations$box$status[["A2"]], "inactive")
  expect_true(report$violations$box$slack[["A2"]] > 0)
  expect_true(report$violations$box$n_binding >= 2L)
})

# ============================================================
# 14. Binding detection — group
# ============================================================

test_that("group binding at upper bound detected", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = c(1, 2), g2 = c(3, 4)),
                         group_min = c(0.2, 0.2),
                         group_max = c(0.5, 0.8))

  # g1 sum = 0.5 (at cUP), g2 sum = 0.5 (inactive)
  w <- c(A1 = 0.3, A2 = 0.2, A3 = 0.3, A4 = 0.2)
  report <- check_portfolio_feasibility(w, spec)

  expect_equal(report$violations$group$details$g1$status, "binding")
  expect_equal(report$violations$group$details$g1$binding_bound, "upper")
  expect_equal(report$violations$group$details$g2$status, "inactive")
  expect_true(report$violations$group$details$g2$slack > 0)
  expect_true("group" %in% report$summary$binding_types)
})

# ============================================================
# 15. Inactive constraints have positive slack
# ============================================================

test_that("inactive constraints have positive slack", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.5, max_sum = 1.5)
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)

  expect_equal(report$violations$weight_sum$status, "inactive")
  expect_true(report$violations$weight_sum$slack > 0)
  expect_equal(report$violations$weight_sum$binding_bound, "none")
  # slack = min(1.0 - 0.5, 1.5 - 1.0) = 0.5
  expect_equal(report$violations$weight_sum$slack, 0.5)
})

# ============================================================
# 16. Violated constraints have "violated" status and negative slack
# ============================================================

test_that("violated constraints have correct status and negative slack", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)

  w <- c(A1 = 0.5, A2 = 0.5, A3 = 0.5, A4 = 0.5)  # sum = 2.0
  report <- check_portfolio_feasibility(w, spec)

  expect_equal(report$violations$weight_sum$status, "violated")
  expect_true(report$violations$weight_sum$slack < 0)
  expect_equal(report$violations$weight_sum$binding_bound, "upper")
})

# ============================================================
# 17. Unchecked constraints have "unchecked" status
# ============================================================

test_that("unchecked constraints get status = 'unchecked'", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "turnover", turnover_target = 0.1)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)

  expect_equal(report$violations$turnover$status, "unchecked")
  expect_equal(report$summary$unchecked_count, 1L)
})

# ============================================================
# 18. Summary binding_count and binding_types
# ============================================================

test_that("summary binding fields are correct", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  # sum = 1 (binding at both min/max of full_investment), A1 at upper box
  w <- c(A1 = 0.5, A2 = 0.2, A3 = 0.2, A4 = 0.1)
  report <- check_portfolio_feasibility(w, spec)

  expect_true(report$summary$binding_count >= 1L)
  expect_true("weight_sum" %in% report$summary$binding_types)
  expect_true("box" %in% report$summary$binding_types)
  expect_equal(report$summary$violated_count, 0L)
})

# ============================================================
# 19. Position limit exactly at limit → binding
# ============================================================

test_that("position limit exactly at limit is binding", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "position_limit", max_pos = 3)

  # 3 non-zero positions out of 4 → exactly at limit
  w <- c(A1 = 0.4, A2 = 0.3, A3 = 0.3, A4 = 0.0)
  report <- check_portfolio_feasibility(w, spec)

  expect_true(report$violations$position_limit$feasible)
  expect_equal(report$violations$position_limit$details$max_pos$status, "binding")
  expect_equal(report$violations$position_limit$details$max_pos$slack, 0)
  expect_true("position_limit" %in% report$summary$binding_types)
})

# ============================================================
# 20. as.data.frame returns correct structure
# ============================================================

test_that("as.data.frame returns data.frame with expected columns", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)

  expect_s3_class(df, "data.frame")
  expected_cols <- c("type", "element", "value", "lower", "upper",
                     "violation", "slack", "status", "binding_bound")
  expect_true(all(expected_cols %in% names(df)))
})

test_that("as.data.frame has correct number of rows", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = c(1, 2), g2 = c(3, 4)),
                         group_min = c(0.1, 0.1),
                         group_max = c(0.9, 0.9))

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)

  # 1 weight_sum + 4 box + 2 group = 7 rows
  expect_equal(nrow(df), 7L)
})

test_that("as.data.frame uses 'total' for scalar constraints", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)

  ws_rows <- df[df$type == "weight_sum", ]
  expect_equal(ws_rows$element, "total")
})

test_that("as.data.frame status column has correct values", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)
  spec <- add.constraint(spec, type = "turnover", turnover_target = 0.1)

  # A1 at upper box limit (binding), sum=1 (binding full inv)
  w <- c(A1 = 0.5, A2 = 0.2, A3 = 0.2, A4 = 0.1)
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)

  expect_true("binding" %in% df$status)
  expect_true("inactive" %in% df$status)
  expect_true("unchecked" %in% df$status)
})

test_that("as.data.frame unchecked rows have NA for numeric columns", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "turnover", turnover_target = 0.1)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)

  uc <- df[df$status == "unchecked", ]
  expect_true(all(is.na(uc$value)))
  expect_true(all(is.na(uc$violation)))
  expect_true(all(is.na(uc$slack)))
})

# ============================================================
# 21. extractFeasibility works
# ============================================================

test_that("extractFeasibility.optimize.portfolio returns report", {
  # Build a mock optimization result with a feasibility_report
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  fr <- check_portfolio_feasibility(w, spec)

  mock_opt <- list(
    weights = w,
    feasibility_report = fr
  )
  class(mock_opt) <- c("optimize.portfolio.ROI", "optimize.portfolio")

  result <- extractFeasibility(mock_opt)
  expect_s3_class(result, "feasibility_report")
  expect_true(result$feasible)
})

test_that("extractFeasibility with as.data.frame returns data.frame", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  fr <- check_portfolio_feasibility(w, spec)

  mock_opt <- list(weights = w, feasibility_report = fr)
  class(mock_opt) <- c("optimize.portfolio.ROI", "optimize.portfolio")

  df <- extractFeasibility(mock_opt, as.data.frame = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
})

test_that("extractFeasibility returns NULL when no report", {
  mock_opt <- list(weights = c(0.5, 0.5))
  class(mock_opt) <- c("optimize.portfolio.ROI", "optimize.portfolio")

  expect_null(extractFeasibility(mock_opt))
})

# ============================================================
# 22. Solver diagnostics — ROI (quadprog)
# ============================================================

test_that("solver_diagnostics present after ROI quadprog optimization", {
  data(edhec, package = "PerformanceAnalytics")
  R <- edhec[, 1:4]
  spec <- portfolio.spec(assets = colnames(R))
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")
  spec <- add.objective(spec, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R, spec, optimize_method = "ROI")

  expect_false(is.null(opt$solver_diagnostics))
  # quadprog should return Lagrangian and iact
  expect_true("Lagrangian" %in% names(opt$solver_diagnostics))
  expect_true("iact" %in% names(opt$solver_diagnostics))
})

# ============================================================
# 23. Solver diagnostics — ROI (glpk LP)
# ============================================================

test_that("solver_diagnostics present after ROI glpk LP optimization", {
  data(edhec, package = "PerformanceAnalytics")
  R <- edhec[, 1:4]
  spec <- portfolio.spec(assets = colnames(R))
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")
  spec <- add.objective(spec, type = "return", name = "mean")

  opt <- optimize.portfolio(R, spec, optimize_method = "ROI")

  expect_false(is.null(opt$solver_diagnostics))
  # glpk should return solution_dual and auxiliary
  expect_true("solution_dual" %in% names(opt$solver_diagnostics))
  expect_true("auxiliary" %in% names(opt$solver_diagnostics))
})

# ============================================================
# 24. Solver diagnostics NULL for stochastic solvers
# ============================================================

test_that("solver_diagnostics NULL for random solver", {
  data(edhec, package = "PerformanceAnalytics")
  R <- edhec[, 1:4]
  spec <- portfolio.spec(assets = colnames(R))
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")
  spec <- add.objective(spec, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R, spec, optimize_method = "random",
                            search_size = 500)

  expect_null(opt$solver_diagnostics)
})

# ============================================================
# 25. Backward compat: existing violation structure unchanged
# ============================================================

test_that("violation details structure unchanged for box", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.05, max = 0.40)

  w <- c(A1 = 0.60, A2 = 0.20, A3 = 0.10, A4 = 0.10)
  report <- check_portfolio_feasibility(w, spec)

  # Original fields still present
  expect_true("details" %in% names(report$violations$box))
  expect_true("n_violations" %in% names(report$violations$box))
  expect_true("feasible" %in% names(report$violations$box))
  # New fields also present
  expect_true("status" %in% names(report$violations$box))
  expect_true("slack" %in% names(report$violations$box))
  expect_true("binding_bound" %in% names(report$violations$box))
})

# ============================================================
# 26. Print method shows binding info
# ============================================================

test_that("print method shows binding count", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 0.5)

  w <- c(A1 = 0.5, A2 = 0.2, A3 = 0.2, A4 = 0.1)
  report <- check_portfolio_feasibility(w, spec)

  expect_output(print(report), "Binding:")
})

# ============================================================
# 27. All-inactive scenario
# ============================================================

test_that("all-inactive with loose constraints works", {
  spec <- make_spec()
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.5, max_sum = 1.5)
  spec <- add.constraint(spec, type = "box", min = -1, max = 1)

  w <- c(A1 = 0.25, A2 = 0.25, A3 = 0.25, A4 = 0.25)
  report <- check_portfolio_feasibility(w, spec)

  expect_true(report$feasible)
  expect_equal(report$summary$binding_count, 0L)
  expect_length(report$summary$binding_types, 0L)
  expect_equal(report$violations$weight_sum$status, "inactive")
  expect_true(all(report$violations$box$status == "inactive"))
})
