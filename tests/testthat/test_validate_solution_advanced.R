library(testthat)
library(PortfolioAnalytics)

context("validate_solution advanced: classify helpers, feasibility checks, as.data.frame")

# ============================================================================
# Shared setup
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:36, 1:4]
funds4 <- colnames(R4)

# ============================================================================
# A. .classify_two_sided helper
# ============================================================================

test_that(".classify_two_sided detects lower violation", {
  result <- PortfolioAnalytics:::.classify_two_sided(0.3, lo = 0.5, hi = 0.8, tol = 1e-8)
  expect_equal(result$status, "violated")
  expect_true(result$slack < 0)
  expect_equal(result$binding_bound, "lower")
})

test_that(".classify_two_sided detects upper violation", {
  result <- PortfolioAnalytics:::.classify_two_sided(0.9, lo = 0.2, hi = 0.7, tol = 1e-8)
  expect_equal(result$status, "violated")
  expect_true(result$slack < 0)
  expect_equal(result$binding_bound, "upper")
})

test_that(".classify_two_sided detects lower binding", {
  result <- PortfolioAnalytics:::.classify_two_sided(0.5, lo = 0.5, hi = 0.8, tol = 1e-8)
  expect_equal(result$status, "binding")
  expect_equal(result$slack, 0)
  expect_equal(result$binding_bound, "lower")
})

test_that(".classify_two_sided detects upper binding", {
  result <- PortfolioAnalytics:::.classify_two_sided(0.8, lo = 0.2, hi = 0.8, tol = 1e-8)
  expect_equal(result$status, "binding")
  expect_equal(result$slack, 0)
  expect_equal(result$binding_bound, "upper")
})

test_that(".classify_two_sided detects inactive", {
  result <- PortfolioAnalytics:::.classify_two_sided(0.5, lo = 0.2, hi = 0.8, tol = 1e-8)
  expect_equal(result$status, "inactive")
  expect_true(result$slack > 0)
  expect_equal(result$binding_bound, "none")
})

test_that(".classify_two_sided handles infinite bounds", {
  result <- PortfolioAnalytics:::.classify_two_sided(100, lo = -Inf, hi = Inf, tol = 1e-8)
  expect_equal(result$status, "inactive")
  expect_equal(result$slack, Inf)
})

# ============================================================================
# B. .classify_upper_only helper
# ============================================================================

test_that(".classify_upper_only detects violation", {
  result <- PortfolioAnalytics:::.classify_upper_only(1.5, limit = 1.0, tol = 1e-8)
  expect_equal(result$status, "violated")
  expect_true(result$slack < 0)
})

test_that(".classify_upper_only detects binding", {
  result <- PortfolioAnalytics:::.classify_upper_only(1.0, limit = 1.0, tol = 1e-8)
  expect_equal(result$status, "binding")
  expect_equal(result$slack, 0)
})

test_that(".classify_upper_only detects inactive", {
  result <- PortfolioAnalytics:::.classify_upper_only(0.8, limit = 1.0, tol = 1e-8)
  expect_equal(result$status, "inactive")
  expect_equal(result$slack, 0.2)
})

# ============================================================================
# C. .classify_lower_only helper
# ============================================================================

test_that(".classify_lower_only detects violation", {
  result <- PortfolioAnalytics:::.classify_lower_only(0.3, target = 0.5, tol = 1e-8)
  expect_equal(result$status, "violated")
  expect_true(result$slack < 0)
})

test_that(".classify_lower_only detects binding", {
  result <- PortfolioAnalytics:::.classify_lower_only(0.5, target = 0.5, tol = 1e-8)
  expect_equal(result$status, "binding")
  expect_equal(result$slack, 0)
})

test_that(".classify_lower_only detects inactive", {
  result <- PortfolioAnalytics:::.classify_lower_only(0.8, target = 0.5, tol = 1e-8)
  expect_equal(result$status, "inactive")
  expect_equal(result$slack, 0.3)
})

# ============================================================================
# D. .classify_count_limit helper
# ============================================================================

test_that(".classify_count_limit detects violation", {
  result <- PortfolioAnalytics:::.classify_count_limit(5, limit = 3)
  expect_equal(result$status, "violated")
  expect_equal(result$slack, -2)
})

test_that(".classify_count_limit detects binding", {
  result <- PortfolioAnalytics:::.classify_count_limit(3, limit = 3)
  expect_equal(result$status, "binding")
  expect_equal(result$slack, 0)
})

test_that(".classify_count_limit detects inactive", {
  result <- PortfolioAnalytics:::.classify_count_limit(2, limit = 3)
  expect_equal(result$status, "inactive")
  expect_equal(result$slack, 1)
})

# ============================================================================
# E. check_portfolio_feasibility: NA weight guard
# ============================================================================

test_that("check_portfolio_feasibility handles NA weights", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")

  w <- c(0.3, NA, 0.2, 0.2)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_s3_class(report, "feasibility_report")
  expect_false(report$feasible)
  expect_equal(report$summary$violated_types, "na_weights")
  expect_true(grepl("1 of 4", report$violations$na_weights$note))
})

# ============================================================================
# F. check_portfolio_feasibility: comprehensive constraint checking
# ============================================================================

test_that("check_portfolio_feasibility detects feasible portfolio", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.5)

  w <- c(0.3, 0.3, 0.2, 0.2)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_true(report$feasible)
  expect_equal(report$summary$violated_count, 0)
})

test_that("check_portfolio_feasibility detects weight_sum violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")

  w <- c(0.5, 0.5, 0.5, 0.5)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_false(report$feasible)
  expect_false(report$violations$weight_sum$feasible)
})

test_that("check_portfolio_feasibility detects box violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.4)

  w <- c(0.6, 0.2, 0.1, 0.1)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_false(report$feasible)
  expect_false(report$violations$box$feasible)
  expect_equal(report$violations$box$n_violations, 1)
})

test_that("check_portfolio_feasibility detects group violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = 1:2, g2 = 3:4),
                         group_min = c(0.4, 0.4),
                         group_max = c(0.6, 0.6))

  # Group 1 sum = 0.1, violates group_min
  w <- c(0.05, 0.05, 0.45, 0.45)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_false(report$feasible)
  expect_false(report$violations$group$feasible)
})

test_that("check_portfolio_feasibility detects position_limit violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "position_limit", max_pos = 2)

  w <- c(0.3, 0.3, 0.2, 0.2)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_false(report$feasible)
  expect_false(report$violations$position_limit$feasible)
  expect_equal(report$violations$position_limit$details$max_pos$status, "violated")
})

test_that("check_portfolio_feasibility detects leverage_exposure violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "weight_sum", min_sum = -0.01, max_sum = 0.01)
  spec <- add.constraint(spec, type = "box", min = -0.5, max = 0.5)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 1.0)

  w <- c(0.4, -0.4, 0.3, -0.3)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_false(report$feasible)
  expect_false(report$violations$leverage_exposure$feasible)
})

test_that("check_portfolio_feasibility detects factor_exposure violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  B <- c(1.5, 0.5, 1.0, 0.8)
  spec <- add.constraint(spec, type = "factor_exposure",
                         B = B, lower = 0.9, upper = 1.0)

  w <- c(0.5, 0.2, 0.2, 0.1)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  # Exposure = 1.5*0.5 + 0.5*0.2 + 1.0*0.2 + 0.8*0.1 = 1.13, violates upper=1.0
  expect_false(report$feasible)
  expect_false(report$violations$factor_exposure$feasible)
})

test_that("check_portfolio_feasibility detects diversification violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "diversification", div_target = 0.9)

  # Very concentrated portfolio: 1-sum(w^2) ≈ 1-0.81 = 0.19
  w <- c(0.9, 0.05, 0.03, 0.02)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_false(report$feasible)
  expect_false(report$violations$diversification$feasible)
})

test_that("check_portfolio_feasibility marks unchecked constraints", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "turnover", turnover_target = 0.2)
  spec <- add.constraint(spec, type = "return", return_target = 0.01)
  spec <- add.constraint(spec, type = "transaction_cost", ptc = 0.01)

  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_true(report$feasible)
  expect_true(is.na(report$violations$turnover$feasible))
  expect_true(is.na(report$violations$return_target$feasible))
  expect_true(is.na(report$violations$transaction_cost$feasible))
  expect_equal(report$summary$unchecked_count, 3)
})

test_that("check_portfolio_feasibility detects binding constraints", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.25, max = 0.25)

  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_true(report$feasible)
  # weight_sum should be binding at exactly 1
  expect_equal(report$violations$weight_sum$status, "binding")
  # box should be all binding (min == max == weight)
  expect_true(all(report$violations$box$status == "binding"))
  expect_true(report$summary$binding_count >= 2)
})

# ============================================================================
# G. as.data.frame.feasibility_report
# ============================================================================

test_that("as.data.frame produces correct structure for full report", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.5)
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = 1:2, g2 = 3:4),
                         group_min = c(0.3, 0.3),
                         group_max = c(0.7, 0.7))
  spec <- add.constraint(spec, type = "diversification", div_target = 0.5)

  w <- c(0.3, 0.3, 0.2, 0.2)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("type", "element", "status", "slack") %in% names(df)))
  expect_true("weight_sum" %in% df$type)
  expect_true("box" %in% df$type)
  expect_true("group" %in% df$type)
  expect_true("diversification" %in% df$type)
})

test_that("as.data.frame handles NA weights report", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")

  w <- c(NA, NA, 0.5, 0.5)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)
  expect_equal(nrow(df), 1)
  expect_equal(df$type, "na_weights")
  expect_equal(df$status, "violated")
})

test_that("as.data.frame handles position_limit rows", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "position_limit", max_pos = 3,
                         max_pos_long = 3, max_pos_short = 1)

  w <- c(0.4, 0.3, 0.2, 0.1)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)
  pl_rows <- df[df$type == "position_limit", ]
  expect_equal(nrow(pl_rows), 3)
  expect_true(all(c("max_pos", "max_pos_long", "max_pos_short") %in% pl_rows$element))
})

test_that("as.data.frame handles leverage_exposure rows", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0, max_sum = 0)
  spec <- add.constraint(spec, type = "box", min = -0.5, max = 0.5)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 2.0)

  w <- c(0.3, -0.3, 0.2, -0.2)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)
  lev_rows <- df[df$type == "leverage_exposure", ]
  expect_equal(nrow(lev_rows), 1)
  expect_equal(lev_rows$element, "total")
})

test_that("as.data.frame handles factor_exposure rows", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  B <- matrix(c(1, 0.5, 0.8, 1.2, 0.3, 0.7, 0.9, 0.4), nrow = 4, ncol = 2,
              dimnames = list(funds4, c("mkt", "size")))
  spec <- add.constraint(spec, type = "factor_exposure",
                         B = B, lower = c(0.5, 0.3), upper = c(1.0, 0.8))

  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)
  fe_rows <- df[df$type == "factor_exposure", ]
  expect_equal(nrow(fe_rows), 2)
  expect_true(all(c("mkt", "size") %in% fe_rows$element))
})

test_that("as.data.frame handles unchecked constraints", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")
  spec <- add.constraint(spec, type = "turnover", turnover_target = 0.5)

  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  df <- as.data.frame(report)
  unc_rows <- df[df$status == "unchecked", ]
  expect_equal(nrow(unc_rows), 1)
  expect_equal(unc_rows$type, "turnover")
})

test_that("as.data.frame returns empty data.frame for empty report", {
  # Create minimal report with no violations
  report <- structure(
    list(
      feasible = TRUE,
      tolerance = 1e-8,
      summary = list(total_checked = 0L, total_violations = 0L,
                     violated_count = 0L, violated_types = character(0),
                     binding_count = 0L, binding_types = character(0),
                     unchecked_count = 0L),
      violations = list()
    ),
    class = "feasibility_report"
  )
  df <- as.data.frame(report)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
  expect_true(all(c("type", "element", "status") %in% names(df)))
})

# ============================================================================
# H. print.feasibility_report
# ============================================================================

test_that("print.feasibility_report shows feasible report", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.5)

  w <- c(0.3, 0.3, 0.2, 0.2)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  out <- capture.output(print(report))
  expect_true(any(grepl("ALL CONSTRAINTS SATISFIED", out)))
})

test_that("print.feasibility_report shows violations", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.4)

  w <- c(0.6, 0.2, 0.1, 0.1)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  out <- capture.output(print(report))
  expect_true(any(grepl("VIOLATIONS DETECTED", out)))
  expect_true(any(grepl("Violated constraints", out)))
})

test_that("print.feasibility_report shows binding constraints", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.25, max = 0.25)

  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  out <- capture.output(print(report))
  expect_true(any(grepl("Binding constraints", out)))
})

test_that("print.feasibility_report shows unchecked constraints", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")
  spec <- add.constraint(spec, type = "turnover", turnover_target = 0.5)

  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  out <- capture.output(print(report))
  expect_true(any(grepl("Not checked", out)))
  expect_true(any(grepl("turnover", out)))
})

test_that("print.feasibility_report returns invisible report", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")

  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  result <- capture.output(ret <- print(report))
  expect_s3_class(ret, "feasibility_report")
})

# ============================================================================
# I. check_portfolio_feasibility: group_pos failure
# ============================================================================

test_that("check_portfolio_feasibility detects group_pos violation", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = 1:2, g2 = 3:4),
                         group_min = c(0.2, 0.2),
                         group_max = c(0.8, 0.8),
                         group_pos = c(1, 2))

  # Both assets in group 1 are non-zero, violates group_pos = 1
  w <- c(0.3, 0.2, 0.3, 0.2)
  names(w) <- funds4
  report <- check_portfolio_feasibility(w, spec)
  expect_false(report$feasible)
  expect_false(report$violations$group$details$g1$feasible)
  expect_true(report$violations$group$details$g1$pos_limit_fail)
})
