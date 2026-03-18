##### test_generics_coverage4.R #####
# Coverage: generics.R — print.portfolio.spec constraint display (enabled &
# disabled box types), risk budget objective printing in print methods for
# ROI/pso/random solvers, and summary.optimize.portfolio constraint details.

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]

# ===========================================================================
# 1. print.portfolio.spec — enabled constraint box type display
# ===========================================================================

test_that("print.portfolio.spec shows box (long only)", {
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  out <- capture.output(print(p))
  expect_true(any(grepl("box \\(long only\\)", out)))
})

test_that("print.portfolio.spec shows box (with shorting)", {
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "box", min = -0.5, max = 1)
  out <- capture.output(print(p))
  expect_true(any(grepl("box \\(with shorting\\)", out)))
})

test_that("print.portfolio.spec shows generic box type", {
  # min > 0 and max < 1, no shorting — falls through to else
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.8)
  out <- capture.output(print(p))
  expect_true(any(grepl("box", out)))
})

# ===========================================================================
# 2. print.portfolio.spec — disabled constraint display
# ===========================================================================

test_that("print.portfolio.spec shows disabled box (long only)", {
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "box", min = 0, max = 1, enabled = FALSE)
  out <- capture.output(print(p))
  expect_true(any(grepl("Disabled constraint types", out)))
  expect_true(any(grepl("box \\(long only\\)", out)))
})

test_that("print.portfolio.spec shows disabled box (with shorting)", {
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "box", min = -0.5, max = 1, enabled = FALSE)
  out <- capture.output(print(p))
  expect_true(any(grepl("box \\(with shorting\\)", out)))
})

test_that("print.portfolio.spec shows disabled non-box constraint", {
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1), group_max = c(0.9, 0.9),
                      enabled = FALSE)
  out <- capture.output(print(p))
  expect_true(any(grepl("Disabled constraint types", out)))
  expect_true(any(grepl("group", out)))
})

# ===========================================================================
# 3. print.optimize.portfolio.ROI — risk budget objective printing
# ===========================================================================

test_that("print.optimize.portfolio.ROI prints risk budget details", {
  # Construct a mock ROI result with multi-element objective_measures
  mock_roi <- list(
    call = quote(optimize.portfolio(R, portfolio, optimize_method = "ROI")),
    weights = c(A = 0.3, B = 0.3, C = 0.2, D = 0.2),
    objective_measures = list(
      StdDev = list(StdDev = 0.05,
                    pct_contrib_StdDev = c(A = 0.25, B = 0.25, C = 0.25, D = 0.25))
    )
  )
  class(mock_roi) <- "optimize.portfolio.ROI"
  out <- capture.output(print(mock_roi))
  expect_true(any(grepl("pct_contrib_StdDev", out)))
})

# ===========================================================================
# 4. print.optimize.portfolio.pso — risk budget objective printing
# ===========================================================================

test_that("print.optimize.portfolio.pso prints risk budget details", {
  mock_pso <- list(
    call = quote(optimize.portfolio(R, portfolio, optimize_method = "pso")),
    weights = c(A = 0.3, B = 0.3, C = 0.2, D = 0.2),
    objective_measures = list(
      ES = list(ES = 0.08,
                pct_contrib_ES = c(A = 0.3, B = 0.2, C = 0.3, D = 0.2))
    )
  )
  class(mock_pso) <- "optimize.portfolio.pso"
  out <- capture.output(print(mock_pso))
  expect_true(any(grepl("pct_contrib_ES", out)))
})

# ===========================================================================
# 5. summary.optimize.portfolio — risk budget in objective_values
# ===========================================================================

test_that("summary.optimize.portfolio prints risk budget in objective_values", {
  mock_summary <- list(
    call = quote(optimize.portfolio(R, portfolio, optimize_method = "random")),
    weights = c(A = 0.3, B = 0.3, C = 0.2, D = 0.2),
    initial_weights = c(A = 0.25, B = 0.25, C = 0.25, D = 0.25),
    objective_values = list(
      StdDev = list(StdDev = 0.04,
                    pct_contrib_StdDev = c(A = 0.2, B = 0.3, C = 0.3, D = 0.2))
    ),
    out = 0.04,
    elapsed_time = 1.0,
    end_t = Sys.time(),
    # Constraint detail slots
    leverage_constraint = list(min_sum = 0.99, max_sum = 1.01, actual = 1.0),
    box_constraint = list(min = c(0, 0, 0, 0), max = c(1, 1, 1, 1)),
    group_constraint = list(groups = NULL),
    position_limit_constraint = list(
      max_pos = 4, max_pos_actual = 4,
      max_pos_long = 4, max_pos_long_actual = 4,
      max_pos_short = 0, max_pos_short_actual = 0
    ),
    diversification_constraint = list(
      diversification_target = 0.7, diversification_actual = 0.75
    ),
    turnover_constraint = list(
      turnover_target = 0.2, turnover_actual = 0.15
    ),
    factor_exposure_constraint = NULL
  )
  class(mock_summary) <- "summary.optimize.portfolio"
  out <- capture.output(print(mock_summary))
  # Risk budget printing
  expect_true(any(grepl("pct_contrib_StdDev", out)))
  # Position limit printing
  expect_true(any(grepl("Position Limit", out)))
  # Diversification target
  expect_true(any(grepl("Diversification", out)))
  # Turnover target
  expect_true(any(grepl("Turnover", out)))
})
