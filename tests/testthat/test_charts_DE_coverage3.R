##### test_charts_DE_coverage3.R #####
# Coverage: charts.DE.R remaining uncovered paths

skip_if_not_installed("DEoptim")

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "risk", name = "ES")
portf <- add.objective(portf, type = "return", name = "mean")

set.seed(9142)
opt_de <- suppressWarnings(suppressMessages(
  optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                     search_size = 500, trace = TRUE,
                     DEoptim.control = list(itermax = 25, trace = FALSE))
))

# ===========================================================================
# 1. Input validation error paths
# ===========================================================================

test_that("chart.Weight.DE errors on wrong class", {
  expect_error(chart.Weight.DE(list()), "class")
})

test_that("chart.Scatter.DE errors on wrong class", {
  expect_error(chart.Scatter.DE(list()), "class")
})

test_that("chart.Scatter.DE errors when R is NULL (no trace)", {
  obj <- opt_de
  obj$R <- NULL
  expect_error(chart.Scatter.DE(obj), "Returns object not detected")
})

# ===========================================================================
# 2. Long column names → bottommargin > 10 truncation (lines 35-36)
# ===========================================================================

test_that("chart.Weight.DE truncates long column names", {
  obj <- opt_de
  long_names <- paste0("VeryLongAssetNameThatExceedsTwenty_", LETTERS[1:5])
  names(obj$weights) <- long_names
  obj$portfolio$assets <- setNames(rep(1/5, 5), long_names)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(obj))
})

# ===========================================================================
# 3. Custom return/risk col not in extractStats → applyFUN path (lines 127-152)
# ===========================================================================

test_that("chart.Scatter.DE with custom risk metric uses applyFUN path", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # "StdDev" is not in the DEoptim extractStats output (which has mean + ES)
  expect_warning(
    chart.RiskReward(opt_de, risk.col = "StdDev"),
    "do  not match"
  )
})

# ===========================================================================
# 4. constrained_objective result slot (line 271)
# ===========================================================================

test_that("chart.Scatter.DE uses constrained_objective slot when present", {
  obj <- opt_de
  obj$constrained_objective <- obj$objective_measures
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(obj))
})

# ===========================================================================
# 5. Optimal point custom applyFUN path (lines 290-297)
# ===========================================================================

test_that("chart.Scatter.DE computes optimal point via applyFUN for non-standard metrics", {
  obj <- opt_de
  # Remove objective_measures so the optimal point lookup falls through
  obj$objective_measures <- list(custom_thing = 42)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(obj))
})

# ===========================================================================
# 6. Trajectory missing (R or portfolio NULL) → message (line 265)
# ===========================================================================

test_that("chart.Scatter.DE shows message when portfolio is NULL", {
  obj <- opt_de
  obj$portfolio <- NULL
  obj$DEoutput <- NULL
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_message(
    chart.RiskReward(obj),
    "Trajectory cannot be drawn"
  )
})

# ===========================================================================
# 7. las <= 1 path (line 41) — already partly tested, ensure minmargin used
# ===========================================================================

test_that("chart.Weight.DE with las=0 uses minmargin for bottom", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_de, las = 0))
})
