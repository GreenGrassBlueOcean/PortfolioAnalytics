##### test_charts_risk_coverage5.R #####
# Coverage: charts.risk.R — main="", las=1, multiple risk_budget_objectives

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
colnames(R3) <- c("A", "B", "C")

# --- Shared setup: risk budget portfolio + opt.list ---
portf_rb <- portfolio.spec(assets = colnames(R3))
portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.65)
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          min_concentration = TRUE)

set.seed(5217)
opt1 <- suppressWarnings(
  optimize.portfolio(R3, portf_rb, optimize_method = "random",
                     search_size = 200, trace = TRUE)
)
set.seed(5218)
opt2 <- suppressWarnings(
  optimize.portfolio(R3, portf_rb, optimize_method = "random",
                     search_size = 200, trace = TRUE)
)
opt_list <- combine.optimizations(list(opt1 = opt1, opt2 = opt2))

# ===========================================================================
# 1. opt.list line plot with main="" and las=1 (L284, L294, L334, L344)
# ===========================================================================

test_that("chart.RiskBudget opt.list absolute line with main='' and las=1", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "absolute", plot.type = "line",
                     main = "", las = 1)
  )
})

test_that("chart.RiskBudget opt.list percentage line with main='' and las=1", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "percentage", plot.type = "line",
                     main = "", las = 1)
  )
})

# ===========================================================================
# 2. barplot with main="" and las=1 (L388, L398, L434, L444)
# ===========================================================================

test_that("chart.RiskBudget opt.list absolute barplot with main='' and las=1", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "absolute", plot.type = "bar",
                     main = "", las = 1)
  )
})

test_that("chart.RiskBudget opt.list percentage barplot with main='' and las=1", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "percentage", plot.type = "bar",
                     main = "", las = 1)
  )
})

# ===========================================================================
# 3. Multiple risk_budget_objectives message (L72)
# ===========================================================================

test_that("chart.RiskBudget messages when multiple risk_budget_objectives", {
  portf_multi <- portfolio.spec(assets = colnames(R3))
  portf_multi <- add.constraint(portf_multi, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
  portf_multi <- add.constraint(portf_multi, type = "box", min = 0.05, max = 0.65)
  portf_multi <- add.objective(portf_multi, type = "risk_budget", name = "StdDev",
                               min_concentration = TRUE)
  portf_multi <- add.objective(portf_multi, type = "risk_budget", name = "ES",
                               min_concentration = TRUE)

  set.seed(5219)
  opt_multi <- suppressWarnings(
    optimize.portfolio(R3, portf_multi, optimize_method = "random",
                       search_size = 200, trace = TRUE)
  )

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_message(
    chart.RiskBudget(opt_multi, risk.type = "absolute"),
    "risk_budget_objectives"
  )
})
