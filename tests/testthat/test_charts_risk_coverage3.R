##### test_charts_risk_coverage3.R #####
# Coverage: charts.risk.R remaining uncovered paths

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
colnames(R3) <- c("A", "B", "C")

# --- Build a risk budget optimization (ROI) ---
portf_rb <- portfolio.spec(assets = colnames(R3))
portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.65)
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          min_prisk = rep(0.05, 3),
                          max_prisk = rep(0.60, 3))

set.seed(6183)
opt_rb <- suppressWarnings(
  optimize.portfolio(R3, portf_rb, optimize_method = "random",
                     search_size = 500, trace = TRUE)
)

# --- Build a second optimization with different weights for opt.list ---
portf_rb2 <- portf_rb
portf_rb2 <- add.constraint(portf_rb2, type = "box", min = 0.10, max = 0.60,
                            indexnum = 2)
set.seed(6184)
opt_rb2 <- suppressWarnings(
  optimize.portfolio(R3, portf_rb2, optimize_method = "random",
                     search_size = 500, trace = TRUE)
)
opt_list <- combine.optimizations(list(opt1 = opt_rb, opt2 = opt_rb2))

# ===========================================================================
# 1. chart.RiskBudget.optimize.portfolio — absolute risk type
# ===========================================================================

test_that("chart.RiskBudget absolute risk type for optimize.portfolio", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev", risk.type = "absolute")
  )
})

# ===========================================================================
# 2. Input validation
# ===========================================================================

test_that("chart.RiskBudget.optimize.portfolio errors on wrong class", {
  expect_error(chart.RiskBudget.optimize.portfolio(list()), "class")
})

# ===========================================================================
# 3. Neighbors in percentage mode — single number
# ===========================================================================

test_that("chart.RiskBudget percentage risk type with neighbors=3", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev",
                     risk.type = "percentage", neighbors = 3)
  )
})

# ===========================================================================
# 4. Neighbors in percentage mode — vector
# ===========================================================================

test_that("chart.RiskBudget percentage risk type with vector neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev",
                     risk.type = "percentage", neighbors = c(1, 2, 5))
  )
})

# ===========================================================================
# 5. Neighbors in percentage mode — matrix
# ===========================================================================

test_that("chart.RiskBudget percentage risk type with matrix neighbors", {
  xtract <- extractStats(opt_rb)
  nb <- xtract[1:3, , drop = FALSE]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev",
                     risk.type = "percentage", neighbors = nb)
  )
})

# ===========================================================================
# 6. chart.RiskBudget.opt.list — absolute line plot
# ===========================================================================

test_that("chart.RiskBudget.opt.list absolute line plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "absolute", plot.type = "line",
                     legend.loc = "topright")
  )
})

# ===========================================================================
# 7. chart.RiskBudget.opt.list — percentage line plot
# ===========================================================================

test_that("chart.RiskBudget.opt.list percentage line plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "percentage", plot.type = "line",
                     legend.loc = "topright")
  )
})

# ===========================================================================
# 8. barplotRiskBudget — absolute
# ===========================================================================

test_that("chart.RiskBudget.opt.list absolute barplot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "absolute", plot.type = "bar",
                     legend.loc = "topright")
  )
})

# ===========================================================================
# 9. barplotRiskBudget — percentage
# ===========================================================================

test_that("chart.RiskBudget.opt.list percentage barplot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     risk.type = "percentage", plot.type = "bar",
                     legend.loc = "topright")
  )
})

# ===========================================================================
# 10. min_prisk bounds plotting (line 171)
# ===========================================================================

test_that("chart.RiskBudget draws min_prisk and max_prisk bounds", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev", risk.type = "percentage")
  )
})
