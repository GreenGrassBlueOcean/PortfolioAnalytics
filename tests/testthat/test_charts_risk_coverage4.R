##### test_charts_risk_coverage4.R #####
# Coverage: charts.risk.R — barplotRiskBudget (absolute + percentage),
#           neighbors in absolute mode (single, vector, matrix),
#           long-name truncation in barplot

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
colnames(R3) <- c("A", "B", "C")

# --- Risk budget optimization (random) for single-object neighbor tests ---
portf_rb <- portfolio.spec(assets = colnames(R3))
portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.65)
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          min_prisk = rep(0.05, 3),
                          max_prisk = rep(0.60, 3))

set.seed(7391)
opt_rb <- suppressWarnings(
  optimize.portfolio(R3, portf_rb, optimize_method = "random",
                     search_size = 500, trace = TRUE)
)

# --- Second optimization for opt.list ---
portf_rb2 <- portf_rb
portf_rb2 <- add.constraint(portf_rb2, type = "box", min = 0.10, max = 0.60,
                            indexnum = 2)
set.seed(7392)
opt_rb2 <- suppressWarnings(
  optimize.portfolio(R3, portf_rb2, optimize_method = "random",
                     search_size = 500, trace = TRUE)
)
opt_list <- combine.optimizations(list(opt1 = opt_rb, opt2 = opt_rb2))

# ===========================================================================
# 1. barplotRiskBudget — absolute (lines 371-415)
# ===========================================================================

test_that("barplotRiskBudget absolute with legend", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     plot.type = "bar", risk.type = "absolute",
                     legend.loc = "topright")
  )
})

# ===========================================================================
# 2. barplotRiskBudget — percentage (lines 417-460)
# ===========================================================================

test_that("barplotRiskBudget percentage with legend", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list, match.col = "StdDev",
                     plot.type = "bar", risk.type = "percentage",
                     legend.loc = "topright")
  )
})

# ===========================================================================
# 3. Neighbors in absolute mode — single integer (lines 123-132)
# ===========================================================================

test_that("chart.RiskBudget absolute with single-number neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev",
                     risk.type = "absolute", neighbors = 3)
  )
})

# ===========================================================================
# 4. Neighbors in absolute mode — vector (lines 133-137)
# ===========================================================================

test_that("chart.RiskBudget absolute with vector neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev",
                     risk.type = "absolute", neighbors = c(1, 2, 5))
  )
})

# ===========================================================================
# 5. Neighbors in absolute mode — matrix (lines 139-147)
# ===========================================================================

test_that("chart.RiskBudget absolute with matrix neighbors", {
  xtract <- extractStats(opt_rb)
  nb <- xtract[1:3, , drop = FALSE]
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev",
                     risk.type = "absolute", neighbors = nb)
  )
})

# ===========================================================================
# 6. barplotRiskBudget with long column names → truncation (bottommargin>10)
# ===========================================================================

test_that("barplotRiskBudget truncates long column names", {
  # Create opt.list with very long asset names
  R_long <- R3
  long_names <- paste0("VeryLongStrategyNameForAsset_", LETTERS[1:3])
  colnames(R_long) <- long_names

  portf_long <- portfolio.spec(assets = long_names)
  portf_long <- add.constraint(portf_long, type = "weight_sum",
                               min_sum = 0.99, max_sum = 1.01)
  portf_long <- add.constraint(portf_long, type = "box",
                               min = 0.05, max = 0.65)
  portf_long <- add.objective(portf_long, type = "return", name = "mean")
  portf_long <- add.objective(portf_long, type = "risk_budget",
                              name = "StdDev",
                              min_prisk = rep(0.05, 3),
                              max_prisk = rep(0.60, 3))

  set.seed(7393)
  opt_long1 <- suppressWarnings(
    optimize.portfolio(R_long, portf_long, optimize_method = "random",
                       search_size = 500, trace = TRUE)
  )
  set.seed(7394)
  opt_long2 <- suppressWarnings(
    optimize.portfolio(R_long, portf_long, optimize_method = "random",
                       search_size = 500, trace = TRUE)
  )
  opt_list_long <- combine.optimizations(list(o1 = opt_long1, o2 = opt_long2))

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list_long, match.col = "StdDev",
                     plot.type = "line", risk.type = "absolute",
                     legend.loc = "topright")
  )
})
