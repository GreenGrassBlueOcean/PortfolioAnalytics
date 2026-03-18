##### test_generics_coverage2.R #####
# Phase 3B coverage: print/summary methods — risk budget nested objectives,
#   "Unconstrained" position limit/diversification/turnover paths,
#   factor exposure display, category labels, non-standard box constraints


data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ===========================================================================
# A. print with risk_budget objective (nested objective measures)
# ===========================================================================

portf_rb <- portfolio.spec(assets = colnames(R5))
portf_rb <- add.constraint(portf_rb, type = "full_investment")
portf_rb <- add.constraint(portf_rb, type = "long_only")
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          min_concentration = TRUE)

set.seed(8412)
opt_rb <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                             search_size = 200, trace = TRUE)

test_that("print handles nested risk_budget objective measures", {
  expect_output(print(opt_rb), "Objective Measure")
})

# ===========================================================================
# B. summary.optimize.portfolio — "Unconstrained" branches
# ===========================================================================

# A minimal optimization with NO position_limit, diversification, or turnover constraints
portf_min <- portfolio.spec(assets = colnames(R5))
portf_min <- add.constraint(portf_min, type = "full_investment")
portf_min <- add.constraint(portf_min, type = "long_only")
portf_min <- add.objective(portf_min, type = "risk", name = "StdDev")
opt_min <- optimize.portfolio(R5, portf_min, optimize_method = "ROI")

test_that("print.summary shows 'Unconstrained' for missing position limits", {
  s <- summary(opt_min)
  out <- capture.output(print(s))
  expect_true(any(grepl("Unconstrained", out)))
})

# ===========================================================================
# C. summary.optimize.portfolio with category_labels (no groups)
# ===========================================================================

test_that("summary with category_labels shows category weights", {
  portf_cat <- portfolio.spec(assets = colnames(R5),
                              category_labels = c("Equity", "Equity",
                                                  "Fixed", "Fixed", "Fixed"))
  portf_cat <- add.constraint(portf_cat, type = "full_investment")
  portf_cat <- add.constraint(portf_cat, type = "long_only")
  portf_cat <- add.objective(portf_cat, type = "risk", name = "StdDev")
  opt_cat <- optimize.portfolio(R5, portf_cat, optimize_method = "ROI")
  s <- summary(opt_cat)
  out <- capture.output(print(s))
  expect_true(any(grepl("Equity|Fixed|Category", out)))
})

# ===========================================================================
# D. print.portfolio — non-standard box constraint labels
# ===========================================================================

test_that("print.portfolio handles custom box constraints (not long-only)", {
  portf_custom <- portfolio.spec(assets = colnames(R5))
  portf_custom <- add.constraint(portf_custom, type = "full_investment")
  portf_custom <- add.constraint(portf_custom, type = "box",
                                 min = 0.05, max = 0.45)
  portf_custom <- add.objective(portf_custom, type = "risk", name = "StdDev")
  out <- capture.output(print(portf_custom))
  expect_true(any(grepl("box", out)))
})

# ===========================================================================
# E. summary.optimize.portfolio with group constraints
# ===========================================================================

test_that("summary with group constraints shows group weights", {
  portf_grp <- portfolio.spec(assets = colnames(R5))
  portf_grp <- add.constraint(portf_grp, type = "full_investment")
  portf_grp <- add.constraint(portf_grp, type = "long_only")
  portf_grp <- add.constraint(portf_grp, type = "group",
                               groups = list(c(1, 2), c(3, 4, 5)),
                               group_min = c(0.2, 0.3),
                               group_max = c(0.6, 0.8))
  portf_grp <- add.objective(portf_grp, type = "risk", name = "StdDev")
  opt_grp <- optimize.portfolio(R5, portf_grp, optimize_method = "ROI")
  s <- summary(opt_grp)
  out <- capture.output(print(s))
  expect_true(any(grepl("Group|group", out)))
})
