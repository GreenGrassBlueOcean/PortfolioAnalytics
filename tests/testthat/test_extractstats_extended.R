library(testthat)
library(PortfolioAnalytics)

context("extractStats extended: name.replace, eqwt/invol, opt.list, extractGroups, extractFeasibility")

data(edhec)
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

base_portf <- portfolio.spec(assets = colnames(R5))
base_portf <- add.constraint(base_portf, type = "full_investment")
base_portf <- add.constraint(base_portf, type = "box", min = 0.05, max = 0.55)
base_portf <- add.objective(base_portf, type = "return", name = "mean")
base_portf <- add.objective(base_portf, type = "risk", name = "StdDev")

# ============================================================================
# A. name.replace utility
# ============================================================================

test_that("name.replace cleans redundant objective_measures prefixes", {
  nr <- PortfolioAnalytics:::name.replace
  expect_equal(nr("objective_measures.mean.mean"), "mean")
  expect_equal(nr("objective_measures.StdDev.StdDev"), "StdDev")
  expect_equal(nr("objective_measures.ES.ES"), "ES")
  expect_equal(nr("objective_measures.ETL.ETL"), "ETL")
  expect_equal(nr("objective_measures.VaR.MVaR"), "VaR")
  expect_equal(nr("objective_measures.CVaR.ES"), "CVaR")
  expect_equal(nr("objective_measures.median.median"), "median")
  expect_equal(nr("objective_measures.sd.sd"), "StdDev")
})

test_that("name.replace passes through non-matching names", {
  nr <- PortfolioAnalytics:::name.replace
  expect_equal(nr("w.A"), "w.A")
  expect_equal(nr("out"), "out")
  expect_equal(nr(c("w.A", "w.B")), c("w.A", "w.B"))
})

# ============================================================================
# B. extractStats for eqwt and invol
# ============================================================================

test_that("extractStats.eqwt returns named vector with out and weights", {
  opt_ew <- equal.weight(R5, base_portf)
  stats <- extractStats(opt_ew)
  expect_true(is.numeric(stats))
  expect_true("out" %in% names(stats))
  # eqwt/invol use bare asset names, not w. prefix
  expect_true(all(colnames(R5) %in% names(stats)))
})

test_that("extractStats.invol returns named vector", {
  opt_iv <- inverse.volatility.weight(R5, base_portf)
  stats <- extractStats(opt_iv)
  expect_true(is.numeric(stats))
  expect_true("out" %in% names(stats))
})

test_that("extractWeights.eqwt returns equal weights", {
  opt_ew <- equal.weight(R5, base_portf)
  w <- extractWeights(opt_ew)
  expect_equal(length(w), 5)
  # All weights should be equal
  expect_true(max(w) - min(w) < 1e-10)
})

test_that("extractWeights.invol returns positive weights summing to ~1", {
  opt_iv <- inverse.volatility.weight(R5, base_portf)
  w <- extractWeights(opt_iv)
  expect_equal(length(w), 5)
  expect_true(all(w > 0))
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# C. opt.list: extractStats, extractWeights, extractObjectiveMeasures
# ============================================================================

test_that("extractStats.opt.list returns one list element per optimization", {
  set.seed(3714)
  opt1 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  set.seed(9281)
  opt2 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  opt_list <- combine.optimizations(list(a = opt1, b = opt2))

  stats <- extractStats(opt_list)
  expect_true(is.list(stats))
  expect_equal(length(stats), 2)
  expect_true(is.matrix(stats[[1]]))
})

test_that("extractWeights.opt.list returns matrix with named rows", {
  set.seed(3714)
  opt1 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  set.seed(9281)
  opt2 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  opt_list <- combine.optimizations(list(a = opt1, b = opt2))

  w <- extractWeights(opt_list)
  expect_true(is.matrix(w))
  expect_equal(nrow(w), 2)
  expect_equal(ncol(w), 5)
  expect_equal(rownames(w), c("a", "b"))
})

test_that("extractObjectiveMeasures.opt.list returns matrix", {
  set.seed(3714)
  opt1 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  set.seed(9281)
  opt2 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  opt_list <- combine.optimizations(list(a = opt1, b = opt2))

  obj <- extractObjectiveMeasures(opt_list)
  expect_true(is.matrix(obj))
  expect_equal(nrow(obj), 2)
  expect_true("mean" %in% colnames(obj) || "StdDev" %in% colnames(obj))
})

# ============================================================================
# D. extractGroups
# ============================================================================

test_that("extractGroups returns group_weights when group constraint present", {
  portf_g <- portfolio.spec(assets = colnames(R5))
  portf_g <- add.constraint(portf_g, type = "full_investment")
  portf_g <- add.constraint(portf_g, type = "box", min = 0.05, max = 0.55)
  portf_g <- add.constraint(portf_g, type = "group",
                             groups = list(G1 = 1:2, G2 = 3:5),
                             group_min = c(0.2, 0.3),
                             group_max = c(0.7, 0.8))
  portf_g <- add.objective(portf_g, type = "risk", name = "StdDev")

  set.seed(7314)
  opt_g <- optimize.portfolio(R5, portf_g,
                              optimize_method = "random",
                              search_size = 200, trace = TRUE)
  grp <- extractGroups(opt_g)
  expect_true(is.list(grp))
  expect_equal(length(grp$group_weights), 2)
  expect_equal(sum(grp$group_weights), sum(grp$weights), tolerance = 1e-10)
})

test_that("extractGroups returns category_weights when labels present", {
  # category_labels is a character vector per asset
  portf_c <- portfolio.spec(assets = colnames(R5),
                            category_labels = c("EQ", "EQ", "FI", "FI", "FI"))
  portf_c <- add.constraint(portf_c, type = "full_investment")
  portf_c <- add.constraint(portf_c, type = "box", min = 0.05, max = 0.55)
  portf_c <- add.objective(portf_c, type = "risk", name = "StdDev")

  set.seed(2461)
  opt_c <- optimize.portfolio(R5, portf_c,
                              optimize_method = "random",
                              search_size = 200, trace = TRUE)
  grp <- extractGroups(opt_c)
  expect_equal(names(grp$category_weights), c("EQ", "FI"))
  expect_equal(sum(grp$category_weights), sum(grp$weights), tolerance = 1e-10)
})

test_that("extractGroups returns NULL for both when no groups or categories", {
  set.seed(5120)
  opt <- optimize.portfolio(R5, base_portf,
                            optimize_method = "random",
                            search_size = 100, trace = TRUE)
  grp <- extractGroups(opt)
  expect_null(grp$group_weights)
  expect_null(grp$category_weights)
})

# ============================================================================
# E. extractFeasibility
# ============================================================================

test_that("extractFeasibility returns NULL or feasibility_report", {
  set.seed(8012)
  opt <- optimize.portfolio(R5, base_portf,
                            optimize_method = "random",
                            search_size = 100, trace = TRUE)
  result <- extractFeasibility(opt)
  expect_true(is.null(result) || inherits(result, "feasibility_report"))
})

test_that("extractFeasibility.opt.list returns list", {
  set.seed(3714)
  opt1 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  set.seed(9281)
  opt2 <- optimize.portfolio(R5, base_portf,
                             optimize_method = "random",
                             search_size = 100, trace = TRUE)
  opt_list <- combine.optimizations(list(a = opt1, b = opt2))

  result <- extractFeasibility(opt_list)
  expect_true(is.list(result))
  expect_equal(length(result), 2)
})
