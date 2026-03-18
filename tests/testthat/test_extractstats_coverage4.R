##### test_extractstats_coverage4.R #####
# Coverage: extractstats.R — edge paths: empty DE trace, PSO normalization,
#           divergent objectives, feasibility as.data.frame, regime stats

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:48, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# ===========================================================================
# 1. extractStats.DEoptim empty trace fallback (lines 97-105)
# ===========================================================================

test_that("extractStats.DEoptim returns single-row matrix when trace is empty", {
  mock_de <- list(
    objective_measures = list(mean = 0.005, StdDev = 0.02),
    out = 0.015,
    weights = c(A = 0.25, B = 0.25, C = 0.25, D = 0.25),
    DEoptim_objective_results = list()
  )
  class(mock_de) <- c("optimize.portfolio.DEoptim", "optimize.portfolio")

  result <- extractStats(mock_de)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 1)
  expect_true("out" %in% colnames(result))
  expect_true(any(grepl("w\\.", colnames(result))))
})

# ===========================================================================
# 2. extractStats.pso normalize_weights (lines 177-212)
# ===========================================================================

test_that("extractStats.pso normalizes weights with leverage constraints", {
  skip_if_not_installed("pso")

  portf_lev <- portfolio.spec(assets = colnames(R4))
  portf_lev <- add.constraint(portf_lev, type = "weight_sum",
                              min_sum = 0.95, max_sum = 1.05)
  portf_lev <- add.constraint(portf_lev, type = "box", min = 0.05, max = 0.65)
  portf_lev <- add.objective(portf_lev, type = "risk", name = "StdDev")

  set.seed(4218)
  opt_pso <- suppressWarnings(suppressMessages(
    optimize.portfolio(R4, portf_lev, optimize_method = "pso", trace = TRUE)
  ))
  result <- extractStats(opt_pso)
  expect_true(is.matrix(result))
  expect_true(nrow(result) > 1)
  # Weight columns should exist
  wt_cols <- grep("^w\\.", colnames(result))
  expect_true(length(wt_cols) == 4)
})

# ===========================================================================
# 3. extractObjectiveMeasures.opt.list divergent objectives (lines 639-694)
# ===========================================================================

test_that("extractObjectiveMeasures recalculates for divergent objectives", {
  portf1 <- portfolio.spec(assets = colnames(R4))
  portf1 <- add.constraint(portf1, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
  portf1 <- add.constraint(portf1, type = "box", min = 0.05, max = 0.65)
  portf1 <- add.objective(portf1, type = "return", name = "mean")

  portf2 <- portfolio.spec(assets = colnames(R4))
  portf2 <- add.constraint(portf2, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
  portf2 <- add.constraint(portf2, type = "box", min = 0.05, max = 0.65)
  portf2 <- add.objective(portf2, type = "return", name = "mean")
  portf2 <- add.objective(portf2, type = "risk", name = "StdDev")

  opt1 <- suppressWarnings(optimize.portfolio(R4, portf1,
                                               optimize_method = "ROI",
                                               trace = TRUE))
  opt2 <- suppressWarnings(optimize.portfolio(R4, portf2,
                                               optimize_method = "ROI",
                                               trace = TRUE))
  opt_list <- combine.optimizations(list(opt1 = opt1, opt2 = opt2))

  result <- extractObjectiveMeasures(opt_list)
  # Should have recalculated both with combined objectives
  expect_true(is.matrix(result) || is.numeric(result))
  expect_equal(nrow(result), 2)
  # Both portfolios should have StdDev now (from the combined objectives)
  expect_true(any(grepl("StdDev", colnames(result))))
})

# ===========================================================================
# 4. extractFeasibility.opt.list with as.data.frame=TRUE (lines 835-844)
# ===========================================================================

test_that("extractFeasibility.opt.list returns data.frame", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt1 <- suppressWarnings(optimize.portfolio(R4, portf,
                                               optimize_method = "ROI"))
  opt2 <- suppressWarnings(optimize.portfolio(R4, portf,
                                               optimize_method = "ROI"))
  opt_list <- combine.optimizations(list(o1 = opt1, o2 = opt2))

  result <- extractFeasibility(opt_list, as.data.frame = TRUE)
  # If feasibility reports are NULL, should get empty data.frame
  # If present, should be a data.frame
  expect_true(is.data.frame(result))
})

# ===========================================================================
# 5. extractFeasibility.optimize.portfolio.rebalancing as.data.frame
#    (lines 815-827)
# ===========================================================================

test_that("extractFeasibility.rebalancing returns data.frame", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt_rebal <- suppressWarnings(
    optimize.portfolio.rebalancing(R4, portf,
                                    optimize_method = "ROI",
                                    rebalance_on = "years",
                                    training_period = 36)
  )
  result <- extractFeasibility(opt_rebal, as.data.frame = TRUE)
  expect_true(is.data.frame(result))
})

# ===========================================================================
# 6. extractStatsRegime (lines 302-326) — mocked regime rebalancing
# ===========================================================================

test_that("extractStats dispatches to extractStatsRegime for regime portfolios", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # Build a real rebalancing object first
  opt_rebal <- suppressWarnings(
    optimize.portfolio.rebalancing(R4, portf,
                                    optimize_method = "ROI",
                                    rebalance_on = "years",
                                    training_period = 36)
  )

  # Manually inject regime info to trigger extractStatsRegime
  for (i in seq_along(opt_rebal$opt_rebalancing)) {
    opt_rebal$opt_rebalancing[[i]]$regime <- ifelse(i %% 2 == 1, 1, 2)
  }
  regime_portf <- portf
  class(regime_portf) <- c("regime.portfolios", class(regime_portf))
  opt_rebal$portfolio <- regime_portf

  result <- extractStats(opt_rebal)
  expect_true(is.list(result))
  # Should have entries for each unique regime
  expect_true(length(result) >= 1)
  expect_true(all(grepl("^regime", names(result))))
})
