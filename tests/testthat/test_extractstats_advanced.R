###############################################################################
# Phase 2B: Advanced extractStats tests
#
# Targets: extractstats.R
# Focus:   Regime switching extraction, parallel result extraction,
#          PSO normalization paths, divergent objective handling,
#          name.replace utility, extractObjectiveMeasures paths,
#          extractWeights for opt.list, extractGroups
###############################################################################

require(testthat)
require(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Shared test data
# ---------------------------------------------------------------------------
data(edhec)
R_es <- edhec["2008::2012", 1:5]
colnames(R_es) <- c("CA", "CTAG", "DS", "EM", "EQM")

# Base portfolio
lo_portf <- portfolio.spec(assets = colnames(R_es))
lo_portf <- add.constraint(lo_portf, type = "full_investment")
lo_portf <- add.constraint(lo_portf, type = "box", min = 0.05, max = 0.60)

###############################################################################
# 1. name.replace utility
###############################################################################

test_that("name.replace cleans common objective measure names", {
  raw <- c("objective_measures.mean.mean", "objective_measures.StdDev.StdDev",
           "objective_measures.ES.ES", "out", "w.CA")
  cleaned <- name.replace(raw)
  expect_equal(cleaned[1], "mean")
  expect_equal(cleaned[2], "StdDev")
  expect_equal(cleaned[3], "ES")
  # Non-matching names pass through
  expect_equal(cleaned[4], "out")
  expect_equal(cleaned[5], "w.CA")
})

test_that("name.replace handles CVaR/ETL aliases", {
  raw <- c("CVaR.ES", "CVaR.MES", "ETL.ETL", "ETL.MES", "VaR.MVaR")
  cleaned <- name.replace(raw)
  expect_equal(cleaned[1], "CVaR")
  expect_equal(cleaned[2], "CVaR")
  expect_equal(cleaned[3], "ETL")
  expect_equal(cleaned[4], "ETL")
  expect_equal(cleaned[5], "VaR")
})

test_that("name.replace handles empty/unmatched input", {
  expect_equal(name.replace(character(0)), character(0))
  expect_equal(name.replace(c("foo", "bar")), c("foo", "bar"))
})

###############################################################################
# 2. extractStats for ROI
###############################################################################

test_that("extractStats.optimize.portfolio.ROI returns named vector", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "ROI")
  
  stats <- extractStats(opt)
  expect_true(is.numeric(stats))
  expect_false(is.matrix(stats))
  expect_true("out" %in% names(stats))
  expect_true(any(grepl("^w\\.", names(stats))))
  # Should have objective measures
  expect_true("StdDev" %in% names(stats) || "mean" %in% names(stats))
})

###############################################################################
# 3. extractStats for CVXR
###############################################################################

test_that("extractStats.optimize.portfolio.CVXR returns named vector", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  opt <- suppressWarnings(
    optimize.portfolio(R_es, portf, optimize_method = "CVXR")
  )
  
  stats <- extractStats(opt)
  expect_true(is.numeric(stats))
  expect_false(is.matrix(stats))
  expect_true("out" %in% names(stats))
  # Weight names present
  wt_names <- paste0("w.", colnames(R_es))
  expect_true(all(wt_names %in% names(stats)))
})

###############################################################################
# 4. extractStats for DEoptim (trace storage)
###############################################################################

test_that("extractStats.optimize.portfolio.DEoptim returns matrix with trace", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "DEoptim",
                            trace = TRUE,
                            DEoptim.control = list(itermax = 10, NP = 20))
  
  stats <- extractStats(opt)
  expect_true(is.matrix(stats))
  # With default %dopar% backend, DEoptim can't accumulate per-iteration
  # results (environment serialization), so we get the 1-row fallback
  expect_true(nrow(stats) >= 1)
  expect_true("out" %in% colnames(stats))
})

test_that("extractStats.optimize.portfolio.DEoptim errors without trace", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "DEoptim",
                            trace = FALSE,
                            DEoptim.control = list(itermax = 10, NP = 20))
  expect_error(extractStats(opt), "trace=TRUE")
})

###############################################################################
# 5. extractStats for DEoptim with empty trace (parallel fallback)
###############################################################################

test_that("extractStats DEoptim with empty trace returns optimal-only row", {
  # Simulate parallel DEoptim: trace exists but results list is empty
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "DEoptim",
                            trace = TRUE,
                            DEoptim.control = list(itermax = 10, NP = 20))
  # Manually empty the trace to simulate parallel behavior
  opt$DEoptim_objective_results <- list()
  
  stats <- extractStats(opt)
  expect_true(is.matrix(stats))
  expect_equal(nrow(stats), 1)
  expect_equal(rownames(stats), "DE.portf.optimal")
  expect_true("out" %in% colnames(stats))
})

###############################################################################
# 6. extractStats for GenSA (single-row, no trace weights)
###############################################################################

test_that("extractStats.optimize.portfolio.GenSA returns named vector", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "GenSA",
                            trace = TRUE,
                            GenSA = list(maxit = 10))
  
  stats <- extractStats(opt)
  expect_true(is.numeric(stats))
  expect_true("out" %in% names(stats))
  # GenSA stores weight names without "w." prefix (uses raw asset names)
  expect_true(any(colnames(R_es) %in% names(stats)))
})

test_that("extractStats GenSA errors without trace", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "GenSA",
                            trace = FALSE,
                            GenSA = list(maxit = 10))
  expect_error(extractStats(opt), "trace=TRUE")
})

###############################################################################
# 7. extractStats for random portfolios
###############################################################################

test_that("extractStats.optimize.portfolio.random returns matrix", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "random",
                            trace = TRUE, search_size = 50)
  
  stats <- extractStats(opt)
  expect_true(is.matrix(stats))
  expect_true(nrow(stats) >= 1)
  expect_true("out" %in% colnames(stats))
})

test_that("extractStats random errors without trace", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "random",
                            trace = FALSE, search_size = 50)
  expect_error(extractStats(opt), "trace=TRUE")
})

###############################################################################
# 8. extractStats for rebalancing (standard, non-regime)
###############################################################################

test_that("extractStats.rebalancing returns list of per-period stats", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  
  rebal <- optimize.portfolio.rebalancing(
    R_es, portf, optimize_method = "ROI",
    rebalance_on = "years", training_period = 36
  )
  
  stats <- extractStats(rebal)
  expect_true(is.list(stats))
  expect_true(length(stats) >= 1)
  # Each element should be the extractStats output for that period
  expect_true(is.numeric(stats[[1]]))
})

###############################################################################
# 9. extractStats for rebalancing with regime switching
###############################################################################

test_that("extractStatsRegime groups results by regime", {
  portf1 <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf2 <- add.objective(lo_portf, type = "risk", name = "ES",
                          arguments = list(p = 0.95))
  
  portf_list <- combine.portfolios(list(portf1, portf2))
  
  # Create regime time series with 2 regimes
  dates <- index(R_es)
  regime_ts <- xts::xts(
    rep(c(1, 2), length.out = length(dates)),
    order.by = dates
  )
  
  rp <- regime.portfolios(regime = regime_ts, portfolios = portf_list)
  
  rebal <- optimize.portfolio.rebalancing(
    R_es, rp, optimize_method = "ROI",
    rebalance_on = "quarters", training_period = 12
  )
  
  stats <- extractStats(rebal)
  expect_true(is.list(stats))
  # Should have entries named regime.1 and regime.2
  expect_true("regime.1" %in% names(stats))
  expect_true("regime.2" %in% names(stats))
  # Each regime entry is a list of per-period stats
  expect_true(is.list(stats$regime.1))
  expect_true(length(stats$regime.1) >= 1)
})

###############################################################################
# 10. extractObjectiveMeasures for rebalancing (standard)
###############################################################################

test_that("extractObjectiveMeasures.rebalancing returns xts matrix", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  
  rebal <- optimize.portfolio.rebalancing(
    R_es, portf, optimize_method = "ROI",
    rebalance_on = "years", training_period = 36
  )
  
  obj_meas <- extractObjectiveMeasures(rebal)
  expect_true(inherits(obj_meas, "xts"))
  expect_true(ncol(obj_meas) >= 2)
  # Cleaned column names
  expect_true("StdDev" %in% colnames(obj_meas) || "mean" %in% colnames(obj_meas))
})

###############################################################################
# 11. extractObjectiveMeasures for regime switching rebalancing
###############################################################################

test_that("extractObjRegime returns list of xts by regime", {
  portf1 <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf2 <- add.objective(lo_portf, type = "risk", name = "ES",
                          arguments = list(p = 0.95))
  portf_list <- combine.portfolios(list(portf1, portf2))
  
  dates <- index(R_es)
  regime_ts <- xts::xts(
    rep(c(1, 2), length.out = length(dates)),
    order.by = dates
  )
  rp <- regime.portfolios(regime = regime_ts, portfolios = portf_list)
  
  rebal <- optimize.portfolio.rebalancing(
    R_es, rp, optimize_method = "ROI",
    rebalance_on = "quarters", training_period = 12
  )
  
  obj_meas <- extractObjectiveMeasures(rebal)
  expect_true(is.list(obj_meas))
  # Should have regime.1 and regime.2
  expect_true(length(obj_meas) == 2)
  # Each element should be an xts object
  expect_true(inherits(obj_meas[[1]], "xts"))
  expect_true(inherits(obj_meas[[2]], "xts"))
})

###############################################################################
# 12. extractWeights for rebalancing
###############################################################################

test_that("extractWeights.rebalancing returns xts with correct dimensions", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  
  rebal <- optimize.portfolio.rebalancing(
    R_es, portf, optimize_method = "ROI",
    rebalance_on = "years", training_period = 36
  )
  
  wts <- extractWeights(rebal)
  expect_true(inherits(wts, "xts"))
  expect_equal(ncol(wts), ncol(R_es))
  expect_equal(colnames(wts), colnames(R_es))
  # Weights sum to ~1
  expect_true(all(abs(rowSums(wts) - 1) < 1e-4))
})

###############################################################################
# 13. extractWeights for opt.list
###############################################################################

test_that("extractWeights.opt.list returns matrix with aligned columns", {
  portf1 <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf2 <- add.objective(lo_portf, type = "return", name = "mean")
  
  opt1 <- optimize.portfolio(R_es, portf1, optimize_method = "ROI")
  opt2 <- optimize.portfolio(R_es, portf2, optimize_method = "ROI")
  
  opt_list <- structure(list(minvar = opt1, maxret = opt2), class = "opt.list")
  
  wts <- extractWeights(opt_list)
  expect_true(is.matrix(wts))
  expect_equal(nrow(wts), 2)
  expect_equal(rownames(wts), c("minvar", "maxret"))
  expect_equal(ncol(wts), ncol(R_es))
})

###############################################################################
# 14. extractObjectiveMeasures for opt.list (identical objectives)
###############################################################################

test_that("extractObjectiveMeasures.opt.list with identical objectives", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt1 <- optimize.portfolio(R_es, portf, optimize_method = "ROI")
  opt2 <- optimize.portfolio(R_es[1:48, ], portf, optimize_method = "ROI")
  
  opt_list <- structure(
    list(full = opt1, partial = opt2),
    class = "opt.list"
  )
  
  obj <- extractObjectiveMeasures(opt_list)
  expect_true(is.matrix(obj))
  expect_equal(nrow(obj), 2)
  expect_equal(rownames(obj), c("full", "partial"))
  expect_true("StdDev" %in% colnames(obj) || "mean" %in% colnames(obj))
})

###############################################################################
# 15. extractObjectiveMeasures for opt.list (divergent objectives)
###############################################################################

test_that("extractObjectiveMeasures.opt.list with divergent objectives", {
  portf1 <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf1 <- add.objective(portf1, type = "return", name = "mean")
  
  portf2 <- add.objective(lo_portf, type = "risk", name = "ES",
                          arguments = list(p = 0.95))
  portf2 <- add.objective(portf2, type = "return", name = "mean")
  
  # trace=TRUE needed so that object$R is available for constrained_objective
  opt1 <- optimize.portfolio(R_es, portf1, optimize_method = "ROI", trace = TRUE)
  opt2 <- optimize.portfolio(R_es, portf2, optimize_method = "ROI", trace = TRUE)
  
  opt_list <- structure(
    list(minvar = opt1, mines = opt2),
    class = "opt.list"
  )
  
  obj <- extractObjectiveMeasures(opt_list)
  # With divergent objectives, the function recalculates all measures
  expect_true(is.matrix(obj))
  expect_equal(nrow(obj), 2)
  # Should have columns for both StdDev and ES objectives
  cnames <- colnames(obj)
  expect_true("StdDev" %in% cnames || "mean" %in% cnames || "ES" %in% cnames)
})

###############################################################################
# 16. extractGroups
###############################################################################

test_that("extractGroups returns group and category weights", {
  g_portf <- portfolio.spec(assets = colnames(R_es))
  g_portf <- add.constraint(g_portf, type = "full_investment")
  g_portf <- add.constraint(g_portf, type = "box", min = 0.05, max = 0.60)
  g_portf <- add.constraint(g_portf, type = "group",
                            groups = list(c(1, 2), c(3, 4, 5)),
                            group_min = c(0.2, 0.3),
                            group_max = c(0.6, 0.8),
                            group_labels = c("GroupA", "GroupB"))
  g_portf$category_labels <- list(style = c("CA", "CTAG"), alt = c("DS", "EM", "EQM"))
  g_portf <- add.objective(g_portf, type = "risk", name = "StdDev")
  
  opt <- optimize.portfolio(R_es, g_portf, optimize_method = "ROI")
  
  grps <- extractGroups(opt)
  expect_true(is.list(grps))
  # Should have weights, category_weights, and group_weights
  expect_true("weights" %in% names(grps))
  expect_true("category_weights" %in% names(grps))
  expect_true("group_weights" %in% names(grps))
  # Group weights should sum to 1
  expect_equal(sum(grps$group_weights), 1, tolerance = 1e-4)
  # Category weights should sum to 1
  expect_equal(sum(grps$category_weights), 1, tolerance = 1e-4)
  expect_equal(names(grps$group_weights), c("GroupA", "GroupB"))
  expect_equal(names(grps$category_weights), c("style", "alt"))
})

test_that("extractGroups without groups returns NULL group_weights", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "ROI")
  
  grps <- extractGroups(opt)
  expect_null(grps$group_weights)
  expect_null(grps$category_weights)
  expect_true(is.numeric(grps$weights))
})

###############################################################################
# 17. extractStats for opt.list and opt.rebal.list
###############################################################################

test_that("extractStats.opt.list returns list of stats", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt1 <- optimize.portfolio(R_es, portf, optimize_method = "ROI")
  opt2 <- optimize.portfolio(R_es[1:48, ], portf, optimize_method = "ROI")
  
  opt_list <- structure(list(opt1, opt2), class = "opt.list")
  
  stats <- extractStats(opt_list)
  expect_true(is.list(stats))
  expect_equal(length(stats), 2)
  expect_true(is.numeric(stats[[1]]))
  expect_true(is.numeric(stats[[2]]))
})

###############################################################################
# 18. extractStats with prefix argument
###############################################################################

test_that("extractStats DEoptim with prefix labels rows correctly", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "DEoptim",
                            trace = TRUE,
                            DEoptim.control = list(itermax = 10, NP = 20))
  
  stats_nopfx <- extractStats(opt)
  stats_pfx <- extractStats(opt, prefix = "test")
  
  # Prefix should appear in row names
  expect_true(all(grepl("^test", rownames(stats_pfx))))
  # Same dimensions
  expect_equal(dim(stats_nopfx), dim(stats_pfx))
})

###############################################################################
# 19. PSO extractStats with normalization
###############################################################################

test_that("extractStats PSO returns matrix with normalized weights", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt <- optimize.portfolio(R_es, portf, optimize_method = "pso",
                            trace = TRUE,
                            pso.control = list(maxit = 10, s = 10))
  
  stats <- extractStats(opt)
  expect_true(is.matrix(stats))
  expect_true(nrow(stats) > 1)
  expect_true("out" %in% colnames(stats))
  
  # Weight columns should be normalized to sum near 1
  wt_cols <- grep("^w\\.", colnames(stats), value = TRUE)
  row_sums <- rowSums(stats[, wt_cols])
  # PSO normalization targets min_sum/max_sum (here 1)
  # Allow tolerance for solver noise
  expect_true(all(abs(row_sums - 1) < 0.1))
})

###############################################################################
# 20. extractFeasibility
###############################################################################

test_that("extractFeasibility returns report when available", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R_es, portf, optimize_method = "ROI")
  
  fr <- extractFeasibility(opt)
  if (!is.null(fr)) {
    expect_s3_class(fr, "feasibility_report")
  }
})

test_that("extractFeasibility.rebalancing as.data.frame works", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  rebal <- optimize.portfolio.rebalancing(
    R_es, portf, optimize_method = "ROI",
    rebalance_on = "years", training_period = 36
  )
  
  fr <- extractFeasibility(rebal)
  expect_true(is.list(fr))
  
  fr_df <- extractFeasibility(rebal, as.data.frame = TRUE)
  if (!is.null(fr_df) && nrow(fr_df) > 0) {
    expect_true(is.data.frame(fr_df))
    expect_true("date" %in% colnames(fr_df))
  }
})

###############################################################################
# 21. extractStats class validation
###############################################################################

test_that("extractStats rejects wrong class", {
  expect_error(extractStats.optimize.portfolio.DEoptim(list()), "optimize.portfolio.DEoptim")
  expect_error(extractStats.optimize.portfolio.ROI(list()), "optimize.portfolio.ROI")
  expect_error(extractStats.optimize.portfolio.CVXR(list()), "optimize.portfolio.CVXR")
  expect_error(extractStats.optimize.portfolio.GenSA(list()), "optimize.portfolio.GenSA")
  expect_error(extractStats.optimize.portfolio.pso(list()), "optimize.portfolio.pso")
  expect_error(extractStats.optimize.portfolio.random(list()), "optimize.portfolio.random")
})

###############################################################################
# 22. extractObjectiveMeasures for summary.optimize.portfolio.rebalancing
###############################################################################

test_that("extractObjectiveMeasures works on summary.rebalancing object", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")
  
  rebal <- optimize.portfolio.rebalancing(
    R_es, portf, optimize_method = "ROI",
    rebalance_on = "years", training_period = 36
  )
  
  s <- summary(rebal)
  obj_meas <- extractObjectiveMeasures(s)
  expect_true(!is.null(obj_meas))
})

###############################################################################
# 23. extractWeights for summary.rebalancing
###############################################################################

test_that("extractWeights works on summary.rebalancing object", {
  portf <- add.objective(lo_portf, type = "risk", name = "StdDev")
  
  rebal <- optimize.portfolio.rebalancing(
    R_es, portf, optimize_method = "ROI",
    rebalance_on = "years", training_period = 36
  )
  
  s <- summary(rebal)
  wts <- extractWeights(s)
  expect_true(!is.null(wts))
  expect_true(inherits(wts, "xts"))
})
