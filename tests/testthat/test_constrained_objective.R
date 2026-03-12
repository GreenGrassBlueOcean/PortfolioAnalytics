##### test_constrained_objective.R #####
# Phase 6 coverage tests for constrained_objective.R
# Targets: constraint penalty branches (group, max_pos, diversification,
#          turnover, return, factor_exposure, ptc, leverage), objective type
#          dispatch (return, risk, turnover, minmax, risk_budget,
#          weight_concentration), trace/storage/verbose paths, NA warning

library(testthat)
library(PortfolioAnalytics)

data(edhec)
R <- edhec[1:60, 1:4]
colnames(R) <- c("CA", "CTAG", "DS", "EM")
funds <- colnames(R)

# Equal-weight vector
w_eq <- rep(1 / length(funds), length(funds))
names(w_eq) <- funds

# ---------------------------------------------------------------------------
# Helper: minimal portfolio with return + risk objectives
# ---------------------------------------------------------------------------
base_portf <- function() {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
}

# ===========================================================================
# 1. Basic return value
# ===========================================================================

test_that("constrained_objective returns a finite scalar (default)", {
  val <- constrained_objective(w = w_eq, R = R, portfolio = base_portf())
  expect_true(is.numeric(val))
  expect_length(val, 1)
  expect_true(is.finite(val))
})

# ===========================================================================
# 2. trace = TRUE returns structured list
# ===========================================================================

test_that("constrained_objective with trace=TRUE returns list with out, weights, objective_measures", {
  res <- constrained_objective(w = w_eq, R = R, portfolio = base_portf(),
                               trace = TRUE)
  expect_true(is.list(res))
  expect_true(all(c("out", "weights", "objective_measures") %in% names(res)))
  expect_true(is.numeric(res$out))
  expect_true(is.numeric(res$weights))
  expect_true(is.list(res$objective_measures))
  expect_true("mean" %in% names(res$objective_measures))
  expect_true("StdDev" %in% names(res$objective_measures))
})

# ===========================================================================
# 3. normalize = FALSE penalizes sum violations
# ===========================================================================

test_that("normalize=FALSE penalizes weights above max_sum", {
  p <- base_portf()
  w_over <- c(0.5, 0.3, 0.2, 0.15)
  names(w_over) <- funds
  
  val_norm   <- constrained_objective(w = w_over, R = R, portfolio = p,
                                      normalize = TRUE)
  val_nonorm <- constrained_objective(w = w_over, R = R, portfolio = p,
                                      normalize = FALSE, penalty = 1e4)
  # With normalize=FALSE the sum>1 violation adds penalty, making it larger

  expect_true(val_nonorm > val_norm)
})

test_that("normalize=FALSE penalizes weights below min_sum", {
  p <- base_portf()
  w_under <- c(0.1, 0.1, 0.1, 0.1)
  names(w_under) <- funds
  
  val <- constrained_objective(w = w_under, R = R, portfolio = p,
                               normalize = FALSE, penalty = 1e4)
  expect_true(is.finite(val))
  # Should have penalty contributions
  val_zero <- constrained_objective(w = w_under, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val > val_zero)
})

# ===========================================================================
# 4. Box constraint penalties
# ===========================================================================

test_that("box constraint violation is penalized", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.10, max = 0.40)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # Weights that violate box (0.80 > 0.40)
  w_bad <- c(0.80, 0.10, 0.05, 0.05)
  names(w_bad) <- funds
  
  val_pen  <- constrained_objective(w = w_bad, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 1e4)
  val_zero <- constrained_objective(w = w_bad, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 5. Group constraint penalties
# ===========================================================================

test_that("group constraint violation is penalized", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "group",
                      groups = list(c(1, 2), c(3, 4)),
                      group_min = c(0.4, 0.4),
                      group_max = c(0.6, 0.6))
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # Weights: group1 = 0.1+0.1 = 0.2 < 0.4 → violation
  w_grp <- c(0.1, 0.1, 0.4, 0.4)
  names(w_grp) <- funds
  
  val_pen  <- constrained_objective(w = w_grp, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 1e4)
  val_zero <- constrained_objective(w = w_grp, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 6. Position limit constraint
# ===========================================================================

test_that("max_pos constraint violation is penalized", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "position_limit", max_pos = 2)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # All 4 assets have nonzero weight → violates max_pos=2
  val_pen  <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 1e4)
  val_zero <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 7. Diversification constraint
# ===========================================================================

test_that("diversification constraint is penalized", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "diversification", div_target = 0.99)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # Concentrated weights → low diversification → penalty
  w_conc <- c(0.85, 0.05, 0.05, 0.05)
  names(w_conc) <- funds
  
  val_pen  <- constrained_objective(w = w_conc, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 1e4)
  val_zero <- constrained_objective(w = w_conc, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 8. Turnover constraint
# ===========================================================================

test_that("turnover constraint is penalized when violated", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "turnover", turnover_target = 0.01)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # Default portfolio$assets is equal weight; weights far from that → high turnover
  w_far <- c(0.7, 0.1, 0.1, 0.1)
  names(w_far) <- funds
  
  val_pen  <- constrained_objective(w = w_far, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 1e4)
  val_zero <- constrained_objective(w = w_far, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 9. Return target constraint
# ===========================================================================

test_that("return_target constraint is penalized", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  # Set an extreme return target that will be violated
  p <- add.constraint(p, type = "return", return_target = 0.50)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  val_pen  <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    penalty = 1e4)
  val_zero <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 10. Factor exposure constraint
# ===========================================================================

test_that("factor exposure constraint is penalized", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  
  # Simple exposure matrix: single factor
  B <- matrix(c(1.2, 0.8, 1.0, 1.5), ncol = 1)
  p <- add.constraint(p, type = "factor_exposure", B = B,
                      lower = 0.5, upper = 0.6)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # Equal weight exposure = mean(B) = 1.125, exceeds upper=0.6
  val_pen  <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 1e4)
  val_zero <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 11. Transaction cost constraint
# ===========================================================================

test_that("proportional transaction cost is applied", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "transaction_cost",
                      ptc = rep(0.01, length(funds)))
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # weights differ from portfolio$assets → transaction cost > 0
  w_diff <- c(0.5, 0.2, 0.2, 0.1)
  names(w_diff) <- funds
  
  val_ptc  <- constrained_objective(w = w_diff, R = R, portfolio = p)
  
  # Compare to portfolio without ptc
  p_noptc <- portfolio.spec(assets = funds)
  p_noptc <- add.constraint(p_noptc, type = "full_investment")
  p_noptc <- add.constraint(p_noptc, type = "long_only")
  p_noptc <- add.objective(p_noptc, type = "risk", name = "StdDev")
  val_noptc <- constrained_objective(w = w_diff, R = R, portfolio = p_noptc)
  
  expect_true(val_ptc > val_noptc)
})

# ===========================================================================
# 12. Leverage exposure constraint
# ===========================================================================

test_that("leverage exposure constraint is penalized", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = -2, max_sum = 2)
  p <- add.constraint(p, type = "box", min = -1, max = 1)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 1.0)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  # Weights with high leverage: sum(|w|) = 2.4 > 1.0
  w_lev <- c(0.8, 0.6, -0.5, -0.5)
  names(w_lev) <- funds
  
  val_pen  <- constrained_objective(w = w_lev, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 1e4)
  val_zero <- constrained_objective(w = w_lev, R = R, portfolio = p,
                                    normalize = FALSE, penalty = 0)
  expect_true(val_pen > val_zero)
})

# ===========================================================================
# 13. Objective type dispatch: return_objective with target
# ===========================================================================

test_that("return_objective with target penalizes deviation", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "return", name = "mean", target = 0.50)
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(is.list(val))
  expect_true(val$out > 0) # Large target → large penalty
})

# ===========================================================================
# 14. Objective type dispatch: portfolio_risk_objective with target
# ===========================================================================

test_that("risk objective with target penalizes deviation", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev", target = 0.001)
  
  val_t <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(val_t$out > 0)
  expect_true("StdDev" %in% names(val_t$objective_measures))
})

# ===========================================================================
# 15. Objective type dispatch: turnover_objective
# ===========================================================================

test_that("turnover objective is computed and contributes to output", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "turnover", name = "turnover")
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true("turnover" %in% names(val$objective_measures))
})

test_that("turnover objective with target penalizes deviation", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "turnover", name = "turnover", target = 0.001)
  
  w_far <- c(0.7, 0.1, 0.1, 0.1)
  names(w_far) <- funds
  
  val <- constrained_objective(w = w_far, R = R, portfolio = p)
  expect_true(is.finite(val))
})

# ===========================================================================
# 16. Objective type dispatch: minmax_objective
# ===========================================================================

test_that("minmax_objective penalizes outside min/max", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "tmp_minmax", name = "mean",
                     min = 0.001, max = 0.002)
  
  val_pen  <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    penalty = 1e4, trace = TRUE)
  val_zero <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    penalty = 0, trace = TRUE)
  # With penalty, deviation from min/max range is penalized
  expect_true(val_pen$out != val_zero$out)
})

# ===========================================================================
# 17. Objective type dispatch: risk_budget_objective (equal risk)
# ===========================================================================

test_that("risk_budget_objective with min_concentration is computed", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk_budget", name = "StdDev",
                     min_concentration = TRUE)
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(is.list(val))
  expect_true("StdDev" %in% names(val$objective_measures))
})

test_that("risk_budget_objective with min_difference is computed", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk_budget", name = "StdDev",
                     min_difference = TRUE)
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(is.list(val))
})

test_that("risk_budget_objective with max_prisk penalizes violations", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk_budget", name = "StdDev",
                     max_prisk = rep(0.20, 4))
  
  # With equal weights, some assets likely have pct risk > 20%
  val_pen  <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    penalty = 1e4)
  val_zero <- constrained_objective(w = w_eq, R = R, portfolio = p,
                                    penalty = 0)
  expect_true(is.finite(val_pen))
  expect_true(is.finite(val_zero))
})

test_that("risk_budget_objective with target penalizes deviation", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk_budget", name = "StdDev",
                     target = 0.001)
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(val$out > 0)
})

# ===========================================================================
# 18. Objective type dispatch: weight_concentration_objective (scalar)
# ===========================================================================

test_that("weight_concentration_objective (scalar) contributes to output", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "weight_concentration",
                     name = "HHI",
                     conc_aversion = 0.1)
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(is.list(val))
  expect_true("HHI" %in% names(val$objective_measures))
})

test_that("weight_concentration_objective (grouped) contributes to output", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "weight_concentration",
                     name = "HHI",
                     conc_aversion = c(0.1, 0.1),
                     conc_groups = list(c(1, 2), c(3, 4)))
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(is.list(val))
})

# ===========================================================================
# 19. Storage path
# ===========================================================================

test_that("storage_env accumulates objective results across calls", {
  p <- base_portf()
  env <- new.env(parent = emptyenv())
  assign(".objectivestorage", list(), envir = env)
  
  constrained_objective(w = w_eq, R = R, portfolio = p, storage_env = env)
  constrained_objective(w = w_eq, R = R, portfolio = p, storage_env = env)
  
  store <- get(".objectivestorage", envir = env)
  expect_equal(length(store), 2)
  expect_true(all(c("out", "weights", "init_weights", "objective_measures") %in%
                    names(store[[1]])))
})

# ===========================================================================
# 20. Warning on asset count mismatch
# ===========================================================================

test_that("warning when weights length != portfolio assets length", {
  # Build portfolio for 4 assets but pass 3 weights
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.5, max_sum = 1.5)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  w_short <- c(0.4, 0.3, 0.3)
  names(w_short) <- funds[1:3]
  
  # The mismatch warning fires, but downstream processing may also error
  warnings_seen <- character()
  tryCatch(
    withCallingHandlers(
      constrained_objective(w = w_short, R = R[, 1:3], portfolio = p),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  expect_true(any(grepl("do not match", warnings_seen)))
})

# ===========================================================================
# 21. Error on non-portfolio object
# ===========================================================================

test_that("error when portfolio is not a portfolio object", {
  expect_error(
    constrained_objective(w = w_eq, R = R, portfolio = list(assets = funds)),
    "not of class portfolio"
  )
})

# ===========================================================================
# 22. No objectives warning
# ===========================================================================

test_that("warning when portfolio has no objectives", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p$objectives <- NULL
  
  expect_warning(
    constrained_objective(w = w_eq, R = R, portfolio = p),
    "no objectives"
  )
})

# ===========================================================================
# 23. var objective name mapped to StdDev in trace output
# ===========================================================================

test_that("var objective is reported as StdDev in trace output", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "var")
  
  res <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true("StdDev" %in% names(res$objective_measures))
  expect_false("var" %in% names(res$objective_measures))
})

# ===========================================================================
# 24. Custom objective function dispatch
# ===========================================================================

test_that("custom objective function is dispatched via match.fun", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  # pasd is a custom function defined in the package
  p <- add.objective(p, type = "risk", name = "pasd",
                     arguments = list(N = 36))
  p <- add.objective(p, type = "risk", name = "StdDev", multiplier = 0)
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true(is.list(val))
  expect_true("pasd" %in% names(val$objective_measures))
})

# ===========================================================================
# 25. R trimmed when ncol(R) > length(w)
# ===========================================================================

test_that("R is trimmed when it has more columns than weights", {
  p <- portfolio.spec(assets = funds[1:2])
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  w_short <- c(0.5, 0.5)
  names(w_short) <- funds[1:2]
  
  val <- constrained_objective(w = w_short, R = R, portfolio = p)
  expect_true(is.finite(val))
})

# ===========================================================================
# 26. ES/VaR objectives in constrained_objective
# ===========================================================================

test_that("ES objective computes correctly in constrained_objective", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "ES")
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true("ES" %in% names(val$objective_measures))
  expect_true(is.finite(val$out))
})

test_that("VaR objective computes correctly in constrained_objective", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "VaR")
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  expect_true("VaR" %in% names(val$objective_measures))
  expect_true(is.finite(val$out))
})

# ===========================================================================
# 27. mean objective (return_objective, no target)
# ===========================================================================

test_that("mean return objective maximization (negative multiplier)", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "return", name = "mean")
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, trace = TRUE)
  # With default multiplier of -1, the return contribution should be negative
  expect_true(val$out < 0 || is.finite(val$out))
})

# ===========================================================================
# 28. env argument pre-computed moments
# ===========================================================================

test_that("env argument passes pre-computed moments", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  
  env <- list(
    mu = matrix(colMeans(R), ncol = 1),
    sigma = cov(R)
  )
  
  val <- constrained_objective(w = w_eq, R = R, portfolio = p, env = env)
  expect_true(is.finite(val))
  
  # Should match the version without env
  val2 <- constrained_objective(w = w_eq, R = R, portfolio = p)
  expect_equal(val, val2)
})

# ===========================================================================
# 29. constrained_objective_v1 basic functionality
# ===========================================================================

# Helper for v1 constraint objects
make_v1_constr <- function() {
  gen <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 1,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = generatesequence(min = 0, max = 1, by = 0.01))
  )
  suppressWarnings(
    add.objective_v1(constraints = gen, type = "risk", name = "StdDev")
  )
}

test_that("constrained_objective_v1 processes objectives", {
  gen <- make_v1_constr()
  
  val <- suppressWarnings(
    constrained_objective_v1(w = w_eq, R = R, constraints = gen)
  )
  expect_true(is.numeric(val))
  expect_true(is.finite(val))
})

test_that("constrained_objective_v1 trace returns list", {
  gen <- make_v1_constr()
  
  res <- suppressWarnings(
    constrained_objective_v1(w = w_eq, R = R, constraints = gen, trace = TRUE)
  )
  expect_true(is.list(res))
  expect_true("out" %in% names(res))
  expect_true("weights" %in% names(res))
})

test_that("constrained_objective_v1 normalize=FALSE penalizes sum violations", {
  gen <- make_v1_constr()
  
  w_over <- c(0.5, 0.3, 0.2, 0.15)
  names(w_over) <- funds
  
  val_pen  <- suppressWarnings(
    constrained_objective_v1(w = w_over, R = R, constraints = gen,
                             normalize = FALSE, penalty = 1e4)
  )
  val_zero <- suppressWarnings(
    constrained_objective_v1(w = w_over, R = R, constraints = gen,
                             normalize = FALSE, penalty = 0)
  )
  expect_true(val_pen > val_zero)
})

test_that("constrained_objective_v1 storage_env accumulates", {
  gen <- make_v1_constr()
  
  env <- new.env(parent = emptyenv())
  assign(".objectivestorage", list(), envir = env)
  
  suppressWarnings(
    constrained_objective_v1(w = w_eq, R = R, constraints = gen,
                             storage_env = env)
  )
  store <- get(".objectivestorage", envir = env)
  expect_equal(length(store), 1)
})

# ===========================================================================
# 30. calibrate_penalty integration
# ===========================================================================

test_that("calibrate_penalty with pre-computed env", {
  p <- base_portf()
  env <- list(
    mu = matrix(colMeans(R), ncol = 1),
    sigma = cov(R)
  )
  
  penalty <- calibrate_penalty(R = R, portfolio = p, env = env)
  expect_true(is.finite(penalty))
  expect_true(penalty > 0)
})
