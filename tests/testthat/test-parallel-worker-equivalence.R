# Regression tests for two defects found on 2026-09-01/02.
#
# Both survived a 3243-test suite because the existing "parallel" coverage
# (test-maxcores-nested-parallel.R) re-implements the sizing helpers locally and
# asserts against those copies. It never spawns a cluster, so it could not see
# that a worker scored portfolios differently from the master.
#
# DEFECT A -- the worker setup was a no-op.
#   solve_deoptim() prepared its workers with
#     clusterEvalQ(rcl, lapply(names(sessionInfo()$otherPkgs), require, ...))
#   clusterEvalQ evaluates ON THE WORKER, where nothing is attached, so
#   sessionInfo()$otherPkgs was NULL there and it loaded nothing at all.
#
#   constrained_objective() special-cases a few measures by SYMBOL (StdDev, VaR,
#   ES, mean, median, turnover); those resolve inside this namespace and survive
#   on a bare worker. Everything else falls through to
#   match.fun(objective$name) -- a STRING lookup from parent.frame(), the
#   CALLER's environment. Under DEoptim the caller is parApply's internals, so
#   that chain never reaches this namespace and the name is unreachable. CRRA is
#   one of those, which is why crra is the strategy that diverged, and why a
#   StdDev-only test would pass with the bug fully present.
#
#   Production symptom: a 9-fold walk-forward gave sharpe 0.453467 in parallel
#   and 0.577793 sequentially, same seed and panel, one weight differing by
#   0.287.
#
# DEFECT B -- the failure was swallowed and then used in arithmetic.
#   match.fun() failing left `fun` a try-error, so `if (is.function(fun))`
#   skipped building `.formals`; do.call() then failed with "object '.formals'
#   not found"; that try-error was message()'d and fell through to
#   `objective$multiplier * tmp_measure`, throwing "non-numeric argument to
#   binary operator" from a line nowhere near the cause.
#
# NOTE ON TEST DESIGN
#   An earlier draft scored a weight vector on a hand-built cluster and compared
#   it to the master. That proves nothing: a closure defined inside test_that()
#   carries this package's namespace in its environment chain, so match.fun()
#   resolves on the "bare" worker and the test passes with the bug present. The
#   only honest test drives optimize.portfolio() itself, where the objective is
#   invoked from parApply and the caller's chain is genuinely bare.

test_that("DEoptim gives the same answer in parallel as it does sequentially", {
  # THE regression test for defect A. Pre-fix the parallel arm either errored
  # or returned a different optimum; both fail here.
  skip_on_cran()
  skip_if_not_installed("parallel")
  skip_if_not_installed("DEoptim")
  skip_if_not_installed("doSNOW")

  set.seed(11)
  n <- 5L; T <- 220L
  R <- xts::xts(matrix(rnorm(T * n, 4e-4, 0.01), ncol = n),
                order.by = as.Date("2023-01-02") + seq_len(T) - 1L)
  colnames(R) <- paste0("A", seq_len(n))

  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "leverage", min_sum = 0.98, max_sum = 1.02)
  p <- add.constraint(p, type = "box", min = 0, max = 0.5)
  # CRRA on purpose: it is resolved by string, not by symbol.
  p <- add.objective(p, type = "return", name = "CRRA",
                     arguments = list(lambda = 4), multiplier = 1)
  p <- add.objective(p, type = "risk", name = "StdDev", multiplier = 0)

  run <- function(par) {
    set.seed(4217)
    optimize.portfolio(R = R, portfolio = p, optimize_method = "DEoptim",
                       momentFUN = crra.moments,
                       search_size = 150, itermax = 3, trace = FALSE,
                       traceDE = 0, message = FALSE,
                       parallel = par, MaxCores = 2, parallelType = 2)
  }

  seq_opt <- run(FALSE)
  par_opt <- tryCatch(run(TRUE), error = function(e) e)

  expect_false(inherits(par_opt, "error"),
               info = paste("parallel run errored:",
                            if (inherits(par_opt, "error")) conditionMessage(par_opt) else ""))
  skip_if(inherits(par_opt, "error"), "parallel arm failed; see the expectation above")

  expect_equal(as.numeric(extractWeights(par_opt)),
               as.numeric(extractWeights(seq_opt)),
               tolerance = 1e-12)
})

test_that("an objective that cannot be resolved is named in the error", {
  # Defect B: the diagnosis must point at the objective, not at an arithmetic
  # line far downstream. No cluster needed -- an unresolvable name reproduces
  # the same code path.
  set.seed(13)
  n <- 4L; T <- 150L
  R <- xts::xts(matrix(rnorm(T * n, 4e-4, 0.01), ncol = n),
                order.by = as.Date("2023-01-02") + seq_len(T) - 1L)
  colnames(R) <- paste0("A", seq_len(n))
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "leverage", min_sum = 0.98, max_sum = 1.02)
  p <- add.constraint(p, type = "box", min = 0, max = 0.5)
  p <- add.objective(p, type = "risk", name = "NoSuchRiskMeasureXYZ", multiplier = 1)
  mom <- set.portfolio.moments(R, portfolio = p)

  err <- tryCatch({
    constrained_objective(w = rep(1 / n, n), R = R, portfolio = p, env = mom,
                          normalize = FALSE, penalty = 100)
    NA_character_
  }, error = function(e) conditionMessage(e))

  expect_false(is.na(err), info = "an unresolvable objective should not succeed")
  expect_match(err, "NoSuchRiskMeasureXYZ", fixed = TRUE)
  expect_match(err, "could not be evaluated", fixed = TRUE)
  # The whole point: never the opaque downstream arithmetic failure again.
  expect_false(grepl("non-numeric argument to binary operator", err, fixed = TRUE),
               info = "the old opaque arithmetic error has returned")
})
