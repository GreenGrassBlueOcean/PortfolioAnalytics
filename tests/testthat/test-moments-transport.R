# Regression tests for one-shot moment transport on the DEoptim cluster path.
#
# WHAT CHANGED
#   solve_deoptim() used to hand DEoptim `env = moments`. DEoptim's cluster
#   path is parApply(cl, pop, 1, fn, ...), which re-serializes FUN and every
#   `...` argument to every worker on every generation. For CRRA at 60 assets
#   the m4 tensor alone is 103.7 MB, so an 18-worker cluster moved ~1.87 GB per
#   generation. The moments are now shipped once via clusterCall and read from
#   a namespace-level cache.
#
# WHY THERE ARE TWO KINDS OF TEST HERE
#   The change must not move any number, so there is an equivalence test that
#   runs both transports and compares optima exactly.
#
#   But an equivalence test alone is not enough, because the way this
#   optimisation gets silently undone does not change any number either. If
#   .pa_cached_objective is ever moved inside solve_deoptim(), its closure
#   environment becomes that frame -- which holds `moments` -- and R serializes
#   the whole tensor along with the function, exactly as before. The results
#   stay identical and the run just goes back to being slow. So the closure
#   hygiene test below pins the environment and the serialized size, and it is
#   the one that actually protects the fix.

test_that("the cached objective carries no data", {
  # If this fails, the transport optimisation has been silently undone even
  # though every other test in this file still passes.
  env <- environment(.pa_cached_objective)
  expect_true(isNamespace(env))
  expect_identical(environmentName(env), "PortfolioAnalytics")

  # A closure whose environment is a namespace serializes as a REFERENCE to
  # that namespace. One built inside a frame that holds the moments drags them
  # along instead. Assert the contrast directly rather than guessing at an
  # absolute byte budget: `wrong` is exactly what this wrapper would become if
  # it were moved back inside solve_deoptim().
  wrong <- local({
    moments <- list(m4 = numeric(2e5))   # stand-in for the real tensor
    function(w, ...) constrained_objective(w = w, env = moments, ...)
  })
  right_size <- length(serialize(.pa_cached_objective, connection = NULL))
  wrong_size <- length(serialize(wrong, connection = NULL))

  expect_gt(wrong_size, 10L * right_size)
  expect_lt(right_size, 100000L)
})

test_that("the moment cache round-trips through a real cluster", {
  skip_on_cran()
  skip_if_not_installed("parallel")

  cl <- parallel::makeCluster(2L, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)

  m <- list(mu = c(1, 2, 3), sigma = diag(3))
  # Passing the setter as FUN also exercises the mechanism the fix relies on:
  # a namespace closure serializes as a reference, and the worker loads this
  # package on unserialize without being told to.
  parallel::clusterCall(cl, .pa_set_moments_cache, m = m)
  got <- parallel::clusterCall(cl, .pa_get_moments_cache)

  expect_length(got, 2L)
  expect_equal(got[[1]], m)
  expect_equal(got[[2]], m)
})

test_that("cached and direct moment transport give identical optima", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  skip_if_not_installed("DEoptim")
  skip_if_not_installed("doSNOW")

  set.seed(23)
  n <- 5L; T <- 220L
  R <- xts::xts(matrix(rnorm(T * n, 4e-4, 0.01), ncol = n),
                order.by = as.Date("2023-01-02") + seq_len(T) - 1L)
  colnames(R) <- paste0("A", seq_len(n))

  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "leverage", min_sum = 0.98, max_sum = 1.02)
  p <- add.constraint(p, type = "box", min = 0, max = 0.5)
  # CRRA on purpose: it is the objective whose moment list is large enough for
  # the transport to matter, and it is resolved by string rather than symbol.
  p <- add.objective(p, type = "return", name = "CRRA",
                     arguments = list(lambda = 4), multiplier = 1)
  p <- add.objective(p, type = "risk", name = "StdDev", multiplier = 0)

  run_arm <- function(cache, par = TRUE, msg = FALSE) {
    set.seed(4217)
    optimize.portfolio(R = R, portfolio = p, optimize_method = "DEoptim",
                       momentFUN = crra.moments,
                       search_size = 150, itermax = 3, trace = FALSE,
                       traceDE = 0, message = msg,
                       parallel = par, MaxCores = 2, parallelType = 2,
                       moments_cache = cache)
  }

  # Guard against a vacuous pass: with no cluster, `use_moment_cache` is FALSE
  # for BOTH parallel arms, they take the same branch, and the comparison
  # proves nothing. The cluster message is the only evidence it was taken.
  msgs <- capture_messages(run_arm(TRUE, msg = TRUE))
  expect_true(any(grepl("DEoptim parallel cluster", msgs, fixed = TRUE)),
              info = "no cluster was built, so the two arms are the same code")

  # Three arms, because two different invariants have to hold at once:
  #   cached   -- parallel, moments shipped once (the new transport)
  #   direct   -- parallel, moments in `...` every generation (the old one)
  #   single   -- no cluster at all
  # cached == direct is the before/after check: dropping ~100 MB per generation
  # off the wire must not move a single weight.
  # cached == single is the invariant that must survive it: the parallel and
  # sequential paths agreed after the 2.1.1.9007 RNG fix, and a change to how
  # the objective reaches the workers is exactly the kind of thing that could
  # quietly break that agreement again.
  cached <- run_arm(TRUE)
  direct <- run_arm(FALSE)
  single <- run_arm(TRUE, par = FALSE)

  # tolerance = 0 deliberately. The arms differ only in where the objective
  # reads an identical moment list from, so anything other than bit-identical
  # output means the moments are not making it across intact.
  expect_equal(as.numeric(extractWeights(cached)),
               as.numeric(extractWeights(direct)), tolerance = 0)
  expect_equal(as.numeric(cached$out), as.numeric(direct$out), tolerance = 0)

  expect_equal(as.numeric(extractWeights(cached)),
               as.numeric(extractWeights(single)), tolerance = 0)
  expect_equal(as.numeric(cached$out), as.numeric(single$out), tolerance = 0)

  # The master's copy must not outlive the optimisation, or every solved
  # portfolio leaves a moment list resident in this namespace.
  expect_null(.pa_get_moments_cache())
})

test_that("an empty moment cache is reported rather than worked around", {
  # The dangerous failure would be scoring one worker against recomputed
  # moments while the rest of the population used the shipped ones: a quietly
  # inconsistent search rather than an error.
  .pa_clear_moments_cache()
  on.exit(.pa_clear_moments_cache(), add = TRUE)

  err <- tryCatch({
    .pa_cached_objective(w = rep(0.2, 5L))
    NA_character_
  }, error = function(e) conditionMessage(e))

  expect_match(err, "moment cache is empty", fixed = TRUE)
})
