###############################################################################
# DEoptim Solver for PortfolioAnalytics
###############################################################################

#' @keywords internal
solve_deoptim <- function(R, portfolio, constraints, moments, penalty,
                          N, call, trace, search_size, rp,
                          message = FALSE, MaxCores = 15, ...) {
  stopifnot("package:DEoptim" %in% search() || requireNamespace("DEoptim", quietly = TRUE))

  dots <- list(...)
  out <- list()

  # --- Parameter extraction ---
  itermax <- dots$itermax
  if (is.null(itermax) || is.na(itermax)) {
    itermax_user <- FALSE
    itermax <- N * 50
  } else {
    itermax_user <- TRUE
  }

  NP <- round(search_size / itermax)
  if (NP < (N * 10)) NP <- N * 10
  if (NP >= 2000) NP <- 2000
  if (!itermax_user) {
    itermax <- round(search_size / NP)
    if (itermax < 50) itermax <- 50
  }

  parallel <- dots$parallel
  if (is.null(parallel) || is.na(parallel)) parallel <- FALSE

  # --- DEoptim control setup ---
  DEcformals <- as.list(formals(DEoptim::DEoptim.control))
  DEcformals$NP <- NP
  DEcformals$itermax <- itermax
  DEcformals$strategy   <- if (!is.null(dots$strategy) && !is.na(dots$strategy)) dots$strategy else 2
  DEcformals$reltol     <- if (!is.null(dots$reltol) && !is.na(dots$reltol)) dots$reltol else 0.000001
  DEcformals$steptol    <- if (!is.null(dots$steptol) && !is.na(dots$steptol)) dots$steptol else round(N * 1.5)
  DEcformals$c          <- if (!is.null(dots$c) && !is.na(dots$c)) dots$c else 0.4
  DEcformals$storepopfrom <- if (!is.null(dots$storepopfrom) && !is.na(dots$storepopfrom)) dots$storepopfrom else 1
  # `packages` is a character VECTOR, so is.na() returns a vector and since
  # R 4.3 `&&` errors with "'length = 2' in coercion to 'logical(1)'" -- which
  # made this documented argument impossible to supply with more than one
  # package. Test emptiness rather than NA-ness; the scalar controls above are
  # unaffected because their guards only ever see length-1 values.
  DEcformals$packages   <- if (length(dots$packages)) dots$packages else names(sessionInfo()$otherPkgs)

  traceDE <- dots$traceDE
  if (is.null(traceDE) || is.na(traceDE)) traceDE <- TRUE
  DEcformals$trace <- traceDE

  # --- Trace storage setup (local environment, reentrant) ---
  storage_env <- new.env(parent = emptyenv())
  tmptrace <- NULL
  if (isTRUE(trace)) {
    tmptrace <- trace
    assign(".objectivestorage", list(), envir = storage_env)
    trace <- FALSE
  }

  # --- Constraint bounds ---
  upper <- constraints$max
  lower <- constraints$min

  if ((constraints$max_sum - constraints$min_sum) < 0.02) {
    message("Leverage constraint min_sum and max_sum are restrictive, ",
            "consider relaxing. e.g. 'full_investment' constraint should be min_sum=0.99 and max_sum=1.01")
  }

  rcl <- NULL

  # Preserve the RNG stream across cluster setup.
  #
  # Loading doSNOW consumes exactly one draw from the global stream, and
  # requireNamespace("doSNOW") below runs ONLY on the parallel path. So a
  # parallel run and a sequential run start DEoptim from different positions in
  # the same seeded stream: different initial population, different mutation
  # sequence, different optimum, from identical inputs. Measured on a 9-fold
  # walk-forward: sharpe 0.453467 parallel vs 0.577793 sequential, with one
  # weight differing by 0.287.
  #
  # It is reproducible WITHIN a mode (two parallel runs are bit-identical),
  # which is exactly what made this look like a scoring bug rather than an RNG
  # one. Snapshot the stream here and restore it once the cluster is up, so the
  # setup cost is unwound and DEoptim starts from the same place either way.
  .pa_seed_before <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv())
  } else NULL

  # A silent fall-back to sequential is the expensive failure mode here: the
  # run still completes, just N times slower, with nothing in the log saying
  # so. `parallel=TRUE` only takes effect when foreach is ATTACHED (not merely
  # imported), so say plainly when that is not the case.
  if (isTRUE(parallel) && !("package:foreach" %in% search())) {
    warning("parallel=TRUE but package 'foreach' is not attached, so DEoptim ",
            "will run SEQUENTIALLY. Attach it (library(foreach), or any ",
            "package that Depends on it) before calling optimize.portfolio().",
            call. = FALSE)
  }
  if (isTRUE(parallel) && "package:foreach" %in% search()) {
    parallelType <- if (!is.null(dots$parallelType)) dots$parallelType else "foreach"
    DEcformals$parallelType <- parallelType
    if (parallelType %in% c(2, "foreach")) {
      if (!requireNamespace("doSNOW", quietly = TRUE)) { # nocov start
        stop("Package 'doSNOW' is required for parallelType='foreach'. ",
             "Install it with install.packages('doSNOW')",
             call. = FALSE)
      } # nocov end
      nC <- parallel::detectCores()
      # `MaxCores` used to be hard-coded to 15 here, which silently discarded
      # whatever the caller asked for -- including the `MaxSubCores` value that
      # optimize.portfolio.rebalancing() forwards as MaxCores specifically to
      # bound NESTED clusters. Honour it, and validate it the same way
      # optimize.portfolio_v1() does.
      if (!is.numeric(MaxCores) || length(MaxCores) != 1L || is.na(MaxCores) ||
          MaxCores < 1) {
        stop("MaxCores must be a single number >= 1, but has value ",
             paste(deparse(MaxCores), collapse = ""), call. = FALSE)
      }
      n_workers <- min(nC, as.integer(MaxCores))
      if (isTRUE(message)) {
        message("DEoptim parallel cluster: ", n_workers, " worker(s) ",
                "(detectCores=", nC, ", MaxCores=", as.integer(MaxCores), ")")
      }
      rcl <- parallel::makeCluster(n_workers, type = "PSOCK")
      on.exit({
        if (!is.null(rcl)) parallel::stopCluster(rcl)
        # Reset to sequential so we never leave a dead cluster registered
        foreach::registerDoSEQ()
      }, add = TRUE)
      # Attach on the workers the packages that are attached HERE.
      #
      # This used to be
      #   clusterEvalQ(rcl, lapply(names(sessionInfo()$otherPkgs), require, ...))
      # which silently did nothing: clusterEvalQ evaluates its expression ON THE
      # WORKER, and a fresh PSOCK worker has no packages attached, so
      # sessionInfo()$otherPkgs was NULL there and lapply(NULL, ...) loaded
      # nothing at all. The objective could then hit a worker where the S3
      # methods it relies on were never registered, failing with errors as
      # opaque as "non-numeric argument to binary operator".
      #
      # The package list has to be computed in the MASTER and shipped as data.
      .pa_worker_pkgs <- names(utils::sessionInfo()$otherPkgs)
      if (length(.pa_worker_pkgs)) {
        parallel::clusterCall(rcl, function(pkgs) {
          for (pk in pkgs) {
            suppressWarnings(suppressMessages(
              requireNamespace(pk, quietly = TRUE) &&
                require(pk, character.only = TRUE, quietly = TRUE)))
          }
          invisible(NULL)
        }, pkgs = .pa_worker_pkgs)
      }
      doSNOW::registerDoSNOW(rcl)
      DEcformals$cluster <- rcl
    }
  }

  # Rewind whatever cluster setup consumed, so the search below draws from the
  # same position as a sequential run. See the note above `.pa_seed_before`.
  if (is.null(.pa_seed_before)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", .pa_seed_before, envir = globalenv())
  }

  # --- Initial population ---
  warm_start <- dots$warm_start
  if (!is.null(rp)) {
    rp_len <- min(nrow(rp), NP)
    DEcformals$initialpop <- rp[1:rp_len, ]
  } else {
    rp_method <- if (!is.null(dots$rp_method)) dots$rp_method else "sample"
    fev <- if (!is.null(dots$fev)) dots$fev else 0:5
    init_rp <- random_portfolios(portfolio = portfolio, permutations = (NP + 1),
                                 rp_method = rp_method, eliminate = FALSE, fev = fev)
    DEcformals$initialpop <- init_rp
  }
  # Inject warm_start as row 1 of initial population
  if (!is.null(warm_start) && is.numeric(warm_start) &&
      length(warm_start) == N && !is.null(DEcformals$initialpop)) {
    DEcformals$initialpop[1, ] <- as.numeric(warm_start)
  }

  # --- Solve ---
  # `normalize` was hard-wired to FALSE here while constrained_objective()'s
  # own default is TRUE. The difference is not cosmetic: with normalize = TRUE
  # a candidate that misses the leverage band is RESCALED onto it before being
  # scored; with FALSE it is PENALISED instead. On a narrow band (e.g. the
  # 0.98-1.02 used in production) almost every DE/rand/1/bin mutation of a
  # 60-weight vector lands outside, so nearly every trial loses to its parent
  # and the population stops moving after a handful of generations.
  #
  # Kept FALSE by default so existing results are bit-identical; expose it so
  # the alternative can be measured on real data without patching an install.
  normalize_obj <- dots$normalize
  if (is.null(normalize_obj) || !is.logical(normalize_obj) ||
      length(normalize_obj) != 1L || is.na(normalize_obj)) {
    normalize_obj <- FALSE
  }
  if (isTRUE(message)) {
    message("DEoptim constrained_objective: normalize = ", normalize_obj)
  }

  # --- Moment transport -------------------------------------------------
  # Passing `env = moments` through DEoptim's `...` re-serializes the whole
  # moment list to every worker on every generation, because the cluster path
  # is `parApply(cl, pop, 1, fn, ...)`. For CRRA at 60 assets the m4 tensor
  # alone is 103.7 MB. Ship it once instead and hand DEoptim a wrapper that
  # reads the worker-local cache. See R/moments_transport.R for the numbers and
  # for why the wrapper cannot be defined in this frame.
  #
  # `moments_cache = FALSE` restores the old transport unchanged. That exists
  # so the two paths can be compared directly, which is what
  # test-moments-transport.R does; it must keep producing identical optima.
  use_moment_cache <- !is.null(rcl) && !identical(dots$moments_cache, FALSE)
  if (use_moment_cache) {
    # A worker resolves namespace closures against ITS OWN installed
    # PortfolioAnalytics. If a stale build sits earlier on that worker's
    # .libPaths() it will not have the cache helpers, and every node would
    # error. Probe before committing to it: a version skew should cost
    # throughput, not the run.
    ready <- isTRUE(tryCatch(
      all(vapply(parallel::clusterCall(rcl, .pa_moments_cache_ready),
                 isTRUE, logical(1))),
      error = function(e) FALSE))
    if (!ready) {
      warning("the cluster workers are running a PortfolioAnalytics build ",
              "without moment-cache support, so the moment list will be sent ",
              "on every generation as before. Align the version installed on ",
              "the workers to avoid the transport cost.", call. = FALSE)
      use_moment_cache <- FALSE
    }
  }
  if (use_moment_cache) {
    parallel::clusterCall(rcl, .pa_set_moments_cache, m = moments)
    # The master evaluates nothing while a cluster is attached, but populate it
    # too so a master-side call could never silently score differently.
    .pa_set_moments_cache(moments)
    on.exit(.pa_clear_moments_cache(), add = TRUE)
  }

  controlDE <- do.call(DEoptim::DEoptim.control, DEcformals)
  de_args <- list(
    if (use_moment_cache) .pa_cached_objective else constrained_objective,
    lower = lower[1:N], upper = upper[1:N],
    control = controlDE,
    R = R, portfolio = portfolio,
    normalize = normalize_obj, penalty = penalty,
    storage_env = storage_env,
    fnMap = function(x) fn_map(x, portfolio = portfolio)$weights
  )
  if (!use_moment_cache) de_args$env <- moments
  minw <- do.call(DEoptim::DEoptim, de_args)

  # nocov start — DEoptim almost never throws; would require mocking to test

  if (inherits(minw, "try-error")) {
    message(minw)
    ErrorM <- minw
    minw <- NULL
  }
  if (is.null(minw)) {
    message("Optimizer was unable to find a solution for target")
    return(optimization_failure(
      message = "Optimizer was unable to find a solution for target",
      solver = "DEoptim",
      call = call,
      error = if (exists("ErrorM")) ErrorM else NULL
    ))
  }
  # nocov end
  if (isTRUE(tmptrace)) trace <- tmptrace

  # --- Extract results ---
  weights <- as.vector(minw$optim$bestmem)
  names(weights) <- colnames(R)
  # Score the winner the same way it was searched, or the reported objective
  # measures would not correspond to the value DEoptim actually minimised.
  obj_vals <- constrained_objective(w = weights, R = R, portfolio, trace = TRUE,
                                    normalize = normalize_obj, env = moments,
                                    penalty = penalty)$objective_measures
  out <- list(weights = weights, objective_measures = obj_vals,
              opt_values = obj_vals, out = minw$optim$bestval, call = call)
  if (isTRUE(trace)) {
    out$DEoutput <- minw
    out$DEoptim_objective_results <- tryCatch(
      get(".objectivestorage", envir = storage_env, inherits = FALSE),
      error = function(e) NULL
    )
    rm(list = ".objectivestorage", envir = storage_env)
  }
  out
}
