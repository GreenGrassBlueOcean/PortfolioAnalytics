###############################################################################
# DEoptim Solver for PortfolioAnalytics
###############################################################################

#' @keywords internal
solve_deoptim <- function(R, portfolio, constraints, moments, penalty,
                          N, call, trace, search_size, rp,
                          message = FALSE, ...) {
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
  if (is.null(parallel) || is.na(parallel)) parallel <- TRUE
  if (!isTRUE(parallel) && "package:foreach" %in% search()) {
    foreach::registerDoSEQ()
  }

  # --- DEoptim control setup ---
  DEcformals <- as.list(formals(DEoptim::DEoptim.control))
  DEcformals$NP <- NP
  DEcformals$itermax <- itermax
  DEcformals$strategy   <- if (!is.null(dots$strategy) && !is.na(dots$strategy)) dots$strategy else 2
  DEcformals$reltol     <- if (!is.null(dots$reltol) && !is.na(dots$reltol)) dots$reltol else 0.000001
  DEcformals$steptol    <- if (!is.null(dots$steptol) && !is.na(dots$steptol)) dots$steptol else round(N * 1.5)
  DEcformals$c          <- if (!is.null(dots$c) && !is.na(dots$c)) dots$c else 0.4
  DEcformals$storepopfrom <- if (!is.null(dots$storepopfrom) && !is.na(dots$storepopfrom)) dots$storepopfrom else 1
  DEcformals$packages   <- if (!is.null(dots$packages) && !is.na(dots$packages)) dots$packages else names(sessionInfo()$otherPkgs)

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

  # --- Parallel cluster setup ---
  rcl <- NULL
  if (isTRUE(parallel) && "package:foreach" %in% search()) {
    parallelType <- if (!is.null(dots$parallelType)) dots$parallelType else 2
    DEcformals$parallelType <- parallelType
    if (parallelType == 2) {
      if (!requireNamespace("snow", quietly = TRUE) ||
          !requireNamespace("doSNOW", quietly = TRUE)) {
        stop("Packages 'snow' and 'doSNOW' are required for parallelType=2. ",
             "Install them with install.packages(c('snow', 'doSNOW'))",
             call. = FALSE)
      }
      nC <- parallel::detectCores()
      rcl <- snow::makeSOCKcluster(ifelse(nC <= 15, nC, 15))
      snow::clusterEvalQ(rcl, lapply(names(sessionInfo()$otherPkgs),
                                     require, character.only = TRUE))
      snow::clusterExport(rcl,
                          list("R", "portfolio", "constraints", "objectives",
                               "optimize_method", "search_size", "trace",
                               "momentFUN", "rp"),
                          envir = parent.frame())
      doSNOW::registerDoSNOW(rcl)
      DEcformals$cluster <- rcl
    }
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
  controlDE <- do.call(DEoptim::DEoptim.control, DEcformals)
  minw <- DEoptim::DEoptim(
    constrained_objective,
    lower = lower[1:N], upper = upper[1:N],
    control = controlDE,
    R = R, portfolio = portfolio, env = moments,
    normalize = FALSE, penalty = penalty,
    storage_env = storage_env,
    fnMap = function(x) fn_map(x, portfolio = portfolio)$weights
  )

  # --- Error handling ---
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

  # --- Cleanup ---
  if (!is.null(rcl)) {
    snow::stopCluster(rcl)
  }
  if (isTRUE(tmptrace)) trace <- tmptrace

  # --- Extract results ---
  weights <- as.vector(minw$optim$bestmem)
  names(weights) <- colnames(R)
  obj_vals <- constrained_objective(w = weights, R = R, portfolio, trace = TRUE,
                                    normalize = FALSE, env = moments,
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
