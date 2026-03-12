###############################################################################
# GenSA (Generalized Simulated Annealing) Solver for PortfolioAnalytics
###############################################################################

#' @keywords internal
solve_gensa <- function(R, portfolio, constraints, moments, penalty,
                        N, call, trace, search_size, rp,
                        message = FALSE, ...) {
  stopifnot("package:GenSA" %in% search() || requireNamespace("GenSA", quietly = TRUE))

  dots <- list(...)
  maxit <- if (!is.null(dots$maxit)) dots$maxit else N * 50

  controlGenSA <- list(maxit = 5000, threshold.stop = NULL, temperature = 5230,
                       visiting.param = 2.62, acceptance.param = -5, max.time = NULL,
                       nb.stop.improvement = 1e+06, smooth = TRUE, max.call = 1e+07,
                       verbose = FALSE)
  GenSAcargs <- names(controlGenSA)

  # Match user-supplied control parameters
  if (length(dots) > 0) {
    pm <- pmatch(names(dots), GenSAcargs, nomatch = 0L)
    controlGenSA$maxit <- maxit
    controlGenSA[pm[pm > 0L]] <- dots[pm > 0L]
    if (isTRUE(trace)) controlGenSA$verbose <- TRUE
  }

  upper <- constraints$max
  lower <- constraints$min
  warm_start <- dots$warm_start
  if (!is.null(warm_start) && is.numeric(warm_start) && length(warm_start) == N) {
    par <- as.numeric(warm_start)
  } else {
    par <- if (!is.null(rp)) rp[, 1] else rep(1 / N, N)
  }

  minw <- try(GenSA::GenSA(
    par = par,
    lower = lower[1:N], upper = upper[1:N],
    control = controlGenSA,
    fn = constrained_objective,
    R = R, portfolio = portfolio, env = moments, penalty = penalty
  ))

  if (inherits(minw, "try-error")) minw <- NULL
  if (is.null(minw)) {
    message("Optimizer was unable to find a solution for target")
    return(optimization_failure(
      message = "Optimizer was unable to find a solution for target",
      solver = "GenSA",
      call = call
    ))
  }

  weights <- as.vector(minw$par)
  weights <- normalize_portfolio_weights(weights, constraints)
  names(weights) <- colnames(R)
  obj_vals <- constrained_objective(w = weights, R = R, portfolio = portfolio,
                                    trace = TRUE, env = moments,
                                    penalty = penalty)$objective_measures
  out <- list(weights = weights, objective_measures = obj_vals,
              opt_values = obj_vals, out = minw$value, call = call)
  if (isTRUE(trace)) {
    out$GenSAoutput <- minw
  }
  out
}
