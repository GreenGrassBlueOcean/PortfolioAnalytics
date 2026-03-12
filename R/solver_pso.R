###############################################################################
# PSO (Particle Swarm Optimization) Solver for PortfolioAnalytics
###############################################################################

#' @keywords internal
solve_pso <- function(R, portfolio, constraints, moments, penalty,
                      N, call, trace, search_size, rp,
                      message = FALSE, ...) {
  stopifnot("package:pso" %in% search() || requireNamespace("pso", quietly = TRUE))

  dots <- list(...)
  maxit <- if (!is.null(dots$maxit)) dots$maxit else N * 50

  controlPSO <- list(trace = FALSE, fnscale = 1, maxit = 1000,
                     maxf = Inf, abstol = -Inf, reltol = 0)
  PSOcargs <- names(controlPSO)

  # Match user-supplied control parameters
  if (length(dots) > 0) {
    pm <- pmatch(names(dots), PSOcargs, nomatch = 0L)
    controlPSO$maxit <- maxit
    controlPSO[pm[pm > 0L]] <- dots[pm > 0L]
    if (is.null(dots$reltol)) controlPSO$reltol <- 0.000001
    if (isTRUE(trace)) {
      controlPSO$trace <- TRUE
      controlPSO$trace.stats <- TRUE
    }
  }

  upper <- constraints$max
  lower <- constraints$min
  warm_start <- dots$warm_start
  pso_par <- if (!is.null(warm_start) && is.numeric(warm_start) &&
                  length(warm_start) == N) {
    as.numeric(warm_start)
  } else {
    rep(NA, N)
  }

  minw <- try(pso::psoptim(
    par = pso_par,
    fn = constrained_objective,
    R = R, portfolio = portfolio, env = moments, penalty = penalty,
    lower = lower[1:N], upper = upper[1:N],
    control = controlPSO
  ))

  if (inherits(minw, "try-error")) minw <- NULL
  if (is.null(minw)) {
    message("Optimizer was unable to find a solution for target")
    return(optimization_failure(
      message = "Optimizer was unable to find a solution for target",
      solver = "pso",
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
    out$PSOoutput <- minw
  }
  out
}
