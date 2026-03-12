###############################################################################
# Random Portfolios Solver for PortfolioAnalytics
###############################################################################

#' @keywords internal
solve_random <- function(R, portfolio, constraints, moments, penalty,
                         N, call, trace, search_size, rp,
                         message = FALSE, ...) {
  dots <- list(...)
  out <- list()

  if ((constraints$max_sum - constraints$min_sum) < 0.02) {
    message("Leverage constraint min_sum and max_sum are restrictive, ",
            "consider relaxing. e.g. 'full_investment' constraint should be min_sum=0.99 and max_sum=1.01")
  }

  # Generate random portfolios if not provided
  if (is.null(rp)) {
    rp_method <- if (!is.null(dots$rp_method)) dots$rp_method else "sample"
    eliminate <- if (!is.null(dots$eliminate)) dots$eliminate else TRUE
    fev <- if (!is.null(dots$fev)) dots$fev else 0:5
    rp <- random_portfolios(portfolio = portfolio, permutations = search_size,
                            rp_method = rp_method, eliminate = eliminate, fev = fev)
  }
  # Inject warm_start as last row of random portfolio matrix
  warm_start <- dots$warm_start
  if (!is.null(warm_start) && is.numeric(warm_start) &&
      length(warm_start) == N && nrow(rp) > 0) {
    rp[nrow(rp), ] <- as.numeric(warm_start)
  }

  if (isTRUE(trace)) out$random_portfolios <- rp

  # Evaluate objective for each portfolio
  use_parallel <- "package:foreach" %in% search() && is.null(dots$parallel)
  if (use_parallel) {
    ii <- 1
    rp_objective_results <- foreach::foreach(
      ii = 1:nrow(rp), .errorhandling = "pass"
    ) %dopar% constrained_objective(
      w = rp[ii, ], R = R, portfolio = portfolio,
      trace = trace, env = moments, normalize = FALSE, penalty = penalty
    )
  } else {
    rp_objective_results <- apply(rp, 1, constrained_objective,
                                  R = R, portfolio = portfolio,
                                  trace = trace, normalize = FALSE,
                                  env = moments, penalty = penalty)
  }

  if (isTRUE(trace)) out$random_portfolio_objective_results <- rp_objective_results

  # Find best portfolio
  search <- vector(length = length(rp_objective_results))
  for (i in seq_along(search)) {
    if (isTRUE(trace)) {
      search[i] <- ifelse(try(rp_objective_results[[i]]$out),
                           rp_objective_results[[i]]$out, 1e6)
    } else {
      search[i] <- as.numeric(rp_objective_results[[i]])
    }
  }

  best_idx <- which.min(search)
  if (isTRUE(trace)) {
    min_objective_weights <- try(normalize_portfolio_weights(
      rp_objective_results[[best_idx]]$weights, constraints))
  } else {
    min_objective_weights <- try(normalize_portfolio_weights(
      rp[best_idx, ], constraints))
  }

  obj_vals <- try(constrained_objective(
    w = min_objective_weights, R = R, portfolio = portfolio,
    trace = TRUE, normalize = FALSE, env = moments, penalty = penalty
  )$objective_measures)

  out$weights <- min_objective_weights
  out$objective_measures <- obj_vals
  out$opt_values <- obj_vals
  out$call <- call
  out
}
