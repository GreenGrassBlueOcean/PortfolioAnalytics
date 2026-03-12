###############################################################################
# Solver Registry and Dispatch
#
# Maps optimize_method names to solver functions. Built-in solvers are
# registered via .solver_dispatch; users can add custom solvers via
# register_solver().
###############################################################################

# Built-in solver dispatch table: optimize_method name -> function name
.solver_dispatch <- c(
  DEoptim  = "solve_deoptim",
  random   = "solve_random",
  ROI      = "solve_roi",
  quadprog = "solve_roi",
  glpk     = "solve_roi",
  symphony = "solve_roi",
  ipop     = "solve_roi",
  pso      = "solve_pso",
  GenSA    = "solve_gensa",
  CVXR     = "solve_cvxr",
  cvxr     = "solve_cvxr",
  CBC      = "solve_cvxr",
  GLPK     = "solve_cvxr",
  GLPK_MI  = "solve_cvxr",
  OSQP     = "solve_cvxr",
  CPLEX    = "solve_cvxr",
  SCS      = "solve_cvxr",
  ECOS     = "solve_cvxr",
  GUROBI   = "solve_cvxr",
  MOSEK    = "solve_cvxr"
)

# Environment for user-registered solvers
.solver_user_registry <- new.env(parent = emptyenv())

#' Register a Custom Solver for optimize.portfolio
#'
#' Register a custom solver function that can be used with
#' \code{\link{optimize.portfolio}} via the \code{optimize_method} argument.
#'
#' @param name character string. The name to use as the \code{optimize_method}
#'   value (e.g., \code{"my_solver"}).
#' @param fn a function following the solver contract. Must accept the standard
#'   solver arguments: \code{R}, \code{portfolio}, \code{constraints},
#'   \code{moments}, \code{penalty}, \code{N}, \code{call}, \code{trace},
#'   \code{search_size}, \code{rp}, \code{message}, \code{...}.
#'   Must return a list with at least \code{weights}, \code{objective_measures},
#'   \code{opt_values}, \code{out}, and \code{call}.
#' @return Invisibly returns \code{NULL}. Called for its side effect.
#' @export
register_solver <- function(name, fn) {
  if (!is.character(name) || length(name) != 1L) {
    stop("'name' must be a single character string")
  }
  if (!is.function(fn)) {
    stop("'fn' must be a function")
  }
  assign(name, fn, envir = .solver_user_registry)
  invisible(NULL)
}

#' Look up a solver function by optimize_method name
#'
#' @param name character string. The optimize_method name.
#' @return The solver function, or NULL if not found.
#' @keywords internal
get_solver <- function(name) {
  # Check built-in dispatch table first
  if (name %in% names(.solver_dispatch)) {
    fn_name <- .solver_dispatch[[name]]
    ns <- asNamespace("PortfolioAnalytics")
    if (exists(fn_name, envir = ns, inherits = FALSE)) {
      return(get(fn_name, envir = ns, inherits = FALSE))
    }
  }
  # Check user-registered solvers

  if (exists(name, envir = .solver_user_registry, inherits = FALSE)) {
    return(get(name, envir = .solver_user_registry, inherits = FALSE))
  }
  NULL
}


#' Normalize portfolio weights to min_sum / max_sum bounds
#'
#' @param weights numeric vector of portfolio weights
#' @param constraints list of constraints (from get_constraints())
#' @return normalized weights vector
#' @keywords internal
normalize_portfolio_weights <- function(weights, constraints) {
  if (!is.null(constraints$min_sum) | !is.null(constraints$max_sum)) {
    if (!is.null(constraints$max_sum) & constraints$max_sum != Inf) {
      if (sum(weights) > constraints$max_sum) {
        weights <- (constraints$max_sum / sum(weights)) * weights
      }
    }
    if (!is.null(constraints$min_sum) & constraints$min_sum != -Inf) {
      if (sum(weights) < constraints$min_sum) {
        weights <- (constraints$min_sum / sum(weights)) * weights
      }
    }
  }
  weights
}
