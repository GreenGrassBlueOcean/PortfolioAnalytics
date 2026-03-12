###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
###############################################################################

#' Structured error object for failed portfolio optimizations
#'
#' When a solver fails to find a solution, \code{optimize.portfolio} returns an
#' \code{optimization_failure} object instead of a plain character string.
#' This allows callers to detect failures programmatically via
#' \code{inherits(result, "optimization_failure")} or
#' \code{is.optimization_failure(result)}, and to use \code{tryCatch}-style
#' dispatch.
#'
#' @param message Character. Human-readable description of the failure.
#' @param solver Character. Name of the solver that failed (e.g. \code{"DEoptim"},
#'   \code{"pso"}, \code{"GenSA"}).
#' @param call The original call to \code{optimize.portfolio}, if available.
#' @param error The original error object from \code{try()}, if available.
#'
#' @return An object of class \code{c("optimization_failure",
#'   "optimize.portfolio")}, a list with components:
#'   \item{message}{The failure message.}
#'   \item{solver}{The solver name.}
#'   \item{call}{The call that produced the failure.}
#'   \item{error}{The original error, or \code{NULL}.}
#'   \item{weights}{Named numeric vector of \code{NA} weights (one per asset).}
#'   \item{objective_measures}{\code{NA}.}
#'   \item{opt_values}{\code{NA}.}
#'   \item{elapsed_time}{\code{NA}.}
#'
#' @examples
#' \dontrun{
#' result <- optimize.portfolio(R, portfolio, optimize_method = "DEoptim")
#' if (is.optimization_failure(result)) {
#'   warning("Optimization failed: ", result$message)
#' }
#' }
#'
#' @seealso \code{\link{optimize.portfolio}}, \code{\link{is.optimization_failure}}
#' @export
optimization_failure <- function(message,
                                 solver = "unknown",
                                 call = NULL,
                                 error = NULL) {
  structure(
    list(
      message            = message,
      solver             = solver,
      call               = call,
      error              = error,
      weights            = NA_real_,
      objective_measures = NA,
      opt_values         = NA,
      elapsed_time       = NA
    ),
    class = c("optimization_failure", "optimize.portfolio")
  )
}

#' Test if an object is an optimization failure
#'
#' @param x Any R object.
#' @return Logical; \code{TRUE} if \code{x} inherits from
#'   \code{"optimization_failure"}.
#' @seealso \code{\link{optimization_failure}}
#' @export
is.optimization_failure <- function(x) {
  inherits(x, "optimization_failure")
}

#' @method print optimization_failure
#' @export
print.optimization_failure <- function(x, ...) {
  cat("Optimization Failure\n")
  cat("  Solver :", x$solver, "\n")
  cat("  Message:", x$message, "\n")
  if (!is.null(x$error)) {
    cat("  Error  :", conditionMessage(x$error), "\n")
  }
  invisible(x)
}
