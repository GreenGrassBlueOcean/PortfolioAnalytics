###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
###############################################################################

# Environment to track which deprecation warnings have already fired.
# Used for "hot-path" v1 functions that may be called thousands of times
# per optimization (e.g. constrained_objective_v1, randomize_portfolio_v1).
.deprecation_warned <- new.env(parent = emptyenv())

#' Emit a deprecation warning at most once per session
#'
#' For functions that are called in tight loops (e.g. objective evaluation
#' inside DEoptim), standard \code{.Deprecated()} would produce thousands of
#' warnings. This helper fires once and then silently returns.
#'
#' @param old character; name of the deprecated function
#' @param new character; name of the replacement function
#' @param msg optional custom message (overrides default)
#' @keywords internal
deprecate_once <- function(old, new, msg = NULL) {
  if (isTRUE(.deprecation_warned[[old]])) return(invisible(NULL))
  .deprecation_warned[[old]] <- TRUE
  if (is.null(msg)) {
    msg <- gettextf("'%s' is deprecated.\nUse '%s' instead.\nSee help(\"Deprecated\")",
                    old, new)
  }
  warning(msg, call. = FALSE)
}
