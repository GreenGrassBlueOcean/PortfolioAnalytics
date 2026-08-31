
modify.args <- function(formals, arglist, ..., dots=FALSE)
{
  # modify.args function from quantstrat
  
  # avoid evaluating '...' to make things faster
  dots.names <- eval(substitute(alist(...)))
  
  if(missing(arglist))
    arglist <- NULL
  arglist <- c(arglist, dots.names)
  
  # see 'S Programming' p. 67 for this matching
  
  # nothing to do if arglist is empty; return formals
  if(!length(arglist))
    return(formals)
  
  argnames <- names(arglist)
  if(!is.list(arglist) && !is.null(argnames) && !any(argnames == ""))
    stop("'arglist' must be a *named* list, with no names == \"\"")
  
  .formals  <- formals
  onames <- names(.formals)
  
  pm <- pmatch(argnames, onames, nomatch = 0L)
  #if(any(pm == 0L))
  #    message(paste("some arguments stored for", fun, "do not match"))
  names(arglist[pm > 0L]) <- onames[pm]
  .formals[pm] <- arglist[pm > 0L]
  
  # include all elements from arglist if function formals contain '...'
  if(dots && !is.null(.formals$...)) {
    dotnames <- names(arglist[pm == 0L])
    .formals[dotnames] <- arglist[dotnames]
    #.formals$... <- NULL  # should we assume we matched them all?
  }
  .formals
}

# This is how it is used in quantstrat in applyIndicators()
# # replace default function arguments with indicator$arguments
# .formals <- formals(indicator$name)
# .formals <- modify.args(.formals, indicator$arguments, dots=TRUE)
# # now add arguments from parameters
# .formals <- modify.args(.formals, parameters, dots=TRUE)
# # now add dots
# .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
# # remove ... to avoid matching multiple args
# .formals$`...` <- NULL
# 
# tmp_val <- do.call(indicator$name, .formals)


#' Was an optional dot-argument meaningfully supplied?
#'
#' Optional arguments to `optimize.portfolio()` are read with the idiom
#' `if (hasArg(x)) x <- eval.parent(match.call(expand.dots = TRUE)$x)` and then
#' tested with `is.na(x)`. That test is unsafe in three ways, each surfacing as
#' an error far from its cause:
#'
#' 1. `hasArg()` is TRUE for an argument passed explicitly as `NULL`, so the
#'    "supplied" branch is taken and `is.na(NULL)` yields `logical(0)`.
#'    Arithmetic on that gives a zero-length value and the next `if()` fails
#'    with "argument is of length zero" -- for `itermax` this happened before
#'    DEoptim was ever invoked.
#' 2. In the `!hasArg(x) || is.na(...)` guards the same `logical(0)` makes the
#'    `||` evaluate to `NA`, so `if (NA)` fails with "missing value where
#'    TRUE/FALSE needed".
#' 3. For a vector argument such as `packages`, `is.na()` returns a vector and
#'    since R 4.3 `||` errors with "'length = 2' in coercion to 'logical(1)'",
#'    making that documented argument impossible to supply.
#'
#' Treat `NULL`, zero-length and a length-one `NA` as "not supplied" -- the
#' semantics the original guards were reaching for -- and everything else,
#' including multi-element vectors, as supplied. Omitting an argument and
#' passing it as `NULL` then mean the same thing.
#'
#' @param x The evaluated argument, or `NULL` when it was not passed.
#' @return `TRUE` when the caller did not meaningfully supply a value.
#' @keywords internal
#' @noRd
.pa_arg_missing <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0L) return(TRUE)
  if (length(x) == 1L && is.atomic(x) && is.na(x)) return(TRUE)
  FALSE
}


###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2021 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
