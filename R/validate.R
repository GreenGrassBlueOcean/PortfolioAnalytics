###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
###############################################################################


#' Validate a portfolio specification before optimization
#'
#' Checks a \code{portfolio} object (and optionally return data \code{R}) for
#' common configuration errors that would cause cryptic failures deep in solver
#' code.
#'
#' Uses a collect-all-then-report pattern: all issues are gathered in one pass
#' and reported together so the user can fix everything at once rather than
#' playing whack-a-mole.
#'
#' @details
#' The following checks are performed:
#'
#' \strong{Fatal errors (E1--E14):}
#' \describe{
#'   \item{E1}{Portfolio is not of class \code{"portfolio"} (immediate stop).}
#'   \item{E2}{No assets defined.}
#'   \item{E3}{Duplicate asset names.}
#'   \item{E4}{NA or empty-string asset names.}
#'   \item{E5}{\code{R} has fewer columns than assets (when \code{R} provided).}
#'   \item{E6}{Portfolio asset names not found in \code{R} column names.}
#'   \item{E7}{\code{R} has fewer than 2 rows.}
#'   \item{E8}{Any matched column in \code{R} is entirely \code{NA}.}
#'   \item{E9}{Box constraint \code{min > max} for any asset.}
#'   \item{E10}{Weight sum \code{min_sum > max_sum}.}
#'   \item{E11}{Group constraint \code{min > max} for any group.}
#'   \item{E12}{Factor exposure \code{lower > upper} for any factor.}
#'   \item{E13}{Obvious infeasibility: \code{sum(box_max) < min_sum} or
#'     \code{sum(box_min) > max_sum}.}
#'   \item{E14}{Group constraint references asset indices outside \code{1:N}.}
#' }
#'
#' \strong{Warnings (W1--W2):}
#' \describe{
#'   \item{W1}{No constraints defined (package defaults will be used).}
#'   \item{W2}{No objectives defined.}
#' }
#'
#' Bound checks (E9--E14) use the effective constraints returned by
#' \code{\link{get_constraints}}, which applies scalar expansion and defaults.
#' A floating-point tolerance of \code{1e-8} is used for all bound comparisons
#' to avoid false positives.
#'
#' @param portfolio an object of class \code{"portfolio"} created by
#'   \code{\link{portfolio.spec}}
#' @param R optional; an xts, matrix, or data frame of asset returns. When
#'   provided, additional checks verify that \code{R} is compatible with the
#'   portfolio specification.
#'
#' @return Invisibly returns \code{TRUE} if no fatal errors are found.
#'   Emits \code{warning()} for non-fatal issues (W1, W2). Throws
#'   \code{stop()} listing all collected error messages if any fatal error
#'   is detected.
#'
#' @examples
#' \dontrun{
#' data(edhec)
#' portf <- portfolio.spec(assets = colnames(edhec[, 1:4]))
#' portf <- add.constraint(portf, type = "full_investment")
#' portf <- add.constraint(portf, type = "long_only")
#' portf <- add.objective(portf, type = "risk", name = "StdDev")
#'
#' # Validate the portfolio spec alone
#' validate_portfolio(portf)
#'
#' # Validate compatibility with return data
#' validate_portfolio(portf, R = edhec[, 1:4])
#' }
#'
#' @seealso \code{\link{portfolio.spec}}, \code{\link{optimize.portfolio}}
#' @export
validate_portfolio <- function(portfolio, R = NULL) {

  errors   <- character(0)
  warnings <- character(0)
  fp_tol   <- 1e-8

  # ---- E1: Portfolio class (immediate stop, nothing else is meaningful) ----
  if (!is.portfolio(portfolio)) {
    stop("E1: 'portfolio' must be an object of class 'portfolio' ",
         "(created by portfolio.spec()). Got class: ",
         paste(class(portfolio), collapse = ", "),
         call. = FALSE)
  }

  # ---- Portfolio asset checks ----

  asset_names <- names(portfolio$assets)
  n_assets    <- length(portfolio$assets)

  # E2: No assets
  if (n_assets == 0 || is.null(portfolio$assets)) {
    errors <- c(errors, "E2: Portfolio has no assets defined.")
  }

  if (n_assets > 0) {
    # E3: Duplicate asset names
    if (anyDuplicated(asset_names)) {
      dups <- asset_names[duplicated(asset_names)]
      errors <- c(errors, paste0("E3: Duplicate asset names: ",
                                 paste(unique(dups), collapse = ", ")))
    }

    # E4: NA or empty-string asset names
    if (any(is.na(asset_names)) || any(nchar(asset_names) == 0)) {
      errors <- c(errors, "E4: Asset names contain NA or empty strings.")
    }
  }

  # ---- Constraints / objectives warnings ----

  has_constraints <- length(portfolio$constraints) > 0

  # W1: No constraints
  if (!has_constraints) {
    warnings <- c(warnings,
                  "W1: No constraints defined; package defaults will be used.")
  }

  # W2: No objectives
  if (length(portfolio$objectives) == 0) {
    warnings <- c(warnings, "W2: No objectives defined.")
  }

  # ---- Return data checks (only when R is supplied) ----

  if (!is.null(R)) {
    # Coerce to xts if needed (standalone calls may pass raw data)
    if (!inherits(R, "xts")) {
      R <- tryCatch(checkData(R), error = function(e) NULL)
      if (is.null(R)) {
        errors <- c(errors,
                    "R could not be coerced to a usable time series format.")
      }
    }

    if (!is.null(R)) {
      r_colnames <- colnames(R)

      # E7: R has fewer than 2 rows
      if (nrow(R) < 2) {
        errors <- c(errors,
                    paste0("E7: R has ", nrow(R),
                           " row(s); at least 2 are required ",
                           "for covariance estimation."))
      }

      if (n_assets > 0) {
        # E5: R has fewer columns than assets
        if (ncol(R) < n_assets) {
          errors <- c(errors,
                      paste0("E5: R has ", ncol(R),
                             " column(s) but portfolio specifies ",
                             n_assets, " assets."))
        }

        # E6: Portfolio asset names not found in R columns
        if (!is.null(r_colnames)) {
          missing_in_r <- setdiff(asset_names, r_colnames)
          if (length(missing_in_r) > 0) {
            errors <- c(errors,
                        paste0("E6: Portfolio asset(s) not found in R columns: ",
                               paste(missing_in_r, collapse = ", ")))
          }
        }

        # E8: Any matched column is entirely NA
        if (!is.null(r_colnames)) {
          cols_to_check <- intersect(asset_names, r_colnames)
          if (length(cols_to_check) > 0) {
            all_na <- vapply(cols_to_check, function(nm) {
              all(is.na(R[, nm]))
            }, logical(1))
            if (any(all_na)) {
              errors <- c(errors,
                          paste0("E8: Column(s) entirely NA in R: ",
                                 paste(cols_to_check[all_na], collapse = ", ")))
            }
          }
        }
      }
    }
  }

  # ---- Constraint bound checks (via get_constraints effective values) ----

  if (has_constraints && n_assets > 0) {
    constr <- tryCatch(get_constraints(portfolio), error = function(e) NULL)

    if (!is.null(constr)) {

      # E9: Box min > max for any asset
      if (length(constr$min) > 1 && length(constr$max) > 1 &&
          !anyNA(constr$min) && !anyNA(constr$max)) {
        box_violated <- which(constr$min > constr$max + fp_tol)
        if (length(box_violated) > 0) {
          violated_names <- if (!is.null(names(constr$min))) {
            names(constr$min)[box_violated]
          } else {
            as.character(box_violated)
          }
          errors <- c(errors,
                      paste0("E9: Box constraint min > max for: ",
                             paste(violated_names, collapse = ", ")))
        }
      }

      # E10: Weight sum min_sum > max_sum
      if (!is.na(constr$min_sum) && !is.na(constr$max_sum)) {
        if (constr$min_sum > constr$max_sum + fp_tol) {
          errors <- c(errors,
                      paste0("E10: Weight sum min_sum (",
                             constr$min_sum, ") > max_sum (",
                             constr$max_sum, ")."))
        }
      }

      # E11: Group min > max for any group
      if (!is.null(constr$cLO) && !is.null(constr$cUP)) {
        grp_violated <- which(constr$cLO > constr$cUP + fp_tol)
        if (length(grp_violated) > 0) {
          grp_labels <- if (!is.null(constr$group_labels)) {
            constr$group_labels[grp_violated]
          } else {
            paste("group", grp_violated)
          }
          errors <- c(errors,
                      paste0("E11: Group constraint min > max for: ",
                             paste(grp_labels, collapse = ", ")))
        }
      }

      # E12: Factor exposure lower > upper for any factor
      if (!is.null(constr$lower) && !is.null(constr$upper)) {
        fe_violated <- which(constr$lower > constr$upper + fp_tol)
        if (length(fe_violated) > 0) {
          fe_names <- if (!is.null(names(constr$lower))) {
            names(constr$lower)[fe_violated]
          } else {
            as.character(fe_violated)
          }
          errors <- c(errors,
                      paste0("E12: Factor exposure lower > upper for: ",
                             paste(fe_names, collapse = ", ")))
        }
      }

      # E13: Obvious infeasibility (box bounds vs weight sum)
      if (length(constr$min) > 1 && length(constr$max) > 1 &&
          !anyNA(constr$min) && !anyNA(constr$max) &&
          !is.na(constr$min_sum) && !is.na(constr$max_sum)) {
        if (all(is.finite(constr$max)) &&
            sum(constr$max) < constr$min_sum - fp_tol) {
          errors <- c(errors,
                      paste0("E13: sum(box max) = ",
                             round(sum(constr$max), 6),
                             " < min_sum = ", constr$min_sum,
                             "; no feasible portfolio exists."))
        }
        if (all(is.finite(constr$min)) &&
            sum(constr$min) > constr$max_sum + fp_tol) {
          errors <- c(errors,
                      paste0("E13: sum(box min) = ",
                             round(sum(constr$min), 6),
                             " > max_sum = ", constr$max_sum,
                             "; no feasible portfolio exists."))
        }
      }

      # E14: Group references asset indices outside 1:N
      if (!is.null(constr$groups) && is.list(constr$groups)) {
        all_indices <- unlist(constr$groups)
        if (is.numeric(all_indices) && length(all_indices) > 0) {
          out_of_range <- all_indices[all_indices < 1 | all_indices > n_assets]
          if (length(out_of_range) > 0) {
            errors <- c(errors,
                        paste0("E14: Group constraint references asset indices ",
                               "outside 1:", n_assets, ": ",
                               paste(unique(out_of_range), collapse = ", ")))
          }
        }
      }
    }
  }

  # ---- Report ----

  for (w in warnings) {
    warning(w, call. = FALSE)
  }

  if (length(errors) > 0) {
    stop("Portfolio validation failed with ", length(errors), " error(s):\n",
         paste("  -", errors, collapse = "\n"),
         call. = FALSE)
  }

  invisible(TRUE)
}
