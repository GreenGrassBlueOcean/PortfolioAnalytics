###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
###############################################################################


# --- Private helpers for constraint classification ---

# Classify a value against two-sided bounds [lo, hi].
# Returns list(status, slack, binding_bound).
# Slack sign: positive = feasible, 0 = binding, negative = violated.
.classify_two_sided <- function(value, lo, hi, tol) {
  below_lo <- is.finite(lo) && (value < lo - tol)
  above_hi <- is.finite(hi) && (value > hi + tol)

  if (below_lo) {
    return(list(status = "violated", slack = value - lo, binding_bound = "lower"))
  }
  if (above_hi) {
    return(list(status = "violated", slack = hi - value, binding_bound = "upper"))
  }

  at_lo <- is.finite(lo) && (abs(value - lo) <= tol)
  at_hi <- is.finite(hi) && (abs(value - hi) <= tol)

  if (at_lo) {
    return(list(status = "binding", slack = 0, binding_bound = "lower"))
  }
  if (at_hi) {
    return(list(status = "binding", slack = 0, binding_bound = "upper"))
  }

  # Inactive: slack = distance to nearest finite bound
  dist_lo <- if (is.finite(lo)) value - lo else Inf
  dist_hi <- if (is.finite(hi)) hi - value else Inf
  list(status = "inactive", slack = min(dist_lo, dist_hi), binding_bound = "none")
}

# Classify a value against an upper-only bound (value <= limit).
.classify_upper_only <- function(value, limit, tol) {
  if (value > limit + tol) {
    list(status = "violated", slack = limit - value)
  } else if (abs(value - limit) <= tol) {
    list(status = "binding", slack = 0)
  } else {
    list(status = "inactive", slack = limit - value)
  }
}

# Classify a value against a lower-only bound (value >= target).
.classify_lower_only <- function(value, target, tol) {
  if (value < target - tol) {
    list(status = "violated", slack = value - target)
  } else if (abs(value - target) <= tol) {
    list(status = "binding", slack = 0)
  } else {
    list(status = "inactive", slack = value - target)
  }
}

# Classify an integer count against an upper limit (count <= limit).
.classify_count_limit <- function(count, limit) {
  if (count > limit) {
    list(status = "violated", slack = as.numeric(limit - count))
  } else if (count == limit) {
    list(status = "binding", slack = 0)
  } else {
    list(status = "inactive", slack = as.numeric(limit - count))
  }
}


#' Check post-optimization constraint feasibility
#'
#' After a solver returns weights, this function checks every enabled constraint
#' in the portfolio specification and returns a structured report with
#' per-constraint pass/fail status, violation magnitudes, binding classification,
#' and slack values.
#'
#' Each constraint element is classified as \code{"binding"} (satisfied at the
#' boundary), \code{"inactive"} (satisfied with slack), \code{"violated"}
#' (exceeds boundary), or \code{"unchecked"} (cannot be validated without
#' additional data). Slack is positive when feasible, zero when binding, and
#' negative when violated.
#'
#' @param weights Named numeric vector of portfolio weights.
#' @param portfolio An object of class \code{portfolio.spec}.
#' @param tolerance Numeric tolerance for floating-point comparisons.
#'   Defaults to \code{sqrt(.Machine$double.eps)}.
#'
#' @return An object of class \code{"feasibility_report"}, a list with:
#'   \item{feasible}{Logical. \code{TRUE} if all checkable constraints are satisfied.}
#'   \item{tolerance}{The tolerance value used.}
#'   \item{summary}{A list with \code{total_checked}, \code{total_violations},
#'     \code{violated_count}, \code{violated_types}, \code{binding_count},
#'     \code{binding_types}, and \code{unchecked_count}.}
#'   \item{violations}{A named list with one entry per constraint type found in
#'     the portfolio. Each entry contains \code{type}, \code{feasible},
#'     \code{status}, \code{slack}, and constraint-specific details.
#'     Two-sided constraints also include \code{binding_bound}.
#'     Constraints that cannot be checked have \code{feasible = NA} and
#'     \code{status = "unchecked"}.}
#'
#' @seealso \code{\link{optimize.portfolio}}, \code{\link{as.data.frame.feasibility_report}}
#' @export
check_portfolio_feasibility <- function(weights,
                                        portfolio,
                                        tolerance = sqrt(.Machine$double.eps)) {

  constraints <- get_constraints(portfolio)
  violations <- list()

  # Guard against NA/NaN weights (e.g. solver failure returning NA)
  if (any(is.na(weights))) {
    violations$na_weights <- list(
      type     = "na_weights",
      feasible = FALSE,
      status   = "violated",
      note     = paste0(sum(is.na(weights)), " of ", length(weights), " weights are NA")
    )
    report <- structure(
      list(
        feasible   = FALSE,
        tolerance  = tolerance,
        summary    = list(
          total_checked    = 1L,
          total_violations = 1L,
          violated_count   = 1L,
          violated_types   = "na_weights",
          binding_count    = 0L,
          binding_types    = character(0),
          unchecked_count  = 0L
        ),
        violations = violations
      ),
      class = "feasibility_report"
    )
    return(report)
  }

  # --- weight_sum (leverage) ---
  if (!is.null(constraints$min_sum) && !is.null(constraints$max_sum)) {
    ws <- sum(weights)
    lo <- constraints$min_sum
    hi <- constraints$max_sum
    viol <- 0
    if (ws < lo - tolerance) viol <- ws - lo
    if (ws > hi + tolerance) viol <- ws - hi
    cls <- .classify_two_sided(ws, lo, hi, tolerance)
    violations$weight_sum <- list(
      type          = "weight_sum",
      feasible      = abs(viol) <= tolerance,
      value         = ws,
      bound         = c(min_sum = lo, max_sum = hi),
      violation     = viol,
      status        = cls$status,
      slack         = cls$slack,
      binding_bound = cls$binding_bound
    )
  }

  # --- box ---
  if (!is.null(constraints$min) && !is.null(constraints$max)) {
    lo <- constraints$min
    hi <- constraints$max
    per_asset_signed <- ifelse(weights > hi + tolerance, weights - hi,
                        ifelse(weights < lo - tolerance, weights - lo, 0))
    n_viol <- sum(abs(per_asset_signed) > tolerance)

    n_a <- length(weights)
    box_status <- character(n_a)
    box_slack  <- numeric(n_a)
    box_bb     <- character(n_a)
    names(box_status) <- names(weights)
    names(box_slack)  <- names(weights)
    names(box_bb)     <- names(weights)

    for (j in seq_len(n_a)) {
      cls <- .classify_two_sided(weights[j], lo[j], hi[j], tolerance)
      box_status[j] <- cls$status
      box_slack[j]  <- cls$slack
      box_bb[j]     <- cls$binding_bound
    }

    violations$box <- list(
      type          = "box",
      feasible      = n_viol == 0L,
      n_violations  = n_viol,
      details       = per_asset_signed,
      status        = box_status,
      slack         = box_slack,
      binding_bound = box_bb,
      n_binding     = sum(box_status == "binding")
    )
  }

  # --- group ---
  if (!is.null(constraints$groups) && !is.null(constraints$cLO) && !is.null(constraints$cUP)) {
    groups <- constraints$groups
    cLO <- constraints$cLO
    cUP <- constraints$cUP
    group_pos <- constraints$group_pos
    n_groups <- length(groups)
    labels <- constraints$group_labels
    if (is.null(labels)) labels <- paste0("group_", seq_len(n_groups))

    group_details <- vector("list", n_groups)
    names(group_details) <- labels
    any_fail <- FALSE
    grp_status <- character(n_groups)
    names(grp_status) <- labels

    for (i in seq_len(n_groups)) {
      gw <- sum(weights[groups[[i]]])
      viol <- 0
      if (gw < cLO[i] - tolerance) viol <- gw - cLO[i]
      if (gw > cUP[i] + tolerance) viol <- gw - cUP[i]
      # Check group position limit
      pos_fail <- FALSE
      if (!is.null(group_pos)) {
        n_nz <- sum(abs(weights[groups[[i]]]) > tolerance)
        pos_fail <- n_nz > group_pos[i]
      }
      ok <- abs(viol) <= tolerance && !pos_fail
      if (!ok) any_fail <- TRUE

      cls <- .classify_two_sided(gw, cLO[i], cUP[i], tolerance)
      # Position limit failure overrides to violated
      if (pos_fail && cls$status != "violated") {
        cls$status <- "violated"
        cls$slack  <- -1
      }
      grp_status[i] <- cls$status

      group_details[[i]] <- list(
        feasible       = ok,
        weight_sum     = gw,
        bound          = c(cLO = cLO[i], cUP = cUP[i]),
        violation      = viol,
        pos_limit_fail = pos_fail,
        status         = cls$status,
        slack          = cls$slack,
        binding_bound  = cls$binding_bound
      )
    }
    violations$group <- list(
      type     = "group",
      feasible = !any_fail,
      details  = group_details,
      status   = grp_status
    )
  }

  # --- position_limit ---
  if (!is.null(constraints$max_pos) || !is.null(constraints$max_pos_long) || !is.null(constraints$max_pos_short)) {
    max_pos <- constraints$max_pos
    max_pos_long <- constraints$max_pos_long
    max_pos_short <- constraints$max_pos_short

    n_nonzero <- sum(abs(weights) > tolerance)
    n_long    <- sum(weights > tolerance)
    n_short   <- sum(weights < -tolerance)

    details <- list()
    any_fail <- FALSE
    pl_status <- character(0)

    if (!is.null(max_pos)) {
      fail <- n_nonzero > max_pos
      if (fail) any_fail <- TRUE
      cls <- .classify_count_limit(n_nonzero, max_pos)
      details$max_pos <- list(value = n_nonzero, limit = max_pos, feasible = !fail,
                              status = cls$status, slack = cls$slack)
      pl_status <- c(pl_status, cls$status)
    }
    if (!is.null(max_pos_long)) {
      fail <- n_long > max_pos_long
      if (fail) any_fail <- TRUE
      cls <- .classify_count_limit(n_long, max_pos_long)
      details$max_pos_long <- list(value = n_long, limit = max_pos_long, feasible = !fail,
                                   status = cls$status, slack = cls$slack)
      pl_status <- c(pl_status, cls$status)
    }
    if (!is.null(max_pos_short)) {
      fail <- n_short > max_pos_short
      if (fail) any_fail <- TRUE
      cls <- .classify_count_limit(n_short, max_pos_short)
      details$max_pos_short <- list(value = n_short, limit = max_pos_short, feasible = !fail,
                                    status = cls$status, slack = cls$slack)
      pl_status <- c(pl_status, cls$status)
    }
    violations$position_limit <- list(
      type     = "position_limit",
      feasible = !any_fail,
      details  = details,
      status   = pl_status
    )
  }

  # --- leverage_exposure ---
  if (!is.null(constraints$leverage)) {
    lev <- sum(abs(weights))
    limit <- constraints$leverage
    viol <- if (lev > limit + tolerance) lev - limit else 0
    cls <- .classify_upper_only(lev, limit, tolerance)
    violations$leverage_exposure <- list(
      type      = "leverage_exposure",
      feasible  = viol <= tolerance,
      value     = lev,
      limit     = limit,
      violation = viol,
      status    = cls$status,
      slack     = cls$slack
    )
  }

  # --- factor_exposure ---
  if (!is.null(constraints$B)) {
    B <- constraints$B
    lower <- constraints$lower
    upper <- constraints$upper
    exposures <- as.numeric(t(B) %*% weights)
    n_factors <- length(exposures)
    factor_labels <- colnames(B)
    if (is.null(factor_labels)) factor_labels <- paste0("factor_", seq_len(n_factors))

    per_factor <- numeric(n_factors)
    names(per_factor) <- factor_labels
    fe_status <- character(n_factors)
    fe_slack  <- numeric(n_factors)
    fe_bb     <- character(n_factors)
    names(fe_status) <- factor_labels
    names(fe_slack)  <- factor_labels
    names(fe_bb)     <- factor_labels

    for (i in seq_len(n_factors)) {
      if (exposures[i] < lower[i] - tolerance) per_factor[i] <- exposures[i] - lower[i]
      if (exposures[i] > upper[i] + tolerance) per_factor[i] <- exposures[i] - upper[i]
      cls <- .classify_two_sided(exposures[i], lower[i], upper[i], tolerance)
      fe_status[i] <- cls$status
      fe_slack[i]  <- cls$slack
      fe_bb[i]     <- cls$binding_bound
    }
    n_viol <- sum(abs(per_factor) > tolerance)

    violations$factor_exposure <- list(
      type          = "factor_exposure",
      feasible      = n_viol == 0L,
      exposures     = exposures,
      lower         = lower,
      upper         = upper,
      n_violations  = n_viol,
      details       = per_factor,
      status        = fe_status,
      slack         = fe_slack,
      binding_bound = fe_bb,
      n_binding     = sum(fe_status == "binding")
    )
  }

  # --- diversification ---
  if (!is.null(constraints$div_target)) {
    div_val <- diversification(weights)
    target <- constraints$div_target
    viol <- div_val - target
    cls <- .classify_lower_only(div_val, target, tolerance)
    violations$diversification <- list(
      type      = "diversification",
      feasible  = div_val >= target - tolerance,
      value     = div_val,
      target    = target,
      violation = viol,
      status    = cls$status,
      slack     = cls$slack
    )
  }

  # --- constraints that cannot be checked without additional data ---
  if (!is.null(constraints$turnover_target)) {
    violations$turnover <- list(
      type     = "turnover",
      feasible = NA,
      status   = "unchecked",
      note     = "Not checked: requires previous weights"
    )
  }
  if (!is.null(constraints$return_target)) {
    violations$return_target <- list(
      type     = "return_target",
      feasible = NA,
      status   = "unchecked",
      note     = "Not checked: requires moment estimates"
    )
  }
  if (!is.null(constraints$ptc)) {
    violations$transaction_cost <- list(
      type     = "transaction_cost",
      feasible = NA,
      status   = "unchecked",
      note     = "Not checked: requires previous weights"
    )
  }

  # --- build summary ---
  checkable <- vapply(violations, function(v) !is.na(v$feasible), logical(1))
  checked_violations <- violations[checkable]
  feasible_flags <- vapply(checked_violations, function(v) v$feasible, logical(1))
  violated_names <- names(checked_violations)[!feasible_flags]

  # Binding: any constraint with at least one binding element
  binding_names <- character(0)
  for (nm in names(checked_violations)) {
    st <- checked_violations[[nm]]$status
    if (!is.null(st) && any(st == "binding")) {
      binding_names <- c(binding_names, nm)
    }
  }

  unchecked_count <- sum(!checkable)

  report <- structure(
    list(
      feasible   = all(feasible_flags),
      tolerance  = tolerance,
      summary    = list(
        total_checked    = sum(checkable),
        total_violations = length(violated_names),
        violated_count   = length(violated_names),
        violated_types   = violated_names,
        binding_count    = length(binding_names),
        binding_types    = binding_names,
        unchecked_count  = unchecked_count
      ),
      violations = violations
    ),
    class = "feasibility_report"
  )
  report
}


#' Convert a feasibility report to a tidy data frame
#'
#' Returns one row per atomic constraint check with columns for type, element,
#' value, bounds, violation, slack, status, and binding bound.
#'
#' @param x An object of class \code{"feasibility_report"}.
#' @param ... Ignored (for S3 method compatibility).
#' @param row.names Ignored.
#' @param optional Ignored.
#'
#' @return A \code{data.frame} with columns: \code{type}, \code{element},
#'   \code{value}, \code{lower}, \code{upper}, \code{violation}, \code{slack},
#'   \code{status}, \code{binding_bound}.
#'
#' @examples
#' \dontrun{
#' opt <- optimize.portfolio(R, portf, optimize_method = "ROI")
#' df <- as.data.frame(opt$feasibility_report)
#' df[df$status == "binding", ]
#' }
#'
#' @method as.data.frame feasibility_report
#' @export
as.data.frame.feasibility_report <- function(x, row.names = NULL, optional = FALSE, ...) {
  rows <- list()
  viols <- x$violations

  for (nm in names(viols)) {
    v <- viols[[nm]]

    if (v$type == "weight_sum") {
      rows[[length(rows) + 1L]] <- data.frame(
        type = "weight_sum", element = "total",
        value = v$value, lower = v$bound[["min_sum"]], upper = v$bound[["max_sum"]],
        violation = v$violation, slack = v$slack,
        status = v$status, binding_bound = v$binding_bound,
        stringsAsFactors = FALSE
      )

    } else if (v$type == "box") {
      nms <- names(v$status)
      if (is.null(nms)) nms <- seq_along(v$status)
      rows[[length(rows) + 1L]] <- data.frame(
        type = "box", element = nms,
        value = NA_real_, lower = NA_real_, upper = NA_real_,
        violation = as.numeric(v$details),
        slack = as.numeric(v$slack),
        status = as.character(v$status),
        binding_bound = as.character(v$binding_bound),
        stringsAsFactors = FALSE
      )

    } else if (v$type == "group") {
      for (lbl in names(v$details)) {
        d <- v$details[[lbl]]
        rows[[length(rows) + 1L]] <- data.frame(
          type = "group", element = lbl,
          value = d$weight_sum,
          lower = d$bound[["cLO"]], upper = d$bound[["cUP"]],
          violation = d$violation, slack = d$slack,
          status = d$status, binding_bound = d$binding_bound,
          stringsAsFactors = FALSE
        )
      }

    } else if (v$type == "position_limit") {
      for (sub_nm in names(v$details)) {
        d <- v$details[[sub_nm]]
        rows[[length(rows) + 1L]] <- data.frame(
          type = "position_limit", element = sub_nm,
          value = d$value, lower = NA_real_, upper = d$limit,
          violation = NA_real_, slack = d$slack,
          status = d$status, binding_bound = NA_character_,
          stringsAsFactors = FALSE
        )
      }

    } else if (v$type == "leverage_exposure") {
      rows[[length(rows) + 1L]] <- data.frame(
        type = "leverage_exposure", element = "total",
        value = v$value, lower = NA_real_, upper = v$limit,
        violation = v$violation, slack = v$slack,
        status = v$status, binding_bound = NA_character_,
        stringsAsFactors = FALSE
      )

    } else if (v$type == "factor_exposure") {
      nms <- names(v$status)
      if (is.null(nms)) nms <- seq_along(v$status)
      rows[[length(rows) + 1L]] <- data.frame(
        type = "factor_exposure", element = nms,
        value = v$exposures, lower = v$lower, upper = v$upper,
        violation = as.numeric(v$details),
        slack = as.numeric(v$slack),
        status = as.character(v$status),
        binding_bound = as.character(v$binding_bound),
        stringsAsFactors = FALSE
      )

    } else if (v$type == "diversification") {
      rows[[length(rows) + 1L]] <- data.frame(
        type = "diversification", element = "total",
        value = v$value, lower = v$target, upper = NA_real_,
        violation = v$violation, slack = v$slack,
        status = v$status, binding_bound = NA_character_,
        stringsAsFactors = FALSE
      )

    } else if (v$type == "na_weights") {
      rows[[length(rows) + 1L]] <- data.frame(
        type = "na_weights", element = "total",
        value = NA_real_, lower = NA_real_, upper = NA_real_,
        violation = NA_real_, slack = NA_real_,
        status = "violated", binding_bound = NA_character_,
        stringsAsFactors = FALSE
      )

    } else if (isTRUE(v$status == "unchecked")) {
      rows[[length(rows) + 1L]] <- data.frame(
        type = v$type, element = v$type,
        value = NA_real_, lower = NA_real_, upper = NA_real_,
        violation = NA_real_, slack = NA_real_,
        status = "unchecked", binding_bound = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      type = character(0), element = character(0),
      value = numeric(0), lower = numeric(0), upper = numeric(0),
      violation = numeric(0), slack = numeric(0),
      status = character(0), binding_bound = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' @method print feasibility_report
#' @export
print.feasibility_report <- function(x, ...) {
  if (x$feasible) {
    cat("Feasibility Report: ALL CONSTRAINTS SATISFIED\n")
  } else {
    cat("Feasibility Report: VIOLATIONS DETECTED\n")
  }
  cat("  Constraints checked:", x$summary$total_checked, "\n")

  # Compact status line
  bc <- x$summary$binding_count
  vc <- x$summary$violated_count
  uc <- x$summary$unchecked_count
  if (!is.null(bc)) {
    cat("  Binding:", bc, "| Violated:", vc, "| Unchecked:", uc, "\n")
  } else {
    # Backward compat for reports created before binding detection
    cat("  Violations found:  ", x$summary$total_violations, "\n")
  }
  cat("  Tolerance:         ", format(x$tolerance, digits = 4), "\n")

  # Binding constraints
  if (!is.null(x$summary$binding_types) && length(x$summary$binding_types) > 0L) {
    cat("\nBinding constraints:\n")
    for (nm in x$summary$binding_types) {
      v <- x$violations[[nm]]
      cat("  -", v$type)
      if (length(v$status) == 1L) {
        # Scalar constraint
        if (!is.null(v$binding_bound)) {
          cat(" (binding at", v$binding_bound, "bound)")
        }
      } else {
        # Vector constraint
        n_bind <- sum(v$status == "binding")
        cat(" (", n_bind, "element(s) at bound)")
      }
      cat("\n")
    }
  }

  if (x$summary$total_violations > 0L) {
    cat("\nViolated constraints:\n")
    for (nm in x$summary$violated_types) {
      v <- x$violations[[nm]]
      cat("  -", v$type)
      if (!is.null(v$violation) && length(v$violation) == 1L) {
        cat(" (violation:", format(v$violation, digits = 6), ")")
      }
      if (!is.null(v$n_violations)) {
        cat(" (", v$n_violations, "assets/factors violated)")
      }
      cat("\n")
    }
  }

  # Note unchecked constraints
  unchecked <- Filter(function(v) isTRUE(is.na(v$feasible)), x$violations)
  if (length(unchecked) > 0L) {
    cat("\nNot checked:\n")
    for (v in unchecked) {
      cat("  -", v$type, ":", v$note, "\n")
    }
  }
  invisible(x)
}
