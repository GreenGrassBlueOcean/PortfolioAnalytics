###############################################################################
# ROI Solver for PortfolioAnalytics
#
# Handles ROI, quadprog, glpk, symphony, ipop optimize_method values.
# Delegates to optFUN.R helper functions (gmv_opt, maxret_opt, etl_opt, etc.)
###############################################################################

#' @keywords internal
solve_roi <- function(R, portfolio, constraints, moments, penalty,
                      N, call, trace, search_size, rp,
                      message = FALSE, optimize_method = "ROI", ...) {
  # ROI.plugin.symphony registers the symphony solver with ROI on load

  if (optimize_method == "symphony") {
    if (!requireNamespace("ROI.plugin.symphony", quietly = TRUE)) {
      stop("Package 'ROI.plugin.symphony' is required for optimize_method='symphony'. ",
           "Please install it with install.packages('ROI.plugin.symphony').",
           call. = FALSE)
    }
  }
  dots <- list(...)
  out <- list()

  # The momentFUN output is used for mu/sigma estimates
  mout <- moments

  control <- dots$control

  # Build ROI-internal moments list (separate from the momentFUN output)
  roi_moments <- list(mean = rep(0, N))
  alpha <- 0.05
  target <- if (!is.null(constraints$return_target)) constraints$return_target else NA
  lambda <- 1
  lambda_hhi <- NULL
  conc_groups <- NULL

  valid_objnames <- c("HHI", "mean", "var", "sd", "StdDev", "CVaR", "ES", "ETL")

  for (objective in portfolio$objectives) {
    if (objective$enabled) {
      if (!(objective$name %in% valid_objnames)) {
        stop("ROI only solves mean, var/StdDev, HHI, or sample ETL/ES/CVaR type business objectives, choose a different optimize_method.")
      }

      arguments <- objective$arguments
      clean <- if (!is.null(arguments$clean)) arguments$clean else "none"
      if (!is.null(arguments[["p"]])) alpha <- arguments$p
      if (alpha > 0.5) alpha <- (1 - alpha)

      if (clean != "none") roi_moments$cleanR <- Return.clean(R = R, method = clean)

      if (objective$name == "mean") {
        if (!is.null(mout$mu)) {
          roi_moments[["mean"]] <- as.vector(mout$mu)
        } else {
          roi_moments[["mean"]] <- try(as.vector(apply(Return.clean(R = R, method = clean), 2, "mean", na.rm = TRUE)), silent = TRUE)
        }
      } else if (objective$name %in% c("StdDev", "sd", "var")) {
        if (!is.null(mout$sigma)) {
          roi_moments[["var"]] <- mout$sigma
        } else {
          roi_moments[["var"]] <- try(var(x = Return.clean(R = R, method = clean), na.rm = TRUE), silent = TRUE)
        }
      } else if (objective$name %in% c("CVaR", "ES", "ETL")) {
        roi_moments[[objective$name]] <- ""
      } else {
        roi_moments[[objective$name]] <- try(eval(as.symbol(objective$name))(Return.clean(R = R, method = clean)), silent = TRUE)
      }

      target <- ifelse(!is.null(objective$target), objective$target, target)
      lambda <- ifelse(!is.null(objective$risk_aversion), objective$risk_aversion, lambda)
      if (!is.null(objective$conc_aversion)) lambda_hhi <- objective$conc_aversion
      if (!is.null(objective$conc_groups)) conc_groups <- objective$conc_groups
    }
  }

  # --- Variance-based optimization (QP) ---
  if ("var" %in% names(roi_moments)) {
    solver <- if (optimize_method == "ROI") "quadprog" else optimize_method

    if (!is.null(constraints$turnover_target) | !is.null(constraints$ptc) | !is.null(constraints$leverage)) {
      if (!is.null(constraints$turnover_target) & !is.null(constraints$ptc)) {
        warning("Turnover and proportional transaction cost constraints detected, only running optimization for turnover constraint.")
        constraints$ptc <- NULL
      }
      if (!is.null(constraints$turnover_target) & is.null(constraints$ptc)) {
        qp_result <- gmv_opt_toc(R = R, constraints = constraints, moments = roi_moments,
                                  lambda = lambda, target = target,
                                  init_weights = portfolio$assets, solver = solver, control = control)
        out <- list(weights = qp_result$weights, objective_measures = qp_result$obj_vals,
                    opt_values = qp_result$obj_vals, out = qp_result$out, call = call)
      }
      if (!is.null(constraints$ptc) & is.null(constraints$turnover_target)) {
        qp_result <- gmv_opt_ptc(R = R, constraints = constraints, moments = roi_moments,
                                  lambda = lambda, target = target,
                                  init_weights = portfolio$assets, solver = solver, control = control)
        out <- list(weights = qp_result$weights, objective_measures = qp_result$obj_vals,
                    opt_values = qp_result$obj_vals, out = qp_result$out, call = call)
      }
      if (!is.null(constraints$leverage)) {
        qp_result <- gmv_opt_leverage(R = R, constraints = constraints, moments = roi_moments,
                                       lambda = lambda, target = target, solver = solver, control = control)
        out <- list(weights = qp_result$weights, objective_measures = qp_result$obj_vals,
                    opt_values = qp_result$obj_vals, out = qp_result$out, call = call)
      }
    } else {
      maxSR <- if (!is.null(dots$maxSR)) dots$maxSR else FALSE
      if (maxSR) {
        target <- max_sr_opt(R = R, constraints = constraints, moments = roi_moments,
                             lambda_hhi = lambda_hhi, conc_groups = conc_groups,
                             solver = solver, control = control)
        tmp_moments_mean <- roi_moments$mean
        roi_moments$mean <- rep(0, length(roi_moments$mean))
      }
      roi_result <- gmv_opt(R = R, constraints = constraints, moments = roi_moments,
                             lambda = lambda, target = target, lambda_hhi = lambda_hhi,
                             conc_groups = conc_groups, solver = solver, control = control)
      obj_vals <- roi_result$obj_vals
      if (maxSR) {
        port.mean <- as.numeric(sum(roi_result$weights * tmp_moments_mean))
        names(port.mean) <- "mean"
        obj_vals$mean <- port.mean
      }
      out <- list(weights = roi_result$weights, objective_measures = obj_vals,
                  opt_values = obj_vals, out = roi_result$out, call = call)
    }
  }

  # --- Linear (mean only) optimization (LP) ---
  if (length(names(roi_moments)) == 1 & "mean" %in% names(roi_moments)) {
    solver <- if (optimize_method == "ROI") "glpk" else optimize_method

    if (!is.null(constraints$max_pos) | !is.null(constraints$leverage)) {
      roi_result <- maxret_milp_opt(R = R, constraints = constraints, moments = roi_moments,
                                     target = target, solver = solver, control = control)
    } else {
      roi_result <- maxret_opt(R = R, constraints = constraints, moments = roi_moments,
                                target = target, solver = solver, control = control)
    }
    out <- list(weights = roi_result$weights, objective_measures = roi_result$obj_vals,
                opt_values = roi_result$obj_vals, out = roi_result$out, call = call)
  }

  # --- CVaR/ES/ETL optimization (LP) ---
  if (any(c("CVaR", "ES", "ETL") %in% names(roi_moments))) {
    solver <- if (optimize_method == "ROI") "glpk" else optimize_method

    ef <- if (!is.null(dots$ef)) dots$ef else FALSE
    maxSTARR <- if (!is.null(dots$maxSTARR)) dots$maxSTARR else TRUE
    meanetl <- if (ef) TRUE else FALSE
    tmpnames <- c("CVaR", "ES", "ETL")
    idx <- which(tmpnames %in% names(roi_moments))

    if (length(roi_moments) == 2 & all(roi_moments$mean != 0) & ef == FALSE & maxSTARR) {
      target <- mean_etl_opt(R = R, constraints = constraints, moments = roi_moments,
                              alpha = alpha, solver = solver, control = control)
      meanetl <- TRUE
    }

    if (!is.null(constraints$max_pos)) {
      roi_result <- etl_milp_opt(R = R, constraints = constraints, moments = roi_moments,
                                  target = target, alpha = alpha, solver = solver, control = control)
    } else {
      roi_result <- etl_opt(R = R, constraints = constraints, moments = roi_moments,
                             target = target, alpha = alpha, solver = solver, control = control)
    }
    obj_vals <- list()
    if (meanetl) obj_vals$mean <- sum(roi_result$weights * roi_moments$mean)
    obj_vals[[tmpnames[idx]]] <- roi_result$out
    out <- list(weights = roi_result$weights, objective_measures = obj_vals,
                opt_values = obj_vals, out = roi_result$out, call = call)
  }

  # Propagate solver diagnostics (Lagrangian/duals) from optFUN result
  sm <- NULL
  if (exists("roi_result", inherits = FALSE) && !is.null(roi_result$solver_message))
    sm <- roi_result$solver_message
  if (exists("qp_result", inherits = FALSE) && !is.null(qp_result$solver_message))
    sm <- qp_result$solver_message
  out$solver_diagnostics <- sm

  # Normalize method name for class assignment
  out$.optimize_method <- "ROI"
  out
}
