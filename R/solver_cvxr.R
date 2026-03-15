###############################################################################
# CVXR Solver for PortfolioAnalytics
#
# Extracted from braverock/PortfolioAnalytics optimize.portfolio.R
# and wrapped in the solver registry pattern.
###############################################################################

#' @keywords internal
solve_cvxr <- function(R, portfolio, constraints, moments, penalty,
                       N, call, trace, search_size, rp,
                       message = FALSE, ...) {
  stopifnot("package:CVXR" %in% search() || requireNamespace("CVXR", quietly = TRUE))

  dots <- list(...)
  out <- list()
  optimize_method <- dots$optimize_method
  if (is.null(optimize_method)) optimize_method <- "CVXR"

  cvxr_solvers <- c("CBC", "GLPK", "GLPK_MI", "OSQP", "CPLEX", "SCS", "ECOS", "GUROBI", "MOSEK")
  cvxr_default <- !(optimize_method %in% cvxr_solvers)

  # --- CVXR Variables ---
  X <- as.matrix(R)
  Tobs <- nrow(X)
  wts <- CVXR::Variable(N)
  z <- CVXR::Variable(Tobs)
  zeta <- CVXR::Variable(1)
  t_var <- CVXR::Variable(1)

  # --- Parse objectives ---
  target <- -Inf
  reward <- FALSE
  risk <- FALSE
  risk_ES <- FALSE
  risk_CSM <- FALSE
  risk_HHI <- FALSE
  risk_EQS <- FALSE
  maxSR <- FALSE
  maxSTARR <- FALSE
  ESratio <- FALSE
  CSMratio <- FALSE
  EQSratio <- FALSE
  alpha <- 0.05
  lambda <- 1

  valid_objnames <- c("mean", "var", "sd", "StdDev", "ES", "CVaR", "ETL", "CSM", "HHI", "hhi", "EQS")
  for (objective in portfolio$objectives) {
    if (objective$enabled) {
      if (!(objective$name %in% valid_objnames)) {
        stop("CVXR only solves mean, var/sd/StdDev and ETL/ES/CVaR/CSM/EQS type business objectives, ",
             "choose a different optimize_method.")
      }
      alpha <- ifelse(!is.null(objective$arguments[["p"]]), objective$arguments[["p"]], alpha)
      lambda <- ifelse(!is.null(objective$risk_aversion), objective$risk_aversion, lambda)
      target <- ifelse(!is.null(objective$target), objective$target, target)
      reward <- ifelse(objective$name == "mean", TRUE, reward)
      risk <- ifelse(objective$name %in% valid_objnames[2:4], TRUE, risk)
      risk_ES <- ifelse(objective$name %in% valid_objnames[5:7], TRUE, risk_ES)
      risk_CSM <- ifelse(objective$name %in% valid_objnames[8:8], TRUE, risk_CSM)
      risk_EQS <- ifelse(objective$name %in% valid_objnames[11:11], TRUE, risk_EQS)
      if (objective$name %in% valid_objnames[9:11]) {
        risk_HHI <- TRUE
        lambda_hhi <- objective$conc_aversion
      }
      arguments <- objective$arguments
    }
  }
  if (alpha > 0.5) alpha <- (1 - alpha)

  mean_value <- moments$mu
  sigma_value <- moments$sigma

  # Efficient frontier support
  ef <- if (methods::hasArg(ef)) match.call(expand.dots = TRUE)$ef else FALSE
  if (ef) {
    reward <- FALSE
    mean_idx <- which(vapply(portfolio$objectives, function(x) x$name, character(1)) == "mean")
    return_target <- portfolio$objectives[[mean_idx]]$target
  } else {
    return_target <- NULL
  }

  # --- Build CVXR objective ---
  if (reward & !risk & !risk_ES & !risk_CSM & !risk_HHI & !risk_EQS) {
    obj <- -t(mean_value) %*% wts
    constraints_cvxr <- list()
    tmpname <- "mean"
  } else if (!reward & risk & !risk_ES & !risk_CSM & !risk_HHI & !risk_EQS) {
    obj <- CVXR::quad_form(wts, sigma_value)
    constraints_cvxr <- list()
    tmpname <- "StdDev"
  } else if (!reward & risk & !risk_ES & !risk_CSM & risk_HHI & !risk_EQS) {
    obj <- CVXR::quad_form(wts, sigma_value) + lambda_hhi * CVXR::cvxr_norm(wts, 2)^2
    constraints_cvxr <- list()
    tmpname <- "HHI"
  } else if (reward & risk & !risk_ES & !risk_CSM & !risk_HHI & !risk_EQS) {
    if (methods::hasArg(maxSR)) maxSR <- match.call(expand.dots = TRUE)$maxSR
    if (!maxSR) {
      obj <- CVXR::quad_form(wts, sigma_value) - (t(mean_value) %*% wts) / lambda
      constraints_cvxr <- list()
      tmpname <- "optimal value"
    } else {
      obj <- CVXR::quad_form(wts, sigma_value)
      constraints_cvxr <- list(t(mean_value) %*% wts == 1, sum(wts) >= 0)
      tmpname <- "Sharpe Ratio"
    }
  } else if (risk_ES & !risk_CSM & !risk_EQS) {
    if (reward) {
      if (methods::hasArg(maxSTARR)) maxSTARR <- match.call(expand.dots = TRUE)$maxSTARR else maxSTARR <- TRUE
      if (methods::hasArg(ESratio)) maxSTARR <- match.call(expand.dots = TRUE)$ESratio else maxSTARR <- maxSTARR
    }
    if (maxSTARR) {
      obj <- zeta + (1 / (Tobs * alpha)) * sum(z)
      constraints_cvxr <- list(z >= 0,
                               z >= -X %*% wts - zeta,
                               t(mean_value) %*% wts == 1,
                               sum(wts) >= 0)
      tmpname <- "ES ratio"
    } else {
      obj <- zeta + (1 / (Tobs * alpha)) * sum(z)
      constraints_cvxr <- list(z >= 0, z >= -X %*% wts - zeta)
      tmpname <- "ES"
    }
  } else if (!risk_ES & risk_CSM & !risk_EQS) {
    if (reward) {
      if (methods::hasArg(CSMratio)) CSMratio <- match.call(expand.dots = TRUE)$CSMratio else CSMratio <- TRUE
    }
    if (CSMratio) {
      obj <- zeta + (1 / (alpha * sqrt(Tobs))) * t_var
      constraints_cvxr <- list(z >= 0,
                               z >= -X %*% wts - zeta,
                               t(mean_value) %*% wts == 1,
                               sum(wts) >= 0,
                               t_var >= CVXR::p_norm(z, p = 2))
      tmpname <- "CSM ratio"
    } else {
      obj <- zeta + (1 / (alpha * sqrt(Tobs))) * t_var
      constraints_cvxr <- list(z >= 0, z >= -X %*% wts - zeta,
                               t_var >= CVXR::p_norm(z, p = 2))
      tmpname <- "CSM"
    }
  } else if (!risk_ES & !risk_CSM & risk_EQS) {
    if (reward) {
      if (methods::hasArg(EQSratio)) EQSratio <- match.call(expand.dots = TRUE)$EQSratio else EQSratio <- TRUE
    }
    if (EQSratio) {
      obj <- zeta + (1 / (alpha * Tobs)) * sum(CVXR::pos(CVXR::square(CVXR::pos(X %*% wts)) - zeta))
      constraints_cvxr <- list(t(mean_value) %*% wts == 1, sum(wts) >= 0)
      tmpname <- "EQS ratio"
    } else {
      obj <- zeta + (1 / (alpha * Tobs)) * sum(CVXR::pos(CVXR::square(CVXR::pos(X %*% wts)) - zeta))
      constraints_cvxr <- list()
      tmpname <- "EQS"
    }
  } else {
    stop("Wrong multiple objectives. CVXR only solves mean, var, or simple ES/CSM type business objectives, ",
         "please reorganize the objectives.")
  }

  # Weight scale for maximizing return per unit risk
  if (!maxSR & !maxSTARR & !CSMratio) weight_scale <- 1 else weight_scale <- sum(wts)

  # --- Build CVXR constraints ---
  # Weight sum constraint
  if (!maxSR & !maxSTARR & !CSMratio) {
    if (!is.null(constraints$max_sum) & !is.infinite(constraints$max_sum) &
        constraints$max_sum - constraints$min_sum <= 0.001) {
      constraints_cvxr <- append(constraints_cvxr, sum(wts) == constraints$max_sum)
    } else {
      if (!is.null(constraints$max_sum)) {
        max_sum <- ifelse(is.infinite(constraints$max_sum), 9999.0, constraints$max_sum)
        constraints_cvxr <- append(constraints_cvxr, sum(wts) <= max_sum)
      }
      if (!is.null(constraints$min_sum)) {
        min_sum <- ifelse(is.infinite(constraints$min_sum), -9999.0, constraints$min_sum)
        constraints_cvxr <- append(constraints_cvxr, sum(wts) >= min_sum)
      }
    }
  }

  # Box constraint
  upper <- constraints$max
  lower <- constraints$min
  if (!all(is.infinite(upper))) {
    upper[which(is.infinite(upper))] <- 9999.0
    constraints_cvxr <- append(constraints_cvxr, wts <= upper * weight_scale)
  }
  if (!all(is.infinite(lower))) {
    lower[which(is.infinite(lower))] <- -9999.0
    constraints_cvxr <- append(constraints_cvxr, wts >= lower * weight_scale)
  }

  # Group constraint
  i <- 1
  for (g in constraints$groups) {
    constraints_cvxr <- append(constraints_cvxr, sum(wts[g]) >= constraints$cLO[i] * weight_scale)
    constraints_cvxr <- append(constraints_cvxr, sum(wts[g]) <= constraints$cUP[i] * weight_scale)
    i <- i + 1
  }

  # Target return constraint
  if (!is.null(constraints$return_target)) {
    constraints_cvxr <- append(constraints_cvxr, t(mean_value) %*% wts >= constraints$return_target * weight_scale)
  }
  if (!is.null(return_target)) {
    constraints_cvxr <- append(constraints_cvxr, t(mean_value) %*% wts >= return_target)
  }

  # Factor exposure constraint
  if (!is.null(constraints$B)) {
    constraints_cvxr <- append(constraints_cvxr, t(constraints$B) %*% wts <= constraints$upper)
    constraints_cvxr <- append(constraints_cvxr, t(constraints$B) %*% wts >= constraints$lower)
  }

  # Turnover constraint
  if (!is.null(constraints$turnover_target)) {
    weight_initial <- if (is.null(constraints$weight_initial)) rep(1 / N, N) else constraints$weight_initial
    if (tmpname == "StdDev") {
      if (is.null(constraints$turnover_penalty)) {
        stopifnot("package:Matrix" %in% search() || requireNamespace("Matrix", quietly = TRUE))
        sigma_value_penalty <- Matrix::nearPD(sigma_value)$mat
      } else {
        sigma_value_penalty <- sigma_value + diag(constraints$turnover_penalty, N)
      }
      obj <- CVXR::quad_form(wts - weight_initial, sigma_value_penalty) +
        2 * t(wts - weight_initial) %*% sigma_value %*% weight_initial +
        t(weight_initial) %*% sigma_value %*% weight_initial
    }
    constraints_cvxr <- append(constraints_cvxr, sum(abs(wts - weight_initial)) <= constraints$turnover_target)
  }

  # --- Solve ---
  prob_cvxr <- CVXR::Problem(CVXR::Minimize(obj), constraints = constraints_cvxr)

  # Filter ... to only pass CVXR-compatible args (not our internal ones like
  # warm_start, optimize_method, etc.)
  cvxr_solve_args <- intersect(
    names(dots),
    c("verbose", "feastol", "reltol", "abstol", "num_iter")
  )
  cvxr_dots <- dots[cvxr_solve_args]

  .cvxr_solve <- function(prob, solver) {
    do.call(CVXR::psolve, c(list(object = prob, solver = solver), cvxr_dots))
  }

  if (cvxr_default) {
    if ((risk || maxSR) && !risk_HHI) {
      result_cvxr <- .cvxr_solve(prob_cvxr, "OSQP")
    } else {
      result_cvxr <- .cvxr_solve(prob_cvxr, "SCS")
    }
  } else {
    result_cvxr <- .cvxr_solve(prob_cvxr, optimize_method)
  }

  # --- Check solver status ---
  if (!isTRUE(result_cvxr$status %in% c("optimal", "optimal_inaccurate"))) {
    return(optimization_failure(
      message = paste("CVXR solver returned status:", result_cvxr$status),
      solver  = result_cvxr$solver %||% "CVXR",
      call    = call
    ))
  }

  # --- Extract results ---
  cvxr_wts <- result_cvxr$getValue(wts)
  if (maxSR | maxSTARR | CSMratio) cvxr_wts <- cvxr_wts / sum(cvxr_wts)
  cvxr_wts <- as.vector(cvxr_wts)

  # Normalize weights to satisfy sum constraints
  if (!is.null(constraints$min_sum) | !is.null(constraints$max_sum)) {
    min_sum <- ifelse(is.null(constraints$min_sum), 0.99, constraints$min_sum)
    max_sum <- ifelse(is.null(constraints$max_sum), 1.01, constraints$max_sum)
    if (!(sum(cvxr_wts) >= min_sum & sum(cvxr_wts) <= max_sum)) {
      cvxr_wts <- (cvxr_wts / sum(cvxr_wts)) * min_sum
    }
  }
  # Clamp weights to box constraints to correct for solver tolerance
  if (!is.null(constraints$min) && !is.null(constraints$max)) {
    cvxr_wts <- pmax(cvxr_wts, constraints$min)
    cvxr_wts <- pmin(cvxr_wts, constraints$max)
  }

  names(cvxr_wts) <- colnames(R)

  # Build objective measures
  obj_cvxr <- list()
  if (reward & !risk & !risk_ES & !risk_CSM & !risk_HHI) {
    obj_cvxr[[tmpname]] <- -result_cvxr$value
  } else if (!reward & risk & !risk_ES & !risk_CSM & !risk_HHI) {
    obj_cvxr[[tmpname]] <- sqrt(result_cvxr$value)
  } else if (!reward & risk & !risk_ES & !risk_CSM & risk_HHI) {
    obj_cvxr[["StdDev"]] <- sqrt(t(cvxr_wts) %*% sigma_value %*% cvxr_wts)
    obj_cvxr[[tmpname]] <- (result_cvxr$value - t(cvxr_wts) %*% sigma_value %*% cvxr_wts) / lambda_hhi
  } else if (!maxSR & !maxSTARR & !CSMratio) {
    obj_cvxr[[tmpname]] <- result_cvxr$value
    if (reward & risk) {
      obj_cvxr[["mean"]] <- cvxr_wts %*% mean_value
      obj_cvxr[["StdDev"]] <- sqrt(t(cvxr_wts) %*% sigma_value %*% cvxr_wts)
    }
  } else {
    obj_cvxr[["mean"]] <- cvxr_wts %*% mean_value
    if (maxSR) {
      obj_cvxr[["StdDev"]] <- sqrt(t(cvxr_wts) %*% sigma_value %*% cvxr_wts)
      obj_cvxr[[tmpname]] <- obj_cvxr[["mean"]] / obj_cvxr[["StdDev"]]
    } else if (maxSTARR) {
      obj_cvxr[["ES"]] <- result_cvxr$value / sum(result_cvxr$getValue(wts))
      obj_cvxr[[tmpname]] <- obj_cvxr[["mean"]] / obj_cvxr[["ES"]]
    } else if (CSMratio) {
      obj_cvxr[["CSM"]] <- result_cvxr$value / sum(result_cvxr$getValue(wts))
      obj_cvxr[[tmpname]] <- obj_cvxr[["mean"]] / obj_cvxr[["CSM"]]
    }
  }
  if (ef) obj_cvxr[["mean"]] <- cvxr_wts %*% mean_value

  out <- list(
    weights = cvxr_wts,
    objective_measures = obj_cvxr,
    opt_values = obj_cvxr,
    out = obj_cvxr[[tmpname]],
    call = call,
    solver = result_cvxr$solver,
    moment_values = list(mu = moments$mu, sigma = moments$sigma),
    .optimize_method = "CVXR"
  )

  out
}
