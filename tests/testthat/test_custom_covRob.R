##### test_custom_covRob.R #####
# Phase 5B: Coverage for custom.covRob.R (from 0% to ~80%)
# Targets: custom.covRob.MM, custom.covRob.Rocke, custom.covRob.Mcd,
#          custom.covRob.TSGS, MycovRobMcd, MycovRobTSGS


data(edhec)
# 5 assets, 120 rows — robust estimators need more obs than parameters
R <- edhec[1:120, 1:5]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
n <- ncol(R)

# ===========================================================================
# 1. MycovRobMcd — control settings (no external dependencies)
# ===========================================================================

context("custom.covRob: MycovRobMcd control function")

test_that("MycovRobMcd returns default parameters", {
  ctrl <- MycovRobMcd()
  
  expect_true(is.list(ctrl))
  expect_equal(ctrl$alpha, 0.5)
  expect_equal(ctrl$nsamp, 500)
  expect_equal(ctrl$nmini, 300L)
  expect_equal(ctrl$kmini, 5L)
  expect_equal(ctrl$scalefn, "hrv2012")
  expect_equal(ctrl$maxcsteps, 200L)
  # as.integer(NULL) produces integer(0), not NULL
  expect_equal(ctrl$seed, integer(0))
  expect_equal(ctrl$tolSolve, 1e-14)
  expect_equal(ctrl$wgtFUN, "01.original")
  expect_equal(ctrl$beta, 0.975)
  expect_true(ctrl$use.correction)
})

test_that("MycovRobMcd respects custom parameters", {
  ctrl <- MycovRobMcd(alpha = 0.75, nsamp = 1000, nmini = 200, kmini = 3,
                       scalefn = "v2014", maxcsteps = 100, seed = 42,
                       tolSolve = 1e-10, wgtFUN = "01.original",
                       beta = 0.95, use.correction = FALSE)
  
  expect_equal(ctrl$alpha, 0.75)
  expect_equal(ctrl$nsamp, 1000)
  expect_equal(ctrl$nmini, 200L)
  expect_equal(ctrl$kmini, 3L)
  expect_equal(ctrl$scalefn, "v2014")
  expect_equal(ctrl$maxcsteps, 100L)
  expect_equal(ctrl$seed, 42L)
  expect_equal(ctrl$tolSolve, 1e-10)
  expect_false(ctrl$use.correction)
  expect_equal(ctrl$beta, 0.95)
})

test_that("MycovRobMcd non-numeric beta falls back to 0.975", {
  ctrl <- MycovRobMcd(beta = "invalid")
  expect_equal(ctrl$beta, 0.975)
  
  ctrl2 <- MycovRobMcd()  # missing beta
  expect_equal(ctrl2$beta, 0.975)
})

# ===========================================================================
# 2. MycovRobTSGS — control settings (no external dependencies)
# ===========================================================================

context("custom.covRob: MycovRobTSGS control function")

test_that("MycovRobTSGS returns default parameters", {
  ctrl <- MycovRobTSGS()
  
  expect_true(is.list(ctrl))
  expect_equal(length(ctrl), 6)
  expect_equal(ctrl$filter, "UBF-DDC")
  expect_false(ctrl$partial.impute)
  expect_equal(ctrl$tol, 1e-4)
  expect_equal(ctrl$maxiter, 150L)
  expect_equal(ctrl$loss, "bisquare")
  # init is the 6th element, unnamed due to bare `init` in return() call
  expect_equal(ctrl[[6]], "emve")
})

test_that("MycovRobTSGS match.arg works for filter choices", {
  expect_equal(MycovRobTSGS(filter = "UBF")$filter, "UBF")
  expect_equal(MycovRobTSGS(filter = "DDC")$filter, "DDC")
  expect_equal(MycovRobTSGS(filter = "UF")$filter, "UF")
  expect_error(MycovRobTSGS(filter = "INVALID"))
})

test_that("MycovRobTSGS match.arg works for loss choices", {
  expect_equal(MycovRobTSGS(loss = "bisquare")$loss, "bisquare")
  expect_equal(MycovRobTSGS(loss = "rocke")$loss, "rocke")
  expect_error(MycovRobTSGS(loss = "INVALID"))
})

test_that("MycovRobTSGS match.arg works for init choices", {
  for (init_val in c("emve", "qc", "huber", "imputed", "emve_c")) {
    ctrl <- MycovRobTSGS(init = init_val)
    expect_equal(ctrl[[6]], init_val, info = paste("init:", init_val))
  }
  expect_error(MycovRobTSGS(init = "INVALID"))
})

test_that("MycovRobTSGS custom parameters", {
  ctrl <- MycovRobTSGS(filter = "DDC", partial.impute = TRUE,
                         tol = 1e-6, maxiter = 300, loss = "rocke",
                         init = "qc")
  
  expect_equal(ctrl$filter, "DDC")
  expect_true(ctrl$partial.impute)
  expect_equal(ctrl$tol, 1e-6)
  expect_equal(ctrl$maxiter, 300L)
  expect_equal(ctrl$loss, "rocke")
  expect_equal(ctrl[[6]], "qc")
})

# ===========================================================================
# 3. custom.covRob.MM — RobStatTM MM-estimator
# ===========================================================================

context("custom.covRob: custom.covRob.MM (RobStatTM)")

test_that("custom.covRob.MM returns mu and sigma", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.MM(R)
  
  expect_true(is.list(result))
  expect_equal(sort(names(result)), c("mu", "sigma"))
})

test_that("custom.covRob.MM sigma is n x n symmetric positive definite", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.MM(R)
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_true(isSymmetric(result$sigma, tol = 1e-10))
  evals <- eigen(result$sigma, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(evals > 0))
})

test_that("custom.covRob.MM mu has correct length", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.MM(R)
  expect_equal(length(result$mu), n)
  expect_true(is.numeric(result$mu))
})

test_that("custom.covRob.MM produces plausible robust estimates", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.MM(R)
  
  # Diagonal (variances) should be positive and in the same ballpark as sample
  sample_var <- diag(cov(R))
  robust_var <- diag(result$sigma)
  expect_true(all(robust_var > 0))
  # Robust variances should be within an order of magnitude
  expect_true(all(robust_var / sample_var > 0.1))
  expect_true(all(robust_var / sample_var < 10))
})

test_that("custom.covRob.MM respects custom tol and maxit", {
  skip_if_not_installed("RobStatTM")
  
  # Loose tolerance should still converge
  result <- custom.covRob.MM(R, tol = 1e-2, maxit = 100)
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

# ===========================================================================
# 4. custom.covRob.Rocke — RobStatTM Rocke S-estimator
# ===========================================================================

context("custom.covRob: custom.covRob.Rocke (RobStatTM)")

test_that("custom.covRob.Rocke returns mu and sigma", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.Rocke(R)
  
  expect_true(is.list(result))
  expect_equal(sort(names(result)), c("mu", "sigma"))
})

test_that("custom.covRob.Rocke sigma is n x n symmetric positive definite", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.Rocke(R)
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_true(isSymmetric(result$sigma, tol = 1e-10))
  evals <- eigen(result$sigma, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(evals > 0))
})

test_that("custom.covRob.Rocke mu has correct length", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.Rocke(R)
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.Rocke respects custom parameters", {
  skip_if_not_installed("RobStatTM")
  
  result <- custom.covRob.Rocke(R, tol = 1e-3, maxit = 100, 
                                 initial = "K", maxsteps = 3)
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

# ===========================================================================
# 5. custom.covRob.Mcd — robustbase MCD estimator
# ===========================================================================

context("custom.covRob: custom.covRob.Mcd (robustbase)")

test_that("custom.covRob.Mcd returns mu and sigma", {
  skip_if_not_installed("robustbase")
  
  result <- custom.covRob.Mcd(R)
  
  expect_true(is.list(result))
  expect_equal(sort(names(result)), c("mu", "sigma"))
})

test_that("custom.covRob.Mcd sigma is n x n symmetric positive definite", {
  skip_if_not_installed("robustbase")
  
  result <- custom.covRob.Mcd(R)
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_true(isSymmetric(result$sigma, tol = 1e-10))
  evals <- eigen(result$sigma, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(evals > 0))
})

test_that("custom.covRob.Mcd mu has correct length", {
  skip_if_not_installed("robustbase")
  
  result <- custom.covRob.Mcd(R)
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.Mcd with custom control via do.call", {
  skip_if_not_installed("robustbase")
  
  # Direct calls with variable args hit match.call bug (returns symbol).
  # do.call works because args are pre-evaluated — this matches how
  # optimize.portfolio calls momentFUN.
  ctrl <- MycovRobMcd(alpha = 0.75, nsamp = 200)
  result <- do.call(custom.covRob.Mcd, list(R = R, control = ctrl))
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.Mcd direct call with control variable works", {
  skip_if_not_installed("robustbase")
  
  # Previously broken: match.call without eval.parent returned unevaluated AST.
  # Fixed by wrapping in eval.parent() — direct calls with variables now work.
  ctrl <- MycovRobMcd(alpha = 0.75)
  result <- custom.covRob.Mcd(R, control = ctrl)
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.Mcd with alpha via do.call", {
  skip_if_not_installed("robustbase")
  
  result_half <- do.call(custom.covRob.Mcd, list(R = R, alpha = 0.5))
  result_high <- do.call(custom.covRob.Mcd, list(R = R, alpha = 0.75))
  
  # Both should produce valid results
  expect_equal(dim(result_half$sigma), c(n, n))
  expect_equal(dim(result_high$sigma), c(n, n))
})

test_that("custom.covRob.Mcd produces plausible robust estimates", {
  skip_if_not_installed("robustbase")
  
  result <- custom.covRob.Mcd(R)
  
  # Variances should be positive and in reasonable range
  robust_var <- diag(result$sigma)
  expect_true(all(robust_var > 0))
})

# ===========================================================================
# 6. custom.covRob.TSGS — GSE Two-Step Generalized S-Estimate
# ===========================================================================

context("custom.covRob: custom.covRob.TSGS (GSE)")

test_that("custom.covRob.TSGS returns mu and sigma", {
  skip_if_not_installed("GSE")
  
  result <- custom.covRob.TSGS(R)
  
  expect_true(is.list(result))
  expect_equal(sort(names(result)), c("mu", "sigma"))
})

test_that("custom.covRob.TSGS sigma is n x n", {
  skip_if_not_installed("GSE")
  
  result <- custom.covRob.TSGS(R)
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_true(is.numeric(result$sigma))
})

test_that("custom.covRob.TSGS mu has correct length", {
  skip_if_not_installed("GSE")
  
  result <- custom.covRob.TSGS(R)
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.TSGS with custom control via do.call", {
  skip_if_not_installed("GSE")
  
  # match.call bug fixed via eval.parent — direct calls now also work
  ctrl <- MycovRobTSGS(filter = "DDC", tol = 1e-3, maxiter = 50)
  result <- custom.covRob.TSGS(R, control = ctrl)
  
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.TSGS produces plausible robust estimates", {
  skip_if_not_installed("GSE")
  
  result <- custom.covRob.TSGS(R)
  
  robust_var <- diag(result$sigma)
  expect_true(all(robust_var > 0))
})

# ===========================================================================
# 7. Integration: robust estimators as momentFUN in optimization
# ===========================================================================

context("custom.covRob: integration with optimize.portfolio")

test_that("custom.covRob.Mcd works as momentFUN string", {
  skip_if_not_installed("robustbase")
  
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  
  # Use custom.covRob.Mcd as moment function
  result <- optimize.portfolio(R, portf, optimize_method = "ROI",
                                momentFUN = "custom.covRob.Mcd")
  
  expect_true(inherits(result, "optimize.portfolio"))
  w <- extractWeights(result)
  expect_equal(length(w), n)
  expect_true(all(w >= -1e-8))
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

###############################################################################
# 8. Direct calls with variable args (eval.parent fix coverage)
###############################################################################

test_that("custom.covRob.MM direct call with custom tol and maxit variables", {
  skip_if_not_installed("RobStatTM")
  
  my_tol <- 1e-3
  my_maxit <- 100
  result <- custom.covRob.MM(R, tol = my_tol, maxit = my_maxit)
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.Rocke direct call with custom params as variables", {
  skip_if_not_installed("RobStatTM")
  
  my_tol <- 1e-3
  my_maxit <- 100
  my_initial <- "K"
  my_maxsteps <- 3
  my_propmin <- 2
  my_qs <- 25
  result <- custom.covRob.Rocke(R, tol = my_tol, maxit = my_maxit,
                                 initial = my_initial, maxsteps = my_maxsteps,
                                 propmin = my_propmin, qs = my_qs)
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.Mcd direct call with individual params as variables", {
  skip_if_not_installed("robustbase")
  
  my_alpha <- 0.75
  my_nsamp <- 250
  result <- custom.covRob.Mcd(R, alpha = my_alpha, nsamp = my_nsamp)
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.TSGS direct call with params as variables", {
  skip_if_not_installed("GSE")
  
  my_filter <- "DDC"
  my_tol <- 1e-3
  my_maxiter <- 50
  result <- custom.covRob.TSGS(R, filter = my_filter, tol = my_tol,
                                maxiter = my_maxiter)
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

###############################################################################
# 9. Individual parameter coverage for Mcd (lines 93-101)
###############################################################################

test_that("custom.covRob.Mcd individual params: nmini through use.correction", {
  skip_if_not_installed("robustbase")

  result <- custom.covRob.Mcd(R,
    nmini = 300, kmini = 5, scalefn = "hrv2012", maxcsteps = 200,
    seed = NULL, tolSolve = 1e-14, wgtFUN = "01.original",
    use.correction = TRUE)
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})

test_that("custom.covRob.Mcd individual param initHsets (line 97)", {
  skip_if_not_installed("robustbase")

  result <- custom.covRob.Mcd(R, initHsets = NULL)
  expect_equal(dim(result$sigma), c(n, n))
})

###############################################################################
# 10. Individual parameter coverage for TSGS (lines 192, 195-196)
###############################################################################

test_that("custom.covRob.TSGS individual params: partial.impute, loss, init", {
  skip_if_not_installed("GSE")

  result <- custom.covRob.TSGS(R,
    partial.impute = FALSE, loss = "bisquare", init = "emve")
  expect_equal(dim(result$sigma), c(n, n))
  expect_equal(length(result$mu), n)
})
