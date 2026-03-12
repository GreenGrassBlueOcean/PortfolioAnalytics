
require(testthat)
require(PortfolioAnalytics)

context("utility functions: equal.weight, inverse.volatility.weight, black_litterman, trailingFUN, constraint_ROI, mult.layer.portfolio")

# ---- Shared test data ----
data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
colnames(R3) <- c("A", "B", "C")

# ---- equal.weight ----

test_that("equal.weight returns correct structure and weights", {
  portf <- portfolio.spec(assets = colnames(R3))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  ew <- equal.weight(R3, portf)

  expect_s3_class(ew, "optimize.portfolio")
  expect_true("optimize.portfolio.eqwt" %in% class(ew))
  expect_equal(length(ew$weights), 3)
  expect_equal(as.numeric(sum(ew$weights)), 1, tolerance = 0.02)
  # equal.weight uses max_sum / nassets, so with max_sum=1.01 weights = 1.01/3
  expected_wt <- 1.01 / 3
  expect_true(all(abs(ew$weights - expected_wt) < 1e-10))
  expect_true(!is.null(ew$objective_measures))
})

test_that("equal.weight errors on non-portfolio object", {
  expect_error(equal.weight(R3, list()), "portfolio")
})

test_that("equal.weight warns when R has more columns than assets", {
  portf2 <- portfolio.spec(assets = colnames(R3)[1:2])
  portf2 <- add.constraint(portf2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf2 <- add.objective(portf2, type = "risk", name = "StdDev")
  expect_warning(equal.weight(R3, portf2), "subsetting")
})

test_that("equal.weight errors when R has fewer columns than assets", {
  portf4 <- portfolio.spec(assets = 4)
  portf4 <- add.constraint(portf4, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf4 <- add.objective(portf4, type = "risk", name = "StdDev")
  expect_error(equal.weight(R3, portf4), "greater than")
})

# ---- inverse.volatility.weight ----

test_that("inverse.volatility.weight returns correct structure", {
  portf <- portfolio.spec(assets = colnames(R3))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  iv <- inverse.volatility.weight(R3, portf)

  expect_s3_class(iv, "optimize.portfolio")
  expect_true("optimize.portfolio.invol" %in% class(iv))
  expect_equal(length(iv$weights), 3)
  expect_equal(sum(iv$weights), 1, tolerance = 0.02)
  # Lower vol assets should have higher weight
  vols <- apply(R3, 2, sd)
  expect_true(cor(iv$weights, 1/vols) > 0.99)
})

test_that("inverse.volatility.weight errors on non-portfolio object", {
  expect_error(inverse.volatility.weight(R3, "not a portfolio"), "portfolio")
})

test_that("inverse.volatility.weight warns when R has more columns than assets", {
  portf2 <- portfolio.spec(assets = colnames(R3)[1:2])
  portf2 <- add.constraint(portf2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf2 <- add.objective(portf2, type = "risk", name = "StdDev")
  expect_warning(inverse.volatility.weight(R3, portf2), "subsetting")
})

# ---- black_litterman ----

test_that("BlackLittermanFormula computes posterior moments", {
  BLF <- PortfolioAnalytics:::BlackLittermanFormula
  Mu <- c(0.05, 0.08, 0.06)
  Sigma <- matrix(c(0.04, 0.01, 0.02,
                     0.01, 0.09, 0.03,
                     0.02, 0.03, 0.06), 3, 3)
  # View: asset 1 outperforms asset 2 by 3%
  P <- matrix(c(1, -1, 0), nrow = 1)
  v <- 0.03
  Omega <- matrix(0.01, 1, 1)

  bl <- BLF(Mu, Sigma, P, v, Omega)

  expect_true(is.list(bl))
  expect_true(!is.null(bl$BLMu))
  expect_true(!is.null(bl$BLSigma))
  expect_equal(length(bl$BLMu), 3)
  expect_equal(dim(bl$BLSigma), c(3, 3))
})

test_that("black.litterman wrapper works with sample estimates", {
  P <- matrix(c(1, -1, 0), nrow = 1)

  bl <- black.litterman(R3, P)

  expect_true(is.list(bl))
  expect_equal(length(bl$BLMu), 3)
  expect_equal(dim(bl$BLSigma), c(3, 3))
})

test_that("black.litterman works with user-supplied Mu and Sigma", {
  Mu <- colMeans(R3)
  Sigma <- cov(as.matrix(R3))
  P <- matrix(c(1, -1, 0), nrow = 1)
  Views <- 0.02

  bl <- black.litterman(R3, P, Mu = Mu, Sigma = Sigma, Views = Views)

  expect_equal(length(bl$BLMu), 3)
  expect_equal(dim(bl$BLSigma), c(3, 3))
})

test_that("black.litterman errors on mismatched dimensions", {
  P <- matrix(c(1, -1, 0), nrow = 1)
  expect_error(black.litterman(R3, P, Mu = c(0.01, 0.02)), "length of Mu")
  expect_error(black.litterman(R3, P, Sigma = matrix(0, 2, 2)), "dimensions of Sigma")
})

# ---- trailingFUN ----

test_that("trailingFUN works with a simple vector", {
  x <- 1:100
  result <- trailingFUN(R = x, weights = NULL, n = 12, FUN = "mean", FUNargs = list())
  expect_true(is.numeric(result))
})

test_that("trailingFUN works with a matrix", {
  mat <- as.matrix(R3)
  result <- trailingFUN(R = mat, weights = rep(1/3, 3), n = 12,
                        FUN = "mean", FUNargs = list())
  expect_true(!is.null(result))
})

test_that("trailingFUN errors when FUN is NULL", {
  expect_error(trailingFUN(R = 1:10, weights = NULL, n = 5, FUN = NULL),
               "you must supply a function")
})

# ---- constraint_ROI ----

test_that("constraint_ROI creates correct object with named assets", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  requireNamespace("ROI", quietly = TRUE)

  # Create a minimal OP object
  n <- 3
  op <- ROI::OP(
    objective = ROI::L_objective(c(1, 1, 1)),
    constraints = ROI::L_constraint(
      L = matrix(1, nrow = 1, ncol = n),
      dir = "==",
      rhs = 1
    ),
    bounds = ROI::V_bound(li = 1:n, ui = 1:n,
                          lb = rep(0, n), ub = rep(1, n))
  )

  expect_message(
    croi <- constraint_ROI(assets = c("A", "B", "C"), op.problem = op, solver = "glpk"),
    "glpk"
  )
  expect_s3_class(croi, "constraint_ROI")
  expect_s3_class(croi, "constraint")
  expect_equal(length(croi$assets), 3)
  expect_equal(croi$solver, "glpk")
})

test_that("constraint_ROI creates correct object with numeric assets", {
  skip_if_not_installed("ROI")
  requireNamespace("ROI", quietly = TRUE)

  n <- 4
  op <- ROI::OP(
    objective = ROI::L_objective(rep(1, n)),
    constraints = ROI::L_constraint(
      L = matrix(1, nrow = 1, ncol = n),
      dir = "==",
      rhs = 1
    )
  )

  expect_message(
    croi <- constraint_ROI(assets = n, op.problem = op),
    "equal weighted"
  )
  expect_equal(length(croi$assets), 4)
})

test_that("constraint_ROI errors without assets or OP", {
  skip_if_not_installed("ROI")
  expect_error(constraint_ROI(assets = NULL, op.problem = list()), "Need to pass in")
  expect_error(constraint_ROI(assets = 3, op.problem = list()), "Need to pass in")
  expect_error(constraint_ROI(assets = NULL, op.problem = structure(list(), class = "OP")),
               "You must specify the assets")
})

# ---- mult.layer.portfolio ----

test_that("mult.portfolio.spec creates correct structure", {
  portf <- portfolio.spec(assets = colnames(R3))
  mp <- mult.portfolio.spec(portf)

  expect_s3_class(mp, "mult.portfolio.spec")
  expect_true(!is.null(mp$top.portfolio))
  expect_equal(length(mp$sub.portfolios), 0)
})

test_that("add.sub.portfolio appends sub portfolios", {
  top <- portfolio.spec(assets = colnames(R3))
  mp <- mult.portfolio.spec(top)

  sub1 <- portfolio.spec(assets = c("A", "B"))
  sub1 <- add.constraint(sub1, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  sub1 <- add.objective(sub1, type = "risk", name = "StdDev")

  sub2 <- portfolio.spec(assets = c("B", "C"))
  sub2 <- add.constraint(sub2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  sub2 <- add.objective(sub2, type = "risk", name = "StdDev")

  mp <- add.sub.portfolio(mp, sub1, optimize_method = "random")
  expect_equal(length(mp$sub.portfolios), 1)
  expect_s3_class(mp$sub.portfolios[[1]], "sub.portfolio")

  mp <- add.sub.portfolio(mp, sub2, optimize_method = "random")
  expect_equal(length(mp$sub.portfolios), 2)
})

test_that("add.sub.portfolio can overwrite by index", {
  top <- portfolio.spec(assets = colnames(R3))
  mp <- mult.portfolio.spec(top)

  sub1 <- portfolio.spec(assets = c("A", "B"))
  sub1 <- add.constraint(sub1, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  sub1 <- add.objective(sub1, type = "risk", name = "StdDev")

  mp <- add.sub.portfolio(mp, sub1, optimize_method = "random")
  mp <- add.sub.portfolio(mp, sub1, optimize_method = "ROI", indexnum = 1)

  expect_equal(length(mp$sub.portfolios), 1)
  expect_equal(mp$sub.portfolios[[1]]$optimize_method, "ROI")
})

test_that("add.sub.portfolio errors on non mult.portfolio.spec", {
  sub1 <- portfolio.spec(assets = c("A", "B"))
  expect_error(add.sub.portfolio(list(), sub1), "mult.portfolio.spec")
})
