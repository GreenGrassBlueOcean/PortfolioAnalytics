
require(testthat)
require(PortfolioAnalytics)

context("solver wrappers: pso and gensa")

# ---- Shared test data ----
data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
colnames(R3) <- c("A", "B", "C")

portf <- portfolio.spec(assets = colnames(R3))
portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ---- PSO solver ----

test_that("PSO solver produces valid output", {
  skip_if_not_installed("pso")

  set.seed(9134)
  opt <- optimize.portfolio(R3, portf, optimize_method = "pso", trace = TRUE)

  expect_s3_class(opt, "optimize.portfolio.pso")
  expect_equal(length(opt$weights), 3)
  expect_true(!is.null(opt$PSOoutput))
  expect_true(!is.null(opt$objective_measures))
})

test_that("PSO solver works with custom control params", {
  skip_if_not_installed("pso")

  set.seed(9134)
  opt <- optimize.portfolio(R3, portf, optimize_method = "pso", trace = FALSE,
                            maxit = 50, reltol = 1e-4)

  expect_equal(length(opt$weights), 3)
  # trace=FALSE means no PSOoutput
  expect_null(opt$PSOoutput)
})

# ---- GenSA solver ----

test_that("GenSA solver produces valid output", {
  skip_if_not_installed("GenSA")

  set.seed(2718)
  opt <- optimize.portfolio(R3, portf, optimize_method = "GenSA", trace = TRUE)

  expect_s3_class(opt, "optimize.portfolio.GenSA")
  expect_equal(length(opt$weights), 3)
  expect_true(!is.null(opt$GenSAoutput))
  expect_true(!is.null(opt$objective_measures))
})

test_that("GenSA solver works with custom control params", {
  skip_if_not_installed("GenSA")

  set.seed(2718)
  opt <- optimize.portfolio(R3, portf, optimize_method = "GenSA", trace = FALSE,
                            maxit = 50)

  expect_equal(length(opt$weights), 3)
  expect_null(opt$GenSAoutput)
})
