

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

# ---- Coverage: trace + custom dots (GenSA verbose branch) ----

test_that("GenSA trace=TRUE with custom control params sets verbose", {
  skip_if_not_installed("GenSA")

  set.seed(4821)
  opt <- optimize.portfolio(R3, portf, optimize_method = "GenSA",
                            trace = TRUE, maxit = 50)

  expect_s3_class(opt, "optimize.portfolio.GenSA")
  expect_true(!is.null(opt$GenSAoutput))
})

# ---- Coverage: warm_start (GenSA) ----

test_that("GenSA solver accepts warm_start", {
  skip_if_not_installed("GenSA")

  set.seed(6293)
  ws <- c(0.5, 0.3, 0.2)
  opt <- optimize.portfolio(R3, portf, optimize_method = "GenSA",
                            trace = FALSE, warm_start = ws, maxit = 50)

  expect_equal(length(opt$weights), 3)
})

# ---- Coverage: trace + custom dots (PSO reltol default + trace branches) ----

test_that("PSO trace=TRUE with custom maxit (no reltol) sets defaults", {
  skip_if_not_installed("pso")

  set.seed(3759)
  opt <- optimize.portfolio(R3, portf, optimize_method = "pso",
                            trace = TRUE, maxit = 50)

  expect_s3_class(opt, "optimize.portfolio.pso")
  expect_true(!is.null(opt$PSOoutput))
})

# ---- Coverage: warm_start (PSO) ----

test_that("PSO solver accepts warm_start", {
  skip_if_not_installed("pso")

  set.seed(8142)
  ws <- c(0.4, 0.35, 0.25)
  opt <- optimize.portfolio(R3, portf, optimize_method = "pso",
                            trace = FALSE, warm_start = ws, maxit = 50)

  expect_equal(length(opt$weights), 3)
})
