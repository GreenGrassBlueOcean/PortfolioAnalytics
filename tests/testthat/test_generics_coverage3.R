##### test_generics_coverage3.R #####
# Coverage: generics.R — print/summary methods for less common solver classes

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]

# ===========================================================================
# 1. print.optimize.portfolio.pso
# ===========================================================================

test_that("print.optimize.portfolio.pso works", {
  skip_if_not_installed("pso")
  portf <- portfolio.spec(assets = colnames(R3))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "ES")
  set.seed(4812)
  opt <- suppressWarnings(suppressMessages(
    optimize.portfolio(R3, portf, optimize_method = "pso", trace = TRUE)
  ))
  expect_output(print(opt))
})

# ===========================================================================
# 2. print.optimize.portfolio.GenSA
# ===========================================================================

test_that("print.optimize.portfolio.GenSA works", {
  skip_if_not_installed("GenSA")
  portf <- portfolio.spec(assets = colnames(R3))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "ES")
  set.seed(4813)
  opt <- suppressWarnings(suppressMessages(
    optimize.portfolio(R3, portf, optimize_method = "GenSA", trace = TRUE)
  ))
  expect_output(print(opt))
})

# ===========================================================================
# 3. print.optimize.portfolio.DEoptim
# ===========================================================================

test_that("print.optimize.portfolio.DEoptim works", {
  skip_if_not_installed("DEoptim")
  portf <- portfolio.spec(assets = colnames(R3))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "ES")
  set.seed(4814)
  opt <- suppressWarnings(suppressMessages(
    optimize.portfolio(R3, portf, optimize_method = "DEoptim", trace = TRUE,
                       DEoptim.control = list(itermax = 10, trace = FALSE))
  ))
  expect_output(print(opt))
})

# ===========================================================================
# 4. summary.optimize.portfolio
# ===========================================================================

test_that("summary.optimize.portfolio works with ROI", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  portf <- portfolio.spec(assets = colnames(R3))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  opt <- optimize.portfolio(R3, portf, optimize_method = "ROI")
  s <- summary(opt)
  expect_true(inherits(s, "summary.optimize.portfolio"))
})
