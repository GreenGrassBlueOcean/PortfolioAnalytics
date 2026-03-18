##### test_extractstats_coverage3.R #####
# Coverage: extractstats.R remaining uncovered paths

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]

# ===========================================================================
# 1. name.replace rare aliases
# ===========================================================================

test_that("name.replace handles rare objective aliases", {
  expect_equal(name.replace("median.median"), "median")
  expect_equal(name.replace("CVaR.MES"), "CVaR")
  expect_equal(name.replace("CVaR.ES"), "CVaR")
  expect_equal(name.replace("ETL.MES"), "ETL")
  expect_equal(name.replace("ETL.ETL"), "ETL")
  expect_equal(name.replace("VaR.MVaR"), "VaR")
  expect_equal(name.replace("maxDrawdown.maxDrawdown"), "maxDrawdown")
})

# ===========================================================================
# 2. Class validation error paths
# ===========================================================================

test_that("extractStats.optimize.portfolio.DEoptim errors on wrong class", {
  expect_error(extractStats.optimize.portfolio.DEoptim(list()), "class")
})

test_that("extractStats.optimize.portfolio.ROI errors on wrong class", {
  expect_error(extractStats.optimize.portfolio.ROI(list()), "class")
})

test_that("extractStats.optimize.portfolio.pso errors on wrong class", {
  expect_error(extractStats.optimize.portfolio.pso(list()), "class")
})

test_that("extractStats.optimize.portfolio.random errors on wrong class", {
  expect_error(extractStats.optimize.portfolio.random(list()), "class")
})

# ===========================================================================
# 3. PSO null PSOoutput error
# ===========================================================================

test_that("extractStats.optimize.portfolio.pso errors when PSOoutput is null", {
  skip_if_not_installed("pso")
  # Create a minimal PSO result with class but no PSOoutput
  obj <- list(weights = c(0.5, 0.5), PSOoutput = NULL)
  class(obj) <- "optimize.portfolio.pso"
  expect_error(extractStats(obj), "PSOoutput is null")
})
