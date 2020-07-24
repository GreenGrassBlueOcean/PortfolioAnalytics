
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

context("test demo_efficient_frontier.R")

##### Source Demo Script #####

test_that("demo_efficient_frontier.R runs succesfully", {
  expect_error(source(system.file("demo/demo_efficient_frontier.R", package="PortfolioAnalytics")), NA)
})


context("mean-var efficient frontier")

test_that("meanvar.ef$frontier has 25 rows", 
          { expect_equal(nrow(meanvar.ef$frontier), 25) })

test_that("colnames(meanvar.ef$frontier) are consistent", 
          { expect_equal(colnames(meanvar.ef$frontier), c("mean", "StdDev", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")) })

test_that("first row of meanvar.ef$frontier is consistent", 
          { expect_equal( round(meanvar.ef$frontier[1,],digits = 4)
                        , c(mean = 0.0051, StdDev = 0.0117, out = 1e-04, w.CA = 0.15, w.CTAG = 0.15, w.DS = 0.15, w.EM = 0.15, w.EQM = 0.39)
                        , tolerance=1e-6) })

test_that("last row of meanvar.ef$frontier is consistent", 
          { expect_equal( round(meanvar.ef$frontier[25,], digits = 4)
                        , c(mean = 0.0058, StdDev = 0.0142, out = 2e-04, w.CA = 0.15, w.CTAG = 0.15, w.DS = 0.41, w.EM = 0.15, w.EQM = 0.15)
                        , tolerance=1e-6) })

context("mean-etl efficient frontier")

test_that("meanetl.ef$frontier has 25 rows", 
          { expect_equal(nrow(meanetl.ef$frontier), 25) })

test_that("colnames(meanetl.ef$frontier) are consistent", 
          { expect_equal(colnames(meanetl.ef$frontier), c("mean", "ES", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")) })

test_that("first row of meanetl.ef$frontier is consistent", 
          { expect_equal( round(meanetl.ef$frontier[1,], digits = 4)
                        , c(mean = 0.005, ES = 0.0244, out = 0.0244, w.CA = 0.15, w.CTAG = 0.3604, w.DS = 0.15, w.EM = 0.15, w.EQM = 0.1796)
                        , tolerance=1e-6
                        )
            })

test_that("last row of meanetl.ef$frontier is consistent", 
          { expect_equal( round(meanetl.ef$frontier[25,], digits = 4)
                        , c(mean = 0.0058, ES = 0.0305, out = 0.0305, w.CA = 0.15, w.CTAG = 0.15, w.DS = 0.41, w.EM = 0.15, w.EQM = 0.15)
                        , tolerance=1e-6
                        ) 
            })
