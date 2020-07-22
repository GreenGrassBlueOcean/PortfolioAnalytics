##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####

context("test meucci_ffv.R")

test_that("meucci_ffv.R runs succesfully", {
  expect_error(source(system.file("demo/meucci_ffv.R", package="PortfolioAnalytics")), NA)
})

test_that("Optimum Meucci weights are constant",{
  expect_equal( extractWeights(opt.meucci)
               , c(`Convertible Arbitrage` = 0.06, `CTA Global` = 0.32, `Distressed Securities` = 0.066, 
                   `Emerging Markets` = 0.056, `Equity Market Neutral` = 0.494)
               )
  
})


test_that("Optimum Meucci ObjectiveMeasures are constant", {
  expect_equal( extractObjectiveMeasures(opt.meucci)
              , list(StdDev = structure(c(StdDev = 0.0132601195647954), .Dim = c(1L, 1L)), mean = c(mean = 0.00779290303400902))
              , tolerance = 1e-4
              )
  
})

