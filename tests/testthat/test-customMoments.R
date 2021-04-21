test_that("Custom Moment Functions ", {

  library(PortfolioAnalytics)
  library(DEoptim)

  data(edhec)
  # Use the first 4 columns in edhec for a returns object>
  R <- edhec[, 1:4]
  colnames(R) <- c("CA", "CTAG", "DS", "EM")
  head(R,5)
  # Get a character vector of the fund names
  funds <- colnames(R)

  # Construct initial portfolio with basic constraints.
  init.portf <- portfolio.spec(assets=funds)
  init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
  init.portf <- add.constraint(portfolio=init.portf, type="long_only")

  # Portfolio with standard deviation as an objective
  SD.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
  # Portfolio with expected shortfall as an objective
  ES.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")

  sd.moments <- set.portfolio.moments(R, SD.portf)
  names(sd.moments)

  expect_equal(names(sd.moments), c("mu", "sigma"))


  es.moments <- set.portfolio.moments(R, ES.portf)
  names(es.moments)

  expect_equal(names(es.moments), c( "mu", "sigma", "m3", "m4" ))


  sigma.robust <- function(R){
    require(MASS)
    out <- list()
    set.seed(1234)
    out$sigma <- cov.rob(R, method="mcd")$cov
    return(out)
  }

  opt.sd <- optimize.portfolio(R, SD.portf
                               , optimize_method="ROI"
                               , momentFUN="sigma.robust"
  )

  opt.sd

  weights <- extractWeights(opt.sd)
  sigma <- sigma.robust(R)$sigma

  sqrt(t(weights) %*% sigma %*% weights)

  extractObjectiveMeasures(opt.sd)$StdDev

  expect_equal(as.numeric(sqrt(t(weights) %*% sigma %*% weights))
               ,as.numeric(extractObjectiveMeasures(opt.sd)$StdDev))


})


test_that("Custom Moment Functions work", {
  
  library(PortfolioAnalytics)
  library(DEoptim)
  
  data(edhec)
  # Use the first 4 columns in edhec for a returns object>
  R <- edhec[, 1:4]
  colnames(R) <- c("CA", "CTAG", "DS", "EM")
  head(R,5)
  # Get a character vector of the fund names
  funds <- colnames(R)
  
  sigma.robust <- function(R){
    require(MASS)
    out <- list()
    set.seed(1234)
    out$sigma <- cov.rob(R, method="mcd")$cov
    return(out)
  }
  
  # pasd <- function(R, weights, sigma, N=36){
  #   R <- tail(R, N)
  #   tmp.sd <- sqrt(as.numeric(t(weights) %*% sigma %*% weights))
  #   return(sqrt(12) * tmp.sd)
  # }
  
  
  # Construct initial portfolio with basic constraints.
  pasd.portf <- portfolio.spec(assets=funds)
  pasd.portf <- add.constraint(portfolio=pasd.portf, type="full_investment")
  pasd.portf <- add.constraint(portfolio=pasd.portf, type="long_only")
  
  # Portfolio with pasd as an objective> # Note how we can specify N as an argument
  pasd.portf <- add.objective(portfolio=pasd.portf, type="risk", name="pasd",arguments=list(N=48))
  pasd.portf <- add.objective(portfolio = pasd.portf, type="risk", name="StdDev", multiplier = 0)
  
  opt.pasd <- optimize.portfolio( R
                                  , pasd.portf
                                  , optimize_method = "DEoptim"
                                  , search_size=5000
                                  , trace=TRUE
                                  , traceDE=1
                                  , momentFUN="sigma.robust"
                                  )
  
  opt.pasd
  
  weights <- PortfolioAnalytics::extractWeights(opt.pasd)
  sigma <- sigma.robust(R)$sigma
  
  sqrt(t(weights) %*% sigma %*% weights)
  
  extractObjectiveMeasures(opt.pasd)$StdDev
  
  expect_equal(as.numeric(sqrt(t(weights) %*% sigma %*% weights))
               ,as.numeric(extractObjectiveMeasures(opt.pasd)$StdDev))
  
  expect_equal(as.numeric(pasd(R, weights, sigma, N=48))
               ,as.numeric(extractObjectiveMeasures(opt.pasd)$pasd)
  )
  
  
})


test_that("Custom Moment Functions work as expected", {

  library(PortfolioAnalytics)
  library(DEoptim)

  data(edhec)
  # Use the first 4 columns in edhec for a returns object>
  R <- edhec[, 1:4]
  colnames(R) <- c("CA", "CTAG", "DS", "EM")
  head(R,5)
  # Get a character vector of the fund names
  funds <- colnames(R)


  # CRRA <- function(R, weights, lambda, sigma, m3, m4){
  #
  #   weights <- matrix(weights, ncol=1)
  #   M2.w <- t(weights) %*% sigma %*% weights
  #   M3.w <- t(weights) %*% m3 %*% (weights %x% weights)
  #   M4.w <- t(weights) %*% m4 %*% (weights %x% weights %x% weights)
  #   term1 <- (1 / 2) * lambda * M2.w
  #   term2 <- (1 / 6) * lambda * (lambda + 1) * M3.w
  #   term3 <- (1 / 24) * lambda * (lambda + 1) * (lambda + 2) * M4.w
  #
  #   out <- -term1 + term2 - term3
  #   out
  # }
  #
  # crra.moments <- function(R, ...){
  #   out <- list()
  #   out$sigma <- cov(R)
  #   out$m3 <- PerformanceAnalytics::M3.MM(R)
  #   out$m4 <- PerformanceAnalytics::M4.MM(R)
  #   out
  # }
  #

  # Construct initial portfolio with basic constraints.

  crra.portf <- PortfolioAnalytics::portfolio.spec(assets=funds)
  crra.portf <- PortfolioAnalytics::add.constraint(portfolio=crra.portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
  crra.portf <- PortfolioAnalytics::add.constraint(portfolio=crra.portf, type="box", min=0.05, max=0.4)

  # Portfolio with crra as an objective
  # Note how we can specify lambda as an argument
  crra.portf <- PortfolioAnalytics::add.objective(portfolio=crra.portf, type="return", name="CRRA", arguments=list(lambda=5))

  opt.crra <- PortfolioAnalytics::optimize.portfolio(R, crra.portf, optimize_method="DEoptim"
                                                     , search_size=5000, trace=TRUE
                                                     , traceDE=0,momentFUN="crra.moments")





  weights <- extractWeights(opt.crra)
  check_CRRA <- CRRA(R, weights, 5, cov(R), PerformanceAnalytics::M3.MM(R), PerformanceAnalytics::M4.MM(R))

  expect_equal(as.numeric(check_CRRA)
               ,as.numeric(extractObjectiveMeasures(opt.crra)$CRRA)
  )

  # Optimum Weights
  # c(CA = 0.31, CTAG = 0.344, DS = 0.284, EM = 0.052)
  # optimum objectiv value
  # -0.0004342389



})
