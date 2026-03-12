#' ---
#' title: "FRAPO Regime Switching Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a portfolio with FRAPO optimization. This does currently not work

#' Load the package and data



library(FRAPO)
library(Rsolnp)
data(MultiAsset)
Rets <- FRAPO::returnseries(MultiAsset, method = "discrete", trim = TRUE, percentage = TRUE)
V <- cov(Rets)
## Budget Constraint
Budget <- function(x, Sigma) sum(x)
ERC <- FRAPO::PERC(V, eqfun = Budget, eqB = 1)
ERC
w <- Weights(ERC) / 100
w

StdDev(Rets, weights = w, portfolio_method = "component")
z <- PERC(V)
z@opt$objective

# requires frapo_perc_proto branch
library(PortfolioAnalytics)
data(edhec)
R <- DailyReturns #edhec[, 1:8]
funds <- colnames(R)

#' Construct initial portfolio with basic constraints.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="leverage", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- PortfolioAnalytics::add.constraint( portfolio = init.portf
                                                , type = "weight_sum"
                                                , min_sum  = 0.99
                                                , max_sum  = 1.01
                                                )

init.portf <- add.constraint(portfolio=init.portf, type="long_only")
eqStdDev.portf <- add.objective(portfolio=init.portf, type="return", name="mean")


eqStdDev.portf <- PortfolioAnalytics::add.objective( portfolio=eqStdDev.portf, type="risk_budget"
                                                            , name="ETL", min_concentration=TRUE
                                                            , arguments=list(p=0.925, method="modified", clean="boudt")
                                                            , multiplier=1
                                                            )

eqStdDev.portf <- PortfolioAnalytics::add.objective( portfolio=eqStdDev.portf, type="risk", name="ETL"
                                                   , arguments=list(p=0.95, method="modified",  clean="boudt"))


# 
# eqStdDev.portf <- add.objective(portfolio=eqStdDev.portf, type="risk_budget",
#                                 name="StdDev", min_concentration = TRUE)
eqStdDev.FRAPO <- optimize.portfolio(R=R, portfolio=eqStdDev.portf,
                                     optimize_method="FRAPO", trace=TRUE)
eqStdDev.FRAPO

eqStdDev.FRAPO.rebal <- PortfolioAnalytics::optimize.portfolio.rebalancing( R = R
                                                                          , portfolio=eqStdDev.portf
                                                                          , optimize_method = "FRAPO"
                                                                          , trace = TRUE, traceDE = 20
                                                                          , message = TRUE
                                                                          , rebalance_on = "quarters"
                                                                          , training_period = 36
                                                                          , rolling_window = ceiling(252/4)
                                                                          , parallel = FALSE
)
