#' ---
#' title: "Target Return Constraint Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a constrained portfolio optimization 
#' problem with a target return constraint.

#' Load packages and data
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
data(edhec)
ret <- edhec #[, 1:4]

#' Create an initial portfolio object with basic constraints.
init.portf <- portfolio.spec(assets=colnames(ret))
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type = "weight_sum", min_sum= 0.99, max_sum= 1.01) 
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="risk"
                               , name="StdDev", multiplier=0)


#' Add mean return objective with target return.
ret.obj.portf <- add.objective(portfolio=init.portf, type="return", 
                         name="mean", target=0.007)


#' Add return target constraint.
ret.constr.portf <- add.constraint(portfolio=init.portf, type="return", return_target=0.007)

#' Add mean return objective to the portfolio with the target return constraint.
ret.constr.portf <- add.objective(portfolio=ret.constr.portf, type="return", name="mean")

#' Relax the leverage constraints for the sum of weights for DEoptim and 
#' random portfolios.
ret.obj.portf$constraints[[1]]$min_sum <- 0.99
ret.obj.portf$constraints[[1]]$max_sum <- 1.01

ret.constr.portf$constraints[[1]]$min_sum <- 0.98
ret.constr.portf$constraints[[1]]$max_sum <- 1.02

#' The following optimization demonstrate the a target return constraint is
#' equivalent to a return objective with a target.

#' Run optimization using randomn with target return as an objective.
ret.obj.opt <- optimize.portfolio(R=ret, portfolio=ret.obj.portf,  optimize_method="random",trace=TRUE, maxSTARR = FALSE ) 
ret.obj.opt

chart.RiskReward(object = ret.obj.opt, return.col = "mean", risk.col = "StdDev")

  
# Run optimization using randomn with target return as a constraint.
ret.constr.opt <- optimize.portfolio(R=ret, portfolio=ret.constr.portf, optimize_method="random",trace=TRUE)
ret.constr.opt

chart.RiskReward(object = ret.constr.opt, return.col = "mean", risk.col = "StdDev")



#' Run the optimizations using DEoptim as the optimization engine.
set.seed(123)
opt.obj.de <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, trace = TRUE,
                                 optimize_method="DEoptim", search_size=2000, traceDE=5)
opt.obj.de

chart.RiskReward(object = opt.obj.de, return.col = "mean", risk.col = "StdDev")

# run optimization with DEoptim using ret.constr.portf
set.seed(123)
opt.constr.de <- optimize.portfolio(R=ret, portfolio=ret.constr.portf,
                                    optimize_method="DEoptim", search_size=2000, traceDE=5)
opt.constr.de

#' Run the optimizations using DEoptim as the optimization engine.
opt.obj.rp <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, 
                                 optimize_method="random", search_size=2000)
opt.obj.rp

# run optimizations with random portfolios using ret.constr.portf
opt.constr.rp <- optimize.portfolio(R=ret, portfolio=ret.constr.portf, 
                                    optimize_method="random", search_size=2000)
opt.constr.rp
