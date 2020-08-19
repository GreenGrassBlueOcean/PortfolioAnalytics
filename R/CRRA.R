#'Fourth order expansion of the constant Relative Risk Aversion (CRRA)
#' Utility function as in Martinelli and Ziemann (2010) and Bound, Lu and Peeters
#'(2015) http://ssrn.com/abstract=2409603
#" Maximize return while Allowing for constant risk aversion
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param weights a numeric  containing portfolio weights 
#' @param lambda a numeric containing lambda either 5 or 10
#' @param sigma a numeric vector containing sigma
#' @param m3 a numeric vector containing M3 moments
#' @param m4 a numeric vector containing m4 moments
#'
#' @return a CRRA minimization value
#' @export
#' @importFrom utils tail
#'
#' @examples
#' data("edhec")
#' Moments <- crra.moments(edhec)
#' Equalweights <- rep(1/ncol(edhec), ncol(edhec))
#' test <- CRRA(R = edhec, weights = Equalweights
#' , lambda = 5, sigma = Moments$sigma
#' , m3 = Moments$m3, m4 = Moments$m4)
CRRA <- function(R, weights, lambda, sigma, m3, m4){
  weights <- matrix(weights, ncol = 1)
  M2.w <- t(weights) %*% sigma %*% weights
  M3.w <- t(weights) %*% m3 %*% (weights %x% weights)
  M4.w <- t(weights) %*% m4 %*% (weights %x% weights %x% weights)
  term1 <- 0.5 * lambda * M2.w
  term2 <- (1/6) * lambda * (lambda + 1) * M3.w
  term3 <- (1/24) * lambda * (lambda + 1) * (lambda + 2) * M4.w
  out <- - term1 + term2 - term3
}


#' # Custom moments fucntion for use in optimize portfolio
#' supplies moments to CRRA objective function
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param ... passthrough to downastream methods
#'
#' @return list ovbject containing  mu, sigma, m3, m4 
#' @importFrom PerformanceAnalytics M3.MM M4.MM
#' @export 
#'
#' @examples
#' data("edhec")
#' Moments <- crra.moments(edhec)
crra.moments <- function(R, ...){
  
  out <- list()
  # out$mu <- matrix( as.vector(apply(R,2,'mean', na.rm=TRUE)),ncol=1) #LVDB own input!!
  out$mu <- colMeans(R)
  out$sigma <- stats::cov(R)
  out$m3 <- PerformanceAnalytics::M3.MM(R)
  out$m4 <- PerformanceAnalytics::M4.MM(R)
  out
}




#' Objective function for  to compute annualized standard deviation for MONTHLY data 
#'
#' @param R list of monthlu returns
#' @param weights character vector of weights
#' @param sigma cov(R)
#' @param N Number of months default 36
#'
#' @return matrix with objective value
#' @export
#'
#' @examples
#' data("edhec")
#' pasd(R = edhec, weights = rep(1/ncol(edhec), times = 13 ), stats::cov(edhec), N = 36  )
pasd <- function(R, weights, sigma, N=36){
  R <- tail(R, N)
  tmp.sd <- sqrt(as.numeric(t(weights) %*% sigma %*% weights))
  return(sqrt(12) * tmp.sd)
}
