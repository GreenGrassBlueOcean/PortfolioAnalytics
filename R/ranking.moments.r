#' Moment ranking function for either 
#' 
#' Meucci: Fully Flexible Views Framework
#' Almgren and Chriss(AC): Portfolios from Sorts
#'
#' The moment function assumes that the assets with the highest return will continue to outperform
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param n numeric lookback period for ranking
#' @param method character either "meucci" or "ac"
#'
#' @return list ovbject containing  mu, sigma 
#' @export
#'
#' @examples
#' data("edhec")
#' AC_Moments <- ranking.moments(edhec,n=3, method  = "ac")
#' meucci_Moments <- ranking.moments(edhec,n=3, method  = "meucci")
ranking.moments <- function(R, n=1, method=c("meucci", "ac")){
  method <- match.arg(method)
  tmpR <- apply(tail(R, n), 2, function(x) prod(1 + x) - 1)
  # Assume that the assets with the highest return will continue to outperform
  asset.rank <- order(tmpR)
  switch(method,
         meucci = {
           p <- rep(1 / nrow(R), nrow(R))
           moments <- meucci.ranking(R, p, asset.rank)
         },
         ac = {
           moments <- list()
           moments$mu <- ac.ranking(R, asset.rank)
           moments$sigma <- cov(R)
         })
  return(moments)
}