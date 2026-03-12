#' Tracking Error Custom Objective Function
#'
#' Define a custom objective function for penalized tracking error
#' @source \url{https://rossb34.github.io/PortfolioAnalyticsPresentation2017/#29}
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param weights weights vector
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of benchmark returns
#' @param min.te a numeric value for the minimum annualized Tracking Error
#' @param max.te a numeric value for the maximum annualized Tracking Error
#' @param scale a numeric value how many rows there are per year/period.
#'
#' @return a tracking error minimization value
#' @export
#'
#' @examples
#' #using the final column of R als benchmark for the sake of simplicity
#' data("edhec")
#' Equalweights <- rep(1/(ncol(edhec)-1), ncol(edhec)-1)
#' test <- te.target( R = edhec[,1:12], Rb = edhec[,13]*10,  weights = Equalweights
#'                  , min.te = 0.02, max.te = 0.05, scale = 12) 
te.target <- function(R, Rb, weights, min.te = 0.02, max.te = 0.05, scale = 12){
  r <- Return.portfolio(R = R, weights = weights)
  Rb <- Rb[index(r)]
  te <- sd(r - Rb) * sqrt(scale)
  # penalize tracking error outside of [min.te, max.te] range
  out <- 0
  if(te > max.te)
    out <- (te - max.te) * 10000
  if(te < min.te)
    out <- (min.te - te) * 10000
  return(out)
}