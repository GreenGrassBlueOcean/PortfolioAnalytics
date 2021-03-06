% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CRRA.R
\name{CRRA}
\alias{CRRA}
\title{Fourth order expansion of the constant Relative Risk Aversion (CRRA)
Utility function as in Martinelli and Ziemann (2010) and Bound, Lu and Peeters
(2015) http://ssrn.com/abstract=2409603}
\usage{
CRRA(R, weights, lambda, sigma, m3, m4)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}

\item{weights}{a numeric  containing portfolio weights}

\item{lambda}{a numeric containing lambda either 5 or 10}

\item{sigma}{a numeric vector containing sigma}

\item{m3}{a numeric vector containing M3 moments}

\item{m4}{a numeric vector containing m4 moments}
}
\value{
a CRRA minimization value
}
\description{
Fourth order expansion of the constant Relative Risk Aversion (CRRA)
Utility function as in Martinelli and Ziemann (2010) and Bound, Lu and Peeters
(2015) http://ssrn.com/abstract=2409603
}
\examples{
data("edhec")
Moments <- crra.moments(edhec)
Equalweights <- rep(1/ncol(edhec), ncol(edhec))
test <- CRRA(R = edhec, weights = Equalweights
, lambda = 5, sigma = Moments$sigma
, m3 = Moments$m3, m4 = Moments$m4)
}
