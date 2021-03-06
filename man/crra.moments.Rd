% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CRRA.R
\name{crra.moments}
\alias{crra.moments}
\title{# Custom moments fucntion for use in optimize portfolio
supplies moments to CRRA objective function}
\usage{
crra.moments(R, ...)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}

\item{...}{passthrough to downastream methods}
}
\value{
list ovbject containing  mu, sigma, m3, m4
}
\description{
# Custom moments fucntion for use in optimize portfolio
supplies moments to CRRA objective function
}
\examples{
data("edhec")
Moments <- crra.moments(edhec)
}
