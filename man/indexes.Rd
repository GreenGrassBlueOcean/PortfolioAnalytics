% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{indexes}
\alias{indexes}
\title{Six Major Economic Indexes}
\format{
CSV converted into xts object with montly observations
}
\source{
unknown
}
\usage{
data("indexes")
}
\description{
Monthly data of five indexes beginning on 2000-01-31 and ending 2009-12-31. The indexes are: US Bonds, US Equities, International Equities, Commodities, US T-Bills, and Inflation
}
\examples{
data(indexes)

#preview the data
head(indexes)

#summary period statistics
summary(indexes)
}
