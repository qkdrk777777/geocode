% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode.R
\name{geocode}
\alias{geocode}
\title{geocode by using selenium and googlemaps}
\usage{
geocode(name, port1 = 4565L, port2 = 4566L, port3 = 4567L, n = 3,
  plot = F)
}
\arguments{
\item{name}{region list(unique list ; if name is not unique then change unique).}

\item{n}{is sleep time.}

\item{plot}{browser open or not.}
}
\description{
geocode by using selenium and googlemaps
}
\examples{
geocode(name=c('daegu','daegu','Gyeongju','Daegu University'),plot=F)
}
