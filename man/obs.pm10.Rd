% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{obs.pm10}
\alias{obs.pm10}
\title{Observed PM10 concentrations}
\format{A data frame with 9504 rows and 8 variables:
\describe{
  \item{Time}{day (POSIXct format)}
  \item{ID}{station code}
  \item{Station}{station name}
  \item{Value}{concentration (in \eqn{\mu g/m^3}{\mug/m3})}
  \item{ZoneType}{\code{"URB"} (urban), \code{"SBR"} (suburban) or \code{"RUR"} (rural)}
  \item{StationType}{always \code{"BKG"} (background)}
  \item{Lat}{latitude}
  \item{Lon}{longitude}
}}
\source{
\url{http://www.arpa.fvg.it/}
}
\usage{
obs.pm10
}
\description{
A dataset containing daily concentrations of PM10 measured at background sites in the
region of Friuli Venezia Giulia (Italy)
}
\keyword{datasets}
