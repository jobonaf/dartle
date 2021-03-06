% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{.roll_meanr_time}
\alias{.roll_meanr_time}
\title{Rolling Mean}
\usage{
.roll_meanr_time(x, time, tstep = "1 hour", n = 8, req = 6)
}
\arguments{
\item{x}{numeric vector of values to be processed}

\item{time}{vector of type \code{POSIXct}}

\item{tstep}{time step (see \code{\link{seq.POSIXt}})}

\item{n}{rolling mean width}

\item{req}{minimum valid data required in the rolling mean window}
}
\description{
Rolling Mean
}
\keyword{internal}
