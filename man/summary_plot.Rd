% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary_plot}
\alias{summary_plot}
\alias{summary_report}
\title{summary plot}
\usage{
summary_report(data, obs, mod, point, pollutant = c("NO2", "O3", "PM10",
  "PM2.5"), beta = 2)

summary_plot(s_rep, title = NULL)
}
\arguments{
\item{data}{a data frame including observations and model forecasts}

\item{obs}{name of the column with observed data}

\item{mod}{name of the column with forecasts}

\item{point}{name of the column with station ID}

\item{pollutant}{one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}}

\item{beta}{parameter \eqn{\beta} (default is 2)}

\item{s_rep}{output of \code{summary_report}}

\item{title}{main title for the plot (if \code{NULL}, is build as \code{paste("Summary statistics",s_rep$parameters$pollutant)})}
}
\value{
\code{summary_report} returns a list of 3:
\describe{
  \item{\code{summary_points}}{a data frame with 8 variables for each station \describe{
  \item{\code{Point}}{station ID}
  \item{\code{Obs.ave}}{annual mean of observations (paired with forecasts)}
  \item{\code{Mod.ave}}{annual mean of forecasts (paired with observations)}
  \item{\code{mpi_bias}}{Model Performance Indicator for the bias (see \link{MPI_bias})}
  \item{\code{mpi_corr_time}}{Model Performance Indicator for the correlation in time (see \link{MPI_corr_time})}
  \item{\code{mpi_sdev_time}}{Model Performance Indicator for the standard deviation in time (see \link{MPI_sdev_time})}
  \item{\code{mpi_perc}}{Model Performance Indicator for high percentile values (see \link{MPI_perc})}
  \item{\code{n_valid}}{no. of valid data}
  }}
  \item{\code{summary_overall}}{a data frame with 3 overall indicators \describe{
  \item{\code{mpi_corr_space}}{Model Performance Indicator for the correlation in space (see \link{MPI_corr_space})}
  \item{\code{mpi_sdev_space}}{Model Performance Indicator for the standard deviation in space (see \link{MPI_sdev_space})}
  \item{\code{n_points}}{no. of valid stations}
  }}
  \item{\code{parameters}}{a list of 7 parameters \describe{
  \item{\code{U_RV95r}}{see \link{params_U}}
  \item{\code{alpha}}{see \link{params_U}}
  \item{\code{RV}}{see \link{params_U}}
  \item{\code{Np}}{see \link{params_U}}
  \item{\code{Nnp}}{see \link{params_U}}
  \item{\code{pollutant}}{same as in input, one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}}
  \item{\code{beta}}{same as in input}
  }}
}
}
\description{
\code{summary_report}: prepare input
}
\examples{
# prepare dataset
require(dplyr)
Mod <- dMean(mod.data \%>\% filter(Var=="c_PM10"),
             value = "Value", time = "Time", point = "Point")
Obs <- obs.pm10 \%>\% mutate(Day=format(Time,"\%Y-\%m-\%d"),
                           Point=ID)
Dat <- inner_join(Mod, Obs, by=c("Point", "Day"), suffix = c(".mod", ".obs"))

# calculate indicators
s_rep <- summary_report(Dat, obs = "Value.obs", mod = "Value.mod",
                       point = "Point", pollutant = "PM10")

# plot
summary_plot(s_rep)
}
\references{
Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
}
