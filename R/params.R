#' Parameters used to calculate the measurement uncertainty
#'
#' @param pollutant one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}
#' @title Pollutant-dependent parameters
#' @name parameters
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#'
#' @return \code{params_U}: numeric list of parameters \code{U_RV95r}, \code{alpha}, \code{RV}, \code{Np},
#'         \code{Nnp}, as in tab.2, p.21 in Janssen et al., 2017
#' @rdname parameters
#' @export
params_U <- function(pollutant=c("NO2", "O3", "PM10", "PM2.5")) {
  pollutant <- match.arg(pollutant)
  params <- switch(pollutant,
                   "NO2"   = list(U_RV95r=0.24, alpha=0.20, RV=200, Np= 5.2, Nnp=5.50),
                   "O3"    = list(U_RV95r=0.18, alpha=0.79, RV=120, Np=11  , Nnp=3.00),
                   "PM10"  = list(U_RV95r=0.28, alpha=0.13, RV= 50, Np=30  , Nnp=0.25),
                   "PM2.5" = list(U_RV95r=0.36, alpha=0.30, RV= 25, Np=30  , Nnp=0.25))
  return(params)
}


#' @return \code{perc}: numeric value in \code{[0,1]}, corresponding to the high percentile selected for the pollutant
#' (if possible, according to legislation): for hourly NO2 99.8\%, for the 8h daily maximum of ozone
#' 92.9\%, for daily PM10 and PM2.5 90.4\%
#' @rdname parameters
#' @export
perc <- function(pollutant=c("NO2", "O3", "PM10", "PM2.5")) {
  pollutant <- match.arg(pollutant)
  perc <- switch(pollutant,
                 "NO2"   = 0.998,
                 "O3"    = 0.929,
                 "PM10"  = 0.904,
                 "PM2.5" = 0.904)
  return(perc)
}


#' @return \code{threshold}: numeric values (in \eqn{\mu g/m^3}{\mug/m3}) of the threshold used to
#' calculate exceedances, according to legislation:
#' 200 for hourly NO2, 120 for the 8h daily maximum of ozone, 50 for daily PM10, \code{NA} otherwise
#' @rdname parameters
#' @export
threshold <- function(pollutant=c("NO2", "O3", "PM10", "PM2.5")) {
  pollutant <- match.arg(pollutant)
  threshold <- switch(pollutant,
                 "NO2"   = 200,
                 "O3"    = 120,
                 "PM10"  = 50,
                 "PM2.5" = NA)
  return(threshold)
}
