#' \code{MQI_ts}: MQI for a time series (vector)
#'
#' @title Modelling Quality Indicators
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#' @name MQI
#' @param obs numeric vector of observed values (yearly averages in \code{MQI_year})
#' @param mod numeric vector of modelled values (yearly averages in \code{MQI_year})
#' @param pollutant one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}
#' @param beta parameter \eqn{\beta} (default is 2)
#' @param ... arguments to be passed to \code{U_obs_95} in \code{MQI_ts} and \code{MQI_ts_synth},
#'  to \code{U_obs_95_year} in \code{MQI_year}
#'
#' @return \code{MQI_ts}: Modelling Quality Indicator for a time series,
#' as in eq.15, p.22 in Janssen et al., 2017
#' \deqn{MQI = \frac{|O_i - M_i|}{\beta U_{95}(O_i)}}{}
#' @rdname MQI
#' @export
MQI_ts <- function(obs, mod, pollutant, beta=2, ...) {
  abs(obs-mod)/(beta*U_obs_95(obs=obs, pollutant=pollutant, ...))
}

#' \code{MQI_ts_synth}: MQI for a time series (synthetic scalar)
#'
#' @return \code{MQI_ts_synth}: scalar synthetic Modelling Quality Indicator for a time series,
#' as in eq.17, p.23 in Janssen et al., 2017
#' \deqn{MQI = \frac{\sqrt{\frac{1}{N} \sum_{i=1}^N{(O_i-M_i)^2}}}{\beta \sqrt{\frac{1}{N} \sum_{i=1}^N{U_{95}(O_i)^2}}}}{}
#' @rdname MQI
#' @export
MQI_ts_synth <- function(obs, mod, pollutant, beta=2, ...) {
  RMSE(obs=obs, mod=mod)/(beta*RMS_U_obs(pollutant, ...))
}

#' \code{MQI_year}: MQI for yearly average
#'
#' @return \code{MQI_year}: Modelling Quality Indicator for yearly average,
#' as in eq.18, p.23 in Janssen et al., 2017
#' \deqn{MQI = \frac{|\bar{O}-\bar{M}|}{\beta U_{95}(\bar{O})}}{}
#' @rdname MQI
#' @export
MQI_year <- function(obs, mod, pollutant, beta=2, ...) {
  abs(obs-mod)/(beta*U_obs_95_year(obs=obs, pollutant=pollutant, ...))
}


