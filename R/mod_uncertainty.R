#' \code{RMS_U_mod}: RMS uncertainty for model time series
#'
#' @title Model Uncertainty
#' @name mod_uncertainty
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#' @param obs numeric vector of observed values (yearly averages in \code{U_mod_year})
#' @param mod numeric vector of modelled values (yearly averages in \code{U_mod_year})
#' @param pollutant one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}
#' @param ... arguments to be passed to \code{U_obs_95} in \code{RMS_U_mod},
#'  to \code{U_obs_95_year} in \code{U_mod_year}
#'
#' @return \code{RMS_U_mod}: root mean square uncertainty for model time series,
#' as in eq.23, p.24 in Janssen et al., 2017
#' \deqn{RMS_{U_M} = RMS_U \sqrt{\left(\frac{RMSE}{RMS_U}\right)^2-1}}{}
#' @rdname mod_uncertainty
#' @export
RMS_U_mod <- function(obs, mod, pollutant, ...) {
  rmsu <- RMS_U_obs(obs = obs, pollutant = pollutant, ...)
  rmse <- RMSE(obs = obs, mod = mod)
  rmsu * sqrt((rmse/rmsu)^2 - 1)
}

#' \code{U_mod_year}: Uncertainty for model yearly averages
#'
#' @return \code{MQI_ts}: uncertainty for model yearly averages,
#' as in eq.24, p.24 in Janssen et al., 2017
#' \deqn{U(\bar{M}) = U_{95}(\bar{O}) \sqrt{\left(\frac{BIAS}{U_{95}(\bar{O})}\right)^2-1}}{}
#' @rdname mod_uncertainty
#' @export
U_mod_year <- function(obs, mod, pollutant, ...) {
  u95 <- U_obs_95_year(obs = obs, pollutant = pollutant, ...)
  bias <- BIAS(obs = obs, mod = mod)
  u95 * sqrt((bias/u95)^2 - 1)
}
