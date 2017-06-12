#' \code{MPI_bias}: MPI for bias
#'
#' @title Modelling Performance Indicators
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#' @name MPI
#' @param obs numeric vector of observed values (yearly averages in \code{MPI_*_space})
#' @param mod numeric vector of modelled values (yearly averages in \code{MPI_*_space})
#' @param pollutant one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}
#' @param beta parameter \eqn{\beta} (default is 2)
#' @param ... arguments to be passed to \code{RMS_U_obs} in \code{MPI_bias} and \code{MPI_*_time},
#' to \code{U_obs_95_year} in \code{MPI_*_space}, to \code{U_obs_95} in \code{MPI_perc}
#'
#' @return \code{MPI_bias}: Modelling Performance Indicator for the bias,
#' as in eq.26, tab.4, p.27 in Janssen et al., 2017
#' \deqn{MPI_{bias} = \frac{BIAS}{\beta RMS_U}}{}
#' @rdname MPI
#' @export
MPI_bias <- function(obs, mod, pollutant, beta=2, ...) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  bias <- BIAS(obs = obs, mod = mod)
  rmsu <- RMS_U_obs(obs = obs, pollutant = pollutant, ...)
  bias/(beta*rmsu)
}

#' \code{MPI_corr_time}: temporal MPI for correlation
#'
#' @return \code{MPI_corr_time}: temporal Modelling Performance Indicator for the correlation,
#' as in eq.27, tab.4, p.27 in Janssen et al., 2017
#' \deqn{MPI_{corr,time} = \frac{R} { \left(1 - \frac{\beta^2  RMS_U^2} { 2 \sigma_O \sigma_M} \right) }}{}
#' @rdname MPI
#' @export
MPI_corr_time <- function(obs, mod, pollutant, beta=2, ...) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  so <- sd(obs, na.rm=T)
  sm <- sd(mod, na.rm=T)
  r <- R(obs = obs, mod = mod)
  rmsu <- RMS_U_obs(obs = obs, pollutant = pollutant, ...)
  r / (1 - (beta^2 * rmsu^2) / (2*so*sm))
}

#' \code{MPI_sdev_time}: temporal MPI for standard deviation
#'
#' @return \code{MPI_sdev_time}: temporal Modelling Performance Indicator for the standard deviation,
#' as in eq.28, tab.4, p.27 in Janssen et al., 2017
#' \deqn{MPI_{s.d.,time} = \frac{\sigma_M - \sigma_O}{\beta RMS_U}}{}
#' @rdname MPI
#' @export
MPI_sdev_time <- function(obs, mod, pollutant, beta=2, ...) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  so <- sd(obs, na.rm=T)
  sm <- sd(mod, na.rm=T)
  rmsu <- RMS_U_obs(obs = obs, pollutant = pollutant, ...)
  (sm-so) / (beta*rmsu)
}

#' \code{MPI_corr_space}: spatial MPI for correlation
#'
#' @return \code{MPI_corr_space}: spatial Modelling Performance Indicator for the correlation,
#' as in eq.29, tab.5, p.27 in Janssen et al., 2017
#' \deqn{MPI_{corr,space} =  \frac{R} { \left(1 - \frac{\beta^2  RMS_{\bar{U}}^2} { 2 \sigma_{\bar{O}} \sigma_{\bar{M}}} \right) }}{}
#' @rdname MPI
#' @importFrom stats na.omit
#' @export
MPI_corr_space <- function(obs, mod, pollutant, beta=2, ...) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  so <- sd(obs, na.rm=T)
  sm <- sd(mod, na.rm=T)
  r <- R(obs = obs, mod = mod)
  uu <- na.omit(U_obs_95_year(obs = obs, pollutant = pollutant, ...))
  rmsu <- sqrt(sum(uu^2)/length(uu))
  r / (1 - (beta^2 * rmsu^2) / (2*so*sm))
}

#' \code{MPI_sdev_space}: spatial MPI for standard deviation
#'
#' @return \code{MPI_sdev_space}: spatial Modelling Performance Indicator for the standard deviation,
#' as in eq.30, tab.5, p.27 in Janssen et al., 2017
#' \deqn{MPI_{s.d.,space} = \frac{\sigma_{\bar{M}} - \sigma_{\bar{O}}}{\beta RMS_{\bar{U}}}}{}
#' @rdname MPI
#' @export
MPI_sdev_space <- function(obs, mod, pollutant, beta=2, ...) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  so <- sd(obs, na.rm=T)
  sm <- sd(mod, na.rm=T)
  uu <- na.omit(U_obs_95_year(obs = obs, pollutant = pollutant, ...))
  rmsu <- sqrt(sum(uu^2)/length(uu))
  (sm-so) / (beta*rmsu)
}

#' \code{MPI_perc}: MPI for high percentile values
#'
#' @return \code{MPI_perc}: Modelling Performance Indicator for high percentile values,
#' as in eq.35, p.35 in Janssen et al., 2017
#' \deqn{MPI_{perc} = \frac{M_{perc} - O_{perc}}{\beta U_{95} (O_{perc})}}{}
#' @rdname MPI
#' @importFrom stats quantile
#' @export
MPI_perc <- function(obs, mod, pollutant, beta=2, ...) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  pp <- perc(pollutant)
  mp <- quantile(mod, probs = pp, na.rm=T)
  op <- quantile(obs, probs = pp, na.rm=T)
  uu <- U_obs_95(obs = op, pollutant = pollutant, ...)
  (mp - op) / (beta * uu)
}

