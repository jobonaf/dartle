#' \code{RMSE}: Root Mean Square Error
#'
#' @param obs numeric vector of observed values
#' @param mod numeric vector of modelled values
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#' @title Core Set of Statistical Indicators
#' @name indicators
#'
#' @return \code{RMSE}: Root Mean Square Error as in eq.1, tab.1, p.18 in Janssen et al., 2017
#' @rdname indicators
RMSE <- function(obs, mod) {
  dd <- (obs-mod)^2
  sqrt(sum(dd, na.rm=T)/length(dd))
}

#' \code{R}: Correlation coefficient
#'
#' @return \code{R}: Correlation coefficient as in eq.2, tab.1, p.18 in Janssen et al., 2017
#' @rdname indicators
R <- function(obs, mod) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  obs.mean <- mean(obs, na.rm=T)
  mod.mean <- mean(mod, na.rm=T)
  od <- (obs-obs.mean)
  md <- (mod-mod.mean)
  sum(md*od, na.rm=T)/(sqrt(sum(md^2, na.rm=T))*sqrt(sum(od^2, na.rm=T)))
}


#' \code{BIAS}: Bias
#'
#' @return \code{BIAS}: bias as in eq.3, tab.1, p.18 in Janssen et al., 2017
#' @rdname indicators
BIAS <- function(obs, mod) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  obs.mean <- mean(obs, na.rm=T)
  mod.mean <- mean(mod, na.rm=T)
  mod.mean - obs.mean
}

#' \code{NMB}: Normalised Mean Bias
#'
#' @return \code{NMB}: Normalised Mean Bias as in eq.3, tab.1, p.18 in Janssen et al., 2017
#' @rdname indicators
NMB <- function(obs, mod) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  obs.mean <- mean(obs, na.rm=T)
  mod.mean <- mean(mod, na.rm=T)
  (mod.mean - obs.mean)/obs.mean
}

#' \code{NMSD}: Normalised Mean Standard Deviation
#'
#' @return \code{NMSD}: Normalised Mean Standard Deviation as in eq.4, tab.1, p.18 in Janssen et al., 2017
#' @rdname indicators
NMSD <- function(obs, mod) {
  obs <- obs+mod-mod
  mod <- mod+obs-obs
  obs.sd <- sd(obs, na.rm=T)
  mod.sd <- sd(mod, na.rm=T)
  (mod.sd - obs.sd)/obs.sd
}
