

#' \code{U_obs_95}: Measurements uncertainty
#'
#' @title Measurements Uncertainty
#' @name obs_uncertainty
#' @param obs numeric vector of observed values (yearly averaged for \code{U_obs_95_year})
#' @param pollutant one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}
#' @param U_RV95r parameter \eqn{U^{RV}_{95,r}}{U^{RV}_{95,r}}; optional if \code{pollutant} is given, otherwise compulsory
#' @param alpha  parameter \eqn{\alpha}{\alpha}; optional if \code{pollutant} is given, otherwise compulsory
#' @param RV  parameter \eqn{RV}{RV}; optional if \code{pollutant} is given, otherwise compulsory
#' @param Np  parameter \eqn{N_p}{N_p}; optional if \code{pollutant} is given, otherwise compulsory
#' @param Nnp  parameter \eqn{N_{np}}{N_np}; optional if \code{pollutant} is given, otherwise compulsory
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#'
#' @return \code{U_obs_95}: measurements uncertainty \eqn{U_{95}}{U_95},
#' as in eq.10, p.20 in Janssen et al., 2017
#' \deqn{U_{95}(O_i) = U^{RV}_{95,r} \sqrt{(1-\alpha^2) O^2_i + \alpha^2 \cdot RV^2}}{}
#' @rdname obs_uncertainty
#' @examples # concentrations
#' cc <- 1:400
#'
#' # uncertainties for time series
#' Uo3   <- U_obs_95(cc, pollutant = "O3")
#' Upm10 <- U_obs_95(cc, pollutant = "PM10")
#' Upm25 <- U_obs_95(cc, pollutant = "PM2.5")
#' Uno2  <- U_obs_95(cc, pollutant = "NO2")
#' plot(cc, Uo3, type="l", log = "xy", xlab="measured concentration",
#' ylab="uncertainty", ylim=c(1,100))
#' lines(cc,Upm10,lty=2)
#' lines(cc,Upm25,lty=3)
#' lines(cc,Uno2,lty=4)
#' legend("topleft",lty=1:4,legend=c("O3", "PM10", "PM2.5", "NO2"))
#' @export
U_obs_95 <- function(obs, pollutant=NULL, U_RV95r=NULL, alpha=NULL, RV=NULL) {
  if(!is.null(pollutant)) params <- params_U(pollutant)
  if(!is.null(U_RV95r)) params$U_RV95r <- U_RV95r
  if(!is.null(alpha))   params$alpha   <- alpha
  if(!is.null(RV))      params$RV      <- RV
  params$U_RV95r * sqrt((1 - params$alpha^2) * obs^2 + (params$alpha^2 * params$RV^2))
}


#' \code{RMS_U_obs}: Root Mean Square Uncertainty
#'
#' @return \code{RMS_U_obs}: root mean square uncertainty \eqn{RMS_{U}}{RMS_U},
#' as in eq.11, p.20 in Janssen et al., 2017
#' \deqn{RMS_U = U^{RV}_{95,r} \sqrt{(1-\alpha^2) (\bar{O}^2 + \sigma_O^2) + \alpha^2 \cdot RV^2}}{}
#' @rdname obs_uncertainty
#' @title Root Mean Square Uncertainty
#' @export
RMS_U_obs <- function(obs, pollutant=NULL, U_RV95r=NULL, alpha=NULL, RV=NULL) {
  if(!is.null(pollutant)) params <- params_U(pollutant)
  if(!is.null(U_RV95r)) params$U_RV95r <- U_RV95r
  if(!is.null(alpha))   params$alpha   <- alpha
  if(!is.null(RV))      params$RV      <- RV
  obs.mean <- mean(obs, na.rm=T)
  obs.sd <- sd(obs, na.rm=T)
  params$U_RV95r * sqrt((1 - params$alpha^2) * (obs.mean^2 + obs.sd^2) + (params$alpha^2 * params$RV^2))
}


#'\code{U_obs_95_year}: Measurements uncertainty for yearly averaged values
#'
#' @return \code{U_obs_95_year}: measurements uncertainty for yearly averaged values \eqn{U_{95}}{U_95},
#' as in eq.12, p.20 in Janssen et al., 2017
#' \deqn{U_{95}(\bar{O}) \cong U^{RV}_{95,r} \sqrt{\frac{(1-\alpha^2)}{N_p}\bar{O}^2 + \frac{\alpha^2 \cdot RV^2}{N_{np}}}}{}
#' @rdname obs_uncertainty
#' @examples
#'
#' # uncertainties for yearly averages
#' Uo3   <- U_obs_95_year(cc, pollutant = "O3")
#' Upm10 <- U_obs_95_year(cc, pollutant = "PM10")
#' Upm25 <- U_obs_95_year(cc, pollutant = "PM2.5")
#' Uno2  <- U_obs_95_year(cc, pollutant = "NO2")
#' plot(cc, Uo3, type="l", log = "xy", xlab="measured concentration",
#' ylab="uncertainty", ylim=c(1,100))
#' lines(cc,Upm10,lty=2)
#' lines(cc,Upm25,lty=3)
#' lines(cc,Uno2,lty=4)
#' legend("topleft",lty=1:4,legend=c("O3", "PM10", "PM2.5", "NO2"))
#' @export
U_obs_95_year <- function(obs, pollutant=NULL, U_RV95r=NULL, alpha=NULL, RV=NULL, Np=NULL, Nnp=NULL) {
  if(!is.null(pollutant)) params <- params_U(pollutant)
  if(!is.null(U_RV95r)) params$U_RV95r <- U_RV95r
  if(!is.null(alpha))   params$alpha   <- alpha
  if(!is.null(RV))      params$RV      <- RV
  if(!is.null(Np))      params$Np      <- Np
  if(!is.null(Nnp))     params$Nnp     <- Nnp
  params$U_RV95r * sqrt((1 - params$alpha^2) * obs^2 / params$Np + (params$alpha^2 * params$RV^2) / params$Nnp)
}
