#' \code{target_report}: prepare input for target plot
#'
#' @title Target plot
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#' @param data a data frame including observations and model forecasts
#' @param obs name of the column with observed data
#' @param mod name of the column with forecasts
#' @param point name of the column with station ID
#' @param pollutant one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}
#' @param beta parameter \eqn{\beta} (default is 2)
#' @param t_rep output of \code{target_report}
#' @name target_plot
#'
#' @return \code{target_report} returns a list of 3:
#' \describe{
#'   \item{\code{quality_points}}{a data frame with 9 variables for each station \describe{
#'   \item{\code{Point}}{station ID}
#'   \item{\code{Obs.ave}}{annual mean of observations (paired with forecasts)}
#'   \item{\code{Mod.ave}}{annual mean of forecasts (paired with observations)}
#'   \item{\code{rmsu}}{\eqn{RMS_U} (see \link{RMS_U_obs})}
#'   \item{\code{crmse_norm}}{\eqn{CRMSE/(\beta*RMS_U)}{CRMSE/(\beta RMS_U)}}
#'   \item{\code{r}}{R (see \link{R})}
#'   \item{\code{crmse_ratio}}{\eqn{abs(NMSD)/sqrt(2*(1-R))}{\frac{|NMSD|}{\sqrt{2(1-R)}}} as in eq.34, p.30 in Janssen et al., 2017}
#'   \item{\code{bias_norm}}{\eqn{BIAS/(\beta*RMS_U)}{BIAS/(\beta RMS_U)}}
#'   \item{\code{mqi_ts}}{Model Quality Indicator \eqn{RMSE/(\beta*RMS_U)}{RMSE/(\beta RMS_U)} (see \link{MQI_ts_synth})}
#'   \item{\code{mqi_year}}{Model Quality Indicator for yearly averages (see \link{MQI_year})}
#'   \item{\code{n_valid}}{no. of valid data}
#'   }}
#'   \item{\code{quality_overall}}{a data frame with 3 overall indicators \describe{
#'   \item{\code{mqi_ts_p90}}{90th percentile of \code{mqi_ts}}
#'   \item{\code{mqi_year_p90}}{90th percentile of \code{mqi_year}}
#'   \item{\code{n_points}}{no. of valid stations}
#'   }}
#'   \item{\code{parameters}}{a list of 7 parameters \describe{
#'   \item{\code{U_RV95r}}{see \link{params_U}}
#'   \item{\code{alpha}}{see \link{params_U}}
#'   \item{\code{RV}}{see \link{params_U}}
#'   \item{\code{Np}}{see \link{params_U}}
#'   \item{\code{Nnp}}{see \link{params_U}}
#'   \item{\code{pollutant}}{same as in input, one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}}
#'   \item{\code{beta}}{same as in input}
#'   }}
#' }
#' @export
#' @import dplyr
#'
#' @examples
#' # prepare dataset
#' require(dplyr)
#' Mod <- dMean(mod.data %>% filter(Var=="c_PM10"),
#'              value = "Value", time = "Time", point = "Point")
#' Obs <- obs.pm10 %>% mutate(Day=format(Time,"%Y-%m-%d"),
#'                            Point=ID)
#' Dat <- inner_join(Mod, Obs, by=c("Point", "Day"), suffix = c(".mod", ".obs"))
#'
#' # calculate indicators
#' t_rep <- target_report(Dat, obs = "Value.obs", mod = "Value.mod",
#'                        point = "Point", pollutant = "PM10")
#'
#' # plot
#' target_plot(t_rep)
#' @rdname target_plot
target_report <- function(data, obs, mod, point,
                          pollutant=c("NO2", "O3", "PM10", "PM2.5"),
                          beta=2) {
  pollutant <- match.arg(pollutant)
  expected <- switch(pollutant,
                     "NO2"=365*24*0.75,
                     "PM10"=365*0.75,
                     "PM2.5"=365*0.75,
                     "O3"=365*0.75)
  data <- as.data.frame(data)
  #Mod.ave=Obs.ave=Point=bias_norm=crmse_norm=n_valid=rmsu=NULL
  Dat <- data.frame(Obs=data[,obs], Mod=data[,mod], Point=data[,point])
  Dat %>% mutate(Obs = Obs+Mod-Mod,
                 Mod = Mod+Obs-Obs) %>%
    filter(!is.na(Obs))-> Dat
  Dat %>% group_by(Point) %>%
    summarize(Obs.ave = mean(Obs, na.rm=T),
              Mod.ave = mean(Mod, na.rm=T),
              rmsu       = RMS_U_obs(obs = Obs, pollutant = pollutant),
              crmse_norm = CRMSE(mod = Mod, obs = Obs)/(beta*rmsu),
              r          = R(mod = Mod, obs = Obs),
              crmse_ratio= abs(NMSD(mod = Mod, obs = Obs))/sqrt(2*(1-r)),
              bias_norm  = BIAS(mod = Mod, obs = Obs)/(beta*rmsu),
              mqi_ts     = MQI_ts_synth(mod = Mod, obs = Obs, pollutant = pollutant),
              mqi_year   = MQI_year(obs = Obs.ave, mod = Mod.ave, pollutant = pollutant),
              n_valid = n()) %>%
    filter(n_valid>=expected) -> quality_points
  quality_overall <- data.frame(mqi_ts_p90    = quantile(quality_points$mqi_ts, probs=0.9, na.rm=T),
                                mqi_year_p90  = quantile(quality_points$mqi_year, probs=0.9, na.rm=T),
                                n_points = nrow(quality_points))

  out <- list()
  out$quality_points  <- quality_points
  out$quality_overall <- quality_overall
  out$parameters <- params_U(pollutant=pollutant)
  out$parameters$pollutant <- pollutant
  out$parameters$beta <- beta
  return(out)
}



#' \code{target_plot}: target plot
#'
#' @import ggplot2
#' @return \code{target_plot} returns a target plot (object of class \code{ggplot} and \code{gg})
#' @rdname target_plot
#' @export
target_plot <- function(t_rep) {
  Point=bias_norm=crmse_norm=NULL
  ggplot(data = t_rep$quality_points,
         aes(x = crmse_norm*sign(crmse_ratio-1), y = bias_norm, shape = Point)) +
    geom_point(size=2.5, col="dodgerblue4")  +
    scale_shape_manual(name='',
                       values = unlist(lapply(c(LETTERS,letters)[1:length(t_rep$quality_points$Point)], utf8ToInt))) +
    stat_function(fun=function(x){+sqrt(1-x^2)}, geom="area", aes(fill="chartreuse3"), n=1001,
                  alpha=0.4, col=NA, inherit.aes = FALSE)+
    stat_function(fun=function(x){-sqrt(1-x^2)}, geom="area", aes(fill="chartreuse3"), n=1001,
                  alpha=0.4, col=NA, inherit.aes = FALSE)+
    scale_fill_manual(values = c('chartreuse3' = 'chartreuse3'),name = '',
                      labels = expression(RMSE <= beta*RMS[U]))+
    stat_function(fun=function(x){+sqrt((1/t_rep$parameters$beta)^2-x^2)}, geom="line",
                  colour="darkgrey", n=2001, linetype=3, inherit.aes = FALSE)+
    stat_function(fun=function(x){-sqrt((1/t_rep$parameters$beta)^2-x^2)}, geom="line",
                  colour="darkgrey", n=2001, linetype=3, inherit.aes = FALSE) -> p
  p +
    theme_bw() + theme(legend.position="bottom")+
    geom_abline(intercept=0,slope=+1,col="darkgrey")+
    geom_abline(intercept=0,slope=-1,col="darkgrey")+
    xlim(-2,2) + ylim(-2,2) +
    scale_x_continuous(labels=c(2,1,0,1,2))+
    xlab(expression(CRMSE[norm]))+ylab(expression(BIAS[norm])) +
    coord_fixed() -> p
  p +
    annotate(geom = "text", x =  0, y =  1.8, label = "BIAS>0" , color = "darkgrey", alpha=0.8)+
    annotate(geom = "text", x =  0, y = -1.8, label = "BIAS<0" , color = "darkgrey", alpha=0.8)+
    annotate(geom = "text", x =  2, y =    0, label = "St.Dev.", color = "darkgrey", alpha=0.8, adj=1)+
    annotate(geom = "text", x = -2, y =    0, label = "R"      , color = "darkgrey", alpha=0.8, adj=0)+
    annotate(geom = "text", x = -2, y =  1.8, size=6, parse=T,
             label = paste0("MQI==",round(t_rep$quality_overall$mqi_ts_p90,2)),
             color = ifelse(round(t_rep$quality_overall$mqi_ts_p90,2)>1,
                            "firebrick3","green4"), adj=0, alpha=0.8)+
    annotate(geom = "text", x = -2, y =  1.6, size=4, parse=T,
             label = paste0("MQI[year]==",round(t_rep$quality_overall$mqi_year_p90,2)),
             color = ifelse(round(t_rep$quality_overall$mqi_year_p90,2)>1,
                            "firebrick3","green4"), adj=0, alpha=0.8) +
    annotate(geom = "text", x = -2, y = -1  , size=3, parse=T,
             label = paste0("U['95,r']^{RV}==",t_rep$parameters$U_RV95r),
             color = "grey20", adj=0, alpha=0.8) +
    annotate(geom = "text", x = -2, y = -1.2, size=3, parse=T,
             label = paste0("alpha==",t_rep$parameters$alpha),
             color = "grey20", adj=0, alpha=0.8) +
    annotate(geom = "text", x = -2, y = -1.4, size=3, parse=T,
             label = paste0("RV==",t_rep$parameters$RV),
             color = "grey20", adj=0, alpha=0.8) +
    annotate(geom = "text", x = -2, y = -1.6, size=3, parse=T,
             label = paste0("beta==",t_rep$parameters$beta),
             color = "grey20", adj=0, alpha=0.8)-> p
  p +
    ggtitle(paste("Target plot",t_rep$parameters$pollutant)) -> p
  p$layers <- p$layers[c(2:length(p$layers),1)]
  return(p)
}

