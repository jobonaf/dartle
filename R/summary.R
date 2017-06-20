#
#' Multiple plot function
#'
#' @param ... ggplot objects to plot
#' @param plotlist list of ggplot objects to plot
#' @param cols number of columns in layout
#' @param layout a matrix specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like \code{matrix(c(1,2,3,3), nrow=2, byrow=TRUE)},
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @return a ggplot object
#' @export
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




#' \code{summary_report}: prepare input
#'
#' @title summary plot
#' @references Janssen et al., 2017. "Guidance Document on Modelling Quality Objectives and Benchmarking. Version 2.1"
#' @param data a data frame including observations and model forecasts
#' @param obs name of the column with observed data
#' @param mod name of the column with forecasts
#' @param point name of the column with station ID
#' @param pollutant one of \code{"NO2"}, \code{"O3"}, \code{"PM10"}, \code{"PM2.5"}
#' @param beta parameter \eqn{\beta} (default is 2)
#' @param s_rep output of \code{summary_report}
#' @name summary_plot
#'
#' @return \code{summary_report} returns a list of 3:
#' \describe{
#'   \item{\code{summary_points}}{a data frame with 8 variables for each station \describe{
#'   \item{\code{Point}}{station ID}
#'   \item{\code{Obs.ave}}{annual mean of observations (paired with forecasts)}
#'   \item{\code{Mod.ave}}{annual mean of forecasts (paired with observations)}
#'   \item{\code{mpi_bias}}{Model Performance Indicator for the bias (see \link{MPI_bias})}
#'   \item{\code{mpi_corr_time}}{Model Performance Indicator for the correlation in time (see \link{MPI_corr_time})}
#'   \item{\code{mpi_sdev_time}}{Model Performance Indicator for the standard deviation in time (see \link{MPI_sdev_time})}
#'   \item{\code{mpi_perc}}{Model Performance Indicator for high percentile values (see \link{MPI_perc})}
#'   \item{\code{n_valid}}{no. of valid data}
#'   }}
#'   \item{\code{summary_overall}}{a data frame with 3 overall indicators \describe{
#'   \item{\code{mpi_corr_space}}{Model Performance Indicator for the correlation in space (see \link{MPI_corr_space})}
#'   \item{\code{mpi_sdev_space}}{Model Performance Indicator for the standard deviation in space (see \link{MPI_sdev_space})}
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
#' s_rep <- summary_report(Dat, obs = "Value.obs", mod = "Value.mod",
#'                        point = "Point", pollutant = "PM10")
#'
#' # plot
#' summary_plot(s_rep)
#' @rdname summary_plot
summary_report <- function(data, obs, mod, point,
                           pollutant=c("NO2", "O3", "PM10", "PM2.5"),
                           beta=2){
  pollutant <- match.arg(pollutant)
  data <- as.data.frame(data)
  Dat <- data.frame(Obs=data[,obs], Mod=data[,mod], Point=data[,point])
  Dat %>% mutate(Obs = Obs+Mod-Mod,
                 Mod = Mod+Obs-Obs) %>%
    filter(!is.na(Obs))-> Dat

  thr <- threshold(pollutant=pollutant)
  Dat %>% group_by(Point) %>%
    summarize(Obs.ave  = mean(Obs, na.rm=T),
              Mod.ave  = mean(Mod, na.rm=T),
              Obs.nexc = ifelse(is.na(thr), NA, sum(Obs>thr, na.rm=T)),
              mpi_bias       = MPI_bias      (obs=Obs, mod=Mod, pollutant=pollutant),
              mpi_corr_time  = MPI_corr_time (obs=Obs, mod=Mod, pollutant=pollutant),
              mpi_sdev_time  = MPI_sdev_time (obs=Obs, mod=Mod, pollutant=pollutant),
              mpi_perc       = MPI_perc      (obs=Obs, mod=Mod, pollutant=pollutant),
              n_valid = n()) %>%
    filter(n_valid>=365*0.75) -> summary_points
  summary_overall <- data.frame(mpi_corr_space = MPI_corr_space(obs=summary_points$Obs.ave,
                                                                mod=summary_points$Mod.ave,
                                                                pollutant=pollutant),
                                mpi_sdev_space = MPI_sdev_space(obs=summary_points$Obs.ave,
                                                                mod=summary_points$Mod.ave,
                                                                pollutant=pollutant),
                                n_points = nrow(summary_points))

  out <- list()
  out$summary_points  <- summary_points
  out$summary_overall <- summary_overall
  out$parameters <- params_U(pollutant=pollutant)
  out$parameters$pollutant <- pollutant
  out$parameters$beta <- beta
  return(out)
}


#' \code{summary_plot}: summary plot
#'
#' @import ggplot2
#' @importFrom tidyr gather
#' @return \code{summary_plot} returns a summary plot (object of class \code{ggplot} and \code{gg})
#' @rdname summary_plot
#' @export
summary_plot <- function(s_rep){

  renameIndex <- function(x) switch(as.character(x),
                                    Obs.ave="'mean\n'~(mu*g/m^3)",
                                    Obs.nexc=paste0("'exceedances\n'~",
                                                    ifelse(s_rep$parameters$pollutant=="NO2",
                                                           "(hours)","(days)")),
                                    mpi_bias="bias[norm]",
                                    mpi_corr_time="corr['norm,time']",
                                    mpi_sdev_time="st.dev.['norm,time']",
                                    mpi_perc="hi.perc.[norm]",
                                    mpi_corr_space="corr['norm,space']",
                                    mpi_sdev_space="st.dev.['norm,space']")



  p1 <- ggplot(s_rep$summary_points %>%
                 gather(key = "Indicator", value = "Value", -Point) %>%
                 filter(Indicator%in%c("Obs.ave","Obs.nexc")) %>%
                 filter(!is.na(Value)) %>%
                 mutate(Indicator=sapply(Indicator,renameIndex)),
               aes(x=Value,y=0)) +
    ggtitle(paste("Summary statistics",s_rep$parameters$pollutant),
            subtitle = "observations")

  p2<- ggplot(s_rep$summary_points %>%
                gather(key = "Indicator", value = "Value", -Point) %>%
                filter(Indicator%in%c("mpi_bias","mpi_corr_time","mpi_sdev_time","mpi_perc")) %>%
                mutate(Indicator=sapply(Indicator,renameIndex)),
              aes(x=Value,y=0)) +
    ggtitle("",subtitle = "performances in time")

  p3 <- ggplot(s_rep$summary_overall %>%
                 gather(key = "Indicator", value = "Value", -n_points) %>%
                 mutate(Indicator=sapply(Indicator,renameIndex)),
               aes(x=Value,y=0)) +
    ggtitle("",subtitle = "performances in space")

  clean_layout <- function(pp) {
    pp +
      geom_jitter(width = 0, height=0.3, col="dodgerblue4") +
      facet_grid(Indicator~., switch="y", labeller=label_parsed) +
      theme_bw() + xlab("") +
      scale_y_continuous(breaks = NULL, limits = c(-1,1)) +
      theme(axis.title=element_blank(),
            axis.text.y=element_blank(),
            strip.text.y = element_text(angle=180))
  }

  rect_mpo <- function(pp) {
    ii <- unique(pp$data$Indicator)
    r1 <- data.frame(xmin=c(-0.7,0,-0.7,-1,0,-0.7),
                     xmax=c(0.7,0.7,0.7,1,0.7,0.7),
                     ymin=-Inf,ymax=Inf,
                     Indicator=c("mpi_bias","mpi_corr_time","mpi_sdev_time","mpi_perc",
                                 "mpi_corr_space","mpi_sdev_space")) %>%
      mutate(Indicator=sapply(Indicator,renameIndex)) %>%
      filter(Indicator %in% ii)
    r2 <- data.frame(xmin=c(-1,0.7,0.7,-1,0.7,0.7,-1,0.7),
                     xmax=c(-0.7,1,1,-0.7,1,1,-0.7,1),
                     ymin=-Inf,ymax=Inf,
                     Indicator=c("mpi_bias","mpi_bias","mpi_corr_time",
                                 "mpi_sdev_time","mpi_sdev_time",
                                 "mpi_corr_space","mpi_sdev_space","mpi_sdev_space")) %>%
      mutate(Indicator=sapply(Indicator,renameIndex)) %>%
      filter(Indicator %in% ii)
    pp +
      geom_rect(data=r1 ,
                aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                fill="chartreuse3",alpha=0.4,inherit.aes = FALSE) +
      geom_rect(data=r2 ,
                aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                fill="darkorange",alpha=0.4,inherit.aes = FALSE)
  }

  rect_mpo(p2) -> p2
  rect_mpo(p3) -> p3
  clean_layout(p1) + xlim(0,max(p1$data$Value,na.rm=T)) -> p1
  clean_layout(p2) + xlim(-1.5,1.5) -> p2
  clean_layout(p3) + xlim(-1.5,1.5) -> p3

  ii <- c("mpi_bias","mpi_corr_time","mpi_sdev_time","mpi_perc")
  pmpo <- round(colMeans(abs(s_rep$summary_points[,ii])<=1, na.rm=T)*100)
  p4 <- ggplot(data.frame(x=1,y=1,
                          lab=paste0(pmpo,"%"),
                          mpo=ifelse(pmpo>=90,"ok","ko"),
                          Ind=sapply(ii,renameIndex)),
               aes(x,y,label=lab,col=mpo))

  ii <- c("mpi_corr_space","mpi_sdev_space")
  mpi <- unlist(s_rep$summary_overall[,ii,drop=F])
  p5 <- ggplot(data.frame(x=1,y=1,
                          lab=round(mpi,2),
                          mpo=ifelse(abs(mpi)<=1,"ok","ko"),
                          Ind=sapply(ii,renameIndex)),
               aes(x,y,label=lab,col=mpo))

  void_layout <- function(pp) {
    pp +
      geom_label() +
      scale_color_manual(values=c("ko"="firebrick3","ok"="green4"), guide=FALSE) +
      theme_classic() + facet_grid(Ind~.)+
      theme(strip.background = element_blank(),
          strip.text.y = element_blank())+
      ggtitle("",subtitle="MPI") +
      theme(axis.title=element_blank(),
            axis.text.y=element_blank(),
            strip.text.y = element_blank(),
            axis.text = element_text(colour = "transparent"),
            axis.line = element_line(colour = "transparent"),
            axis.ticks = element_line(colour = "transparent"))
  }

  p4 <- void_layout(p4)
  p5 <- void_layout(p5)

  nc<-7
  p <- multiplot(p1,p2,p3,p4,p5,cols=nc,
                 layout=matrix(c(rep(1,nc*(2+!is.na(threshold(s_rep$parameters$pollutant)))),
                                 rep(c(rep(2,nc-1),4),5),
                                 rep(c(rep(3,nc-1),5),3)),
                               ncol=nc, byrow = T))
  return(p)
}


