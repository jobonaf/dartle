#' Rolling Mean
#'
#' @param x numeric vector of values to be processed
#' @param time vector of type \code{POSIXct}
#' @param tstep time step (see \code{\link{seq.POSIXt}})
#' @param n rolling mean width
#' @param req minimum valid data required in the rolling mean window
#'
#' @keywords internal
#' @importFrom RcppRoll roll_meanr roll_sumr
#' @importFrom dplyr full_join
#' @export
.roll_meanr_time <- function(x, time, tstep="1 hour", n=8, req=6) {
  if(anyDuplicated(time)) stop("time must be unique")
  if(length(x)!=length(time)) stop("x and time must have the same length")
  dat <- data.frame(x=x, time=time)
  exptime <- data.frame(time=seq.POSIXt(from=min(time),
                                        to=max(time),
                                        by=tstep))
  Dat <- dplyr::full_join(exptime,dat)
  rmean <- RcppRoll::roll_meanr(Dat$x, n, na.rm=T)
  valid <- RcppRoll::roll_sumr(!is.na(Dat$x), n)
  rmean[valid<req] <- NA
  out <- data.frame(x=rmean, time=exptime)
  return(out)
}

#' \code{dMaxAvg8h}: daily maximum of 8h rolling mean
#'
#' @param data data frame
#' @param value name of variable to be processed
#' @param time column containing time (\code{POSIXct})
#' @param point column containing point ID
#' @param req minimum valid required 8h averages to compute their daily max
#' @title Daily statistics
#' @name daily_statistics
#'
#' @return \code{dMaxAvg8h}: daily maxima of 8h rolling mean, for each point
#' @rdname daily_statistics
#' @import data.table
#' @export
dMaxAvg8h <- function(data, value, time="Time", point="Point", req = 18) {
  Point=Value=Day=Time=N=NULL  # to avoid warnings from R CMD check
  DT <- data.table(Value= data[,value],
                   Time = data[,time],
                   Point= data[,point])
  DT <- DT[,list(Value=.roll_meanr_time(x = Value, time = Time, n = 8, req = 6)$x,
                 Day=format(.roll_meanr_time(x = Value, time = Time, n = 8, req = 6)$time,"%Y-%m-%d")),
           by=list(Point)]
  DT <- DT[!is.na(DT$Value),]
  DT <- DT[,list(Value=max(Value,na.rm=T),
                 N=.N),
           by=list(Point,Day)]
  DT <- subset(DT, N>=req)
  return(DT)
}

#' \code{dMean}: daily average
#'
#' @return \code{dMean}: daily averages, for each point
#' @rdname daily_statistics
#' @import data.table
#' @export
dMean <- function(data, value, time="Time", point="Point", req = 18) {
  Point=Value=Day=N=NULL  # to avoid warnings from R CMD check
  DT <- data.table(Value= data[,value],
                   Time = data[,time],
                   Day  = format(data[,time],"%Y-%m-%d"),
                   Point= data[,point])
  DT <- DT[,list(Value=mean(Value,na.rm=T),
              N=.N),
           by=list(Point,Day)]
  DT <- subset(DT, N>=req)
  return(DT)
}
