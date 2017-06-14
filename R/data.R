#' Observed PM10 concentrations
#'
#' A dataset containing daily concentrations of PM10 measured at background sites in the
#' region of Friuli Venezia Giulia (Italy)
#'
#' @format A data frame with 9504 rows and 8 variables:
#' \describe{
#'   \item{Time}{day (POSIXct format)}
#'   \item{ID}{station code}
#'   \item{Station}{station name}
#'   \item{Value}{concentration (in \eqn{\mu g/m^3}{\mug/m3})}
#'   \item{ZoneType}{\code{"URB"} (urban), \code{"SBR"} (suburban) or \code{"RUR"} (rural)}
#'   \item{StationType}{always \code{"BKG"} (background)}
#'   \item{Lat}{latitude}
#'   \item{Lon}{longitude}
#' }
#' @source \url{http://www.arpa.fvg.it/}
"obs.pm10"

#' Observed PM2.5 concentrations
#'
#' A dataset containing daily concentrations of PM2.5 measured at background sites in the
#' region of Friuli Venezia Giulia (Italy)
#'
#' @format A data frame with 1056 rows and 8 variables:
#' \describe{
#'   \item{Time}{day (POSIXct format)}
#'   \item{ID}{station code}
#'   \item{Station}{station name}
#'   \item{Value}{concentration (in \eqn{\mu g/m^3}{\mug/m3})}
#'   \item{ZoneType}{\code{"URB"} (urban), \code{"SBR"} (suburban) or \code{"RUR"} (rural)}
#'   \item{StationType}{always \code{"BKG"} (background)}
#'   \item{Lat}{latitude}
#'   \item{Lon}{longitude}
#' }
#' @source \url{http://www.arpa.fvg.it/}
"obs.pm25"

#' Observed NO2 concentrations
#'
#' A dataset containing hourly concentrations of NO2 measured at background sites in the
#' region of Friuli Venezia Giulia (Italy)
#'
#' @format A data frame with 202752 rows and 8 variables:
#' \describe{
#'   \item{Time}{time (POSIXct format)}
#'   \item{ID}{station code}
#'   \item{Station}{station name}
#'   \item{Value}{concentration (in \eqn{\mu g/m^3}{\mug/m3})}
#'   \item{ZoneType}{\code{"URB"} (urban), \code{"SBR"} (suburban) or \code{"RUR"} (rural)}
#'   \item{StationType}{always \code{"BKG"} (background)}
#'   \item{Lat}{latitude}
#'   \item{Lon}{longitude}
#' }
#' @source \url{http://www.arpa.fvg.it/}
"obs.no2"

#' Observed ozone concentrations
#'
#' A dataset containing hourly concentrations of ozone measured at background sites in the
#' region of Friuli Venezia Giulia (Italy)
#'
#' @format A data frame with 215424 rows and 8 variables:
#' \describe{
#'   \item{Time}{time (POSIXct format)}
#'   \item{ID}{station code}
#'   \item{Station}{station name}
#'   \item{Value}{concentration (in \eqn{\mu g/m^3}{\mug/m3})}
#'   \item{ZoneType}{\code{"URB"} (urban), \code{"SBR"} (suburban) or \code{"RUR"} (rural)}
#'   \item{StationType}{always \code{"BKG"} (background)}
#'   \item{Lat}{latitude}
#'   \item{Lon}{longitude}
#' }
#' @source \url{http://www.arpa.fvg.it/}
"obs.o3"

#' Forecasted concentrations
#'
#' A dataset containing hourly concentrations of PM10, PM2.5, NO2 and ozone forecasted at
#' background sites in the region of Friuli Venezia Giulia (Italy)
#'
#' @format A data frame with 658944 rows and 4 variables:
#' \describe{
#'   \item{Time}{time in POSIXct format}
#'   \item{Var}{\code{"c_PM10"} (PM10), \code{"c_PM25"} (PM2.5), \code{"c_NO2"} (NO2) or \code{"c_O3"} (ozone)}
#'   \item{Point}{station code}
#'   \item{Value}{concentration (in \eqn{\mu g/m^3}{\mug/m3})}
#' }
#' @source \url{http://www.arpa.fvg.it/}
"mod.data"

