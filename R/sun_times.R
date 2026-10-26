#' Estimate sunrise and sunset times
#'
#' Estimate sunrise and sunset times for a location and date.  This is a stripped down version of \code{getSunlightTimes} from the suncalc package, which implements the SunCalc algorithm by Vladimir Agafonkin (\url{https://github.com/mourner/suncalc}).
#'
#' @param date Date vector, or a vector that can be converted with \code{\link[base]{as.Date}}
#' @param lat numeric for latitude
#' @param long numeric for longitude (negative west of prime meridian)
#' @param tz chr string for timezone of the output, e.g., 'America/Chicago'
#'
#' @details Times are estimated for a solar altitude of -0.833 degrees, which accounts for atmospheric refraction and the radius of the solar disc.  Estimates are typically within one to two minutes of those from the NOAA solar calculator for mid-latitude locations.  Times for each date are based on the solar transit nearest to noon UTC on that date.
#'
#' This function is used within \code{\link{metab_day}} to identify metabolic days for \code{\link{ecometab}}.
#'
#' @return A \code{data.frame} with columns \code{date}, \code{sunrise}, and \code{sunset}.  The latter two are \code{POSIXct} in the timezone given by \code{tz}.  Values are \code{NA} if the sun does not rise or set on a date, e.g., polar day or night.
#'
#' @export
#'
#' @seealso \code{\link{metab_day}}
#'
#' @examples
#' # Apalachicola Bay NERR, Florida
#' sun_times(as.Date('2024-06-21'), lat = 29.67, long = -85.06, tz = 'America/Jamaica')
#'
#' # a sequence of dates
#' dts <- seq(as.Date('2024-01-01'), as.Date('2024-01-07'), by = 'days')
#' sun_times(dts, lat = 29.67, long = -85.06, tz = 'America/Jamaica')
sun_times <- function(date, lat, long, tz = 'UTC'){

  if(!is.numeric(lat) || length(lat) != 1 || is.na(lat) || abs(lat) > 90)
    stop('lat must be a single numeric value between -90 and 90')
  if(!is.numeric(long) || length(long) != 1 || is.na(long) || abs(long) > 180)
    stop('long must be a single numeric value between -180 and 180')

  date <- as.Date(date)

  rad <- pi / 180
  J1970 <- 2440588
  J2000 <- 2451545
  e <- rad * 23.4397 # obliquity of the earth

  # days since J2000 at noon UTC for each date
  d <- as.numeric(as.POSIXct(paste(date, '12:00:00'), tz = 'UTC')) / 86400 - 0.5 + J1970 - J2000

  lw <- rad * -long
  phi <- rad * lat

  # solar transit
  n <- round(d - 0.0009 - lw / (2 * pi))
  ds <- 0.0009 + lw / (2 * pi) + n
  M <- rad * (357.5291 + 0.98560028 * ds)
  C <- rad * (1.9148 * sin(M) + 0.02 * sin(2 * M) + 0.0003 * sin(3 * M))
  L <- M + C + rad * 102.9372 + pi
  dec <- asin(sin(e) * sin(L))
  Jnoon <- J2000 + ds + 0.0053 * sin(M) - 0.0069 * sin(2 * L)

  # hour angle at sunset, NaN for polar day or night
  h0 <- rad * -0.833
  w <- suppressWarnings(acos((sin(h0) - sin(phi) * sin(dec)) / (cos(phi) * cos(dec))))
  a <- 0.0009 + (w + lw) / (2 * pi) + n
  Jset <- J2000 + a + 0.0053 * sin(M) - 0.0069 * sin(2 * L)
  Jrise <- Jnoon - (Jset - Jnoon)

  # julian date to POSIXct, truncated to seconds
  fromJulian <- function(j){
    secs <- floor((j + 0.5 - J1970) * 86400)
    secs[is.nan(secs)] <- NA
    as.POSIXct(secs, origin = '1970-01-01', tz = tz)
  }

  out <- data.frame(
    date = date,
    sunrise = fromJulian(Jrise),
    sunset = fromJulian(Jset)
  )

  return(out)

}
