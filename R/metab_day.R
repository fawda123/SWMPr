#' Identify metabolic days in a time series
#'
#' Identify metabolic days in a time series based on sunrise and sunset times for a location and date.  The metabolic day is considered the 24 hour period between sunsets for two adjacent calendar days.  The function calls the \code{\link[maptools]{sunriset}} function from the maptools package, which uses algorithms from the National Oceanic and Atmospheric Administration (\url{http://www.esrl.noaa.gov/gmd/grad/solcalc/}).
#' 
#' @param dat_in data.frame
#' @param tz chr string for timezone, e.g., 'America/Chicago'
#' @param lat numeric for latitude
#' @param long numeric for longitude (negative west of prime meridian)
#' @param ... arguments passed to or from other methods
#' 
#' @import maptools
#' 
#' @export 
#' 
#' @details This function is only used within \code{\link{ecometab}} and should not be called explicitly.
#' 
#' @seealso 
#' \code{\link{ecometab}}, \code{\link[maptools]{sunriset}}
#' 
metab_day <- function(dat_in, ...) UseMethod('metab_day')

#' @rdname metab_day
#' 
#' @export
#' 
#' @method metab_day default
metab_day.default <- function(dat_in, tz, lat, long, ...){
  
  dtrng <- range(as.Date(dat_in$datetimestamp), na.rm = TRUE)
  start_day <- dtrng[1] - 1
  end_day <- dtrng[2] + 1
  lat.long <- matrix(c(long, lat), nrow = 1)
  sequence <- seq(
    from = as.POSIXct(start_day, tz = tz), 
    to = as.POSIXct(end_day, tz = tz),
    by = "days"
  )
  sunrise <- sunriset(lat.long, sequence, direction = "sunrise", 
                      POSIXct = TRUE)
  sunset <- sunriset(lat.long, sequence, direction = "sunset", 
                     POSIXct = TRUE)
  ss_dat <- data.frame(sunrise, sunset)
  ss_dat <- ss_dat[, -c(1, 3)]
  colnames(ss_dat) <- c("sunrise", "sunset")
  
  # remove duplicates, if any
  ss_dat <- ss_dat[!duplicated(strftime(ss_dat[, 1], format = '%Y-%m_%d')), ]
  ss_dat <- data.frame(
    ss_dat,
    metab_date = as.Date(ss_dat$sunrise, tz = tz)
  )
  ss_dat <- reshape2::melt(ss_dat, id.vars = 'metab_date')
  if(!"POSIXct" %in% class(ss_dat$value))
    ss_dat$value <- as.POSIXct(ss_dat$value, origin='1970-01-01', tz = tz)
  ss_dat <- ss_dat[order(ss_dat$value),]
  ss_dat$day_hrs <- unlist(lapply(
    split(ss_dat, ss_dat$metab_date),
    function(x) rep(as.numeric(x[2, 'value'] - x[1, 'value']), 2) 
  ))
  names(ss_dat)[names(ss_dat) %in% c('variable', 'value')] <- c('solar_period', 'solar_time')
  
  # matches is vector of row numbers indicating starting value that each
  # unique datetimestamp is within in ss_dat
  # output is meteorological day matches appended to dat_in
  matches <- findInterval(dat_in$datetimestamp, ss_dat$solar_time)
  out <- data.frame(dat_in, ss_dat[matches, ])
  row.names(out) <- 1:nrow(out)
  return(out)
  
}