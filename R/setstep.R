#' Format a swmpr time vector
#'
#' Create a continuous time vector at set time step for a swmpr object
#' 
#' @param dat_in input data object
#' @param date_col chr string for the name of the date/time column, e.g., \code{"POSIXct"} or \code{"POSIXlt"} objects
#' @param timestep numeric value of time step to use in minutes.  Alternatively, a chr string indicating \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'days'}, or \code{'hours'} can also be used. A character input assumes 365 days in a year and 31 days in a month.
#' @param differ numeric value defining buffer for merging time stamps to standardized time series, defaults to one half of the timestep
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @concept organize
#' 
#' @import data.table
#' 
#' @return Returns a data object for the specified time step
#' 
#' @seealso \code{\link{comb}}
#' 
#' @details
#' The setstep function formats a swmpr object to a continuous time series at a given time step. This function is not necessary for most stations but can be useful for combining data or converting an existing time series to a set interval.  The first argument of the function, \code{timestep}, specifies the desired time step in minutes starting from the nearest hour of the first observation. The second argument, \code{differ}, specifies the allowable tolerance in minutes for matching existing observations to user-defined time steps in cases where the two are dissimilar. Values for \code{differ} that are greater than one half the value of timestep are not allowed to prevent duplication of existing data. Likewise, the default value for differ is one half the time step. Rows that do not match any existing data within the limits of the differ argument are not discarded. Output from the function can be used with \code{subset} and to create a time series at a set interval with empty data removed.
#' 
#' @examples
#' ## import data
#' data(apaebmet)
#' dat <- apaebmet
#' 
#' ## convert time series to two hour invervals
#' ## tolerance of +/- 30 minutes for matching existing data
#' setstep(dat, timestep = 120, differ = 30)
#' 
#' ## convert a nutrient time series to a continuous time series
#' ## then remove empty rows and columns
#' data(apacpnut)
#' dat_nut <- apacpnut
#' dat_nut <- setstep(dat_nut, timestep = 60)
#' subset(dat_nut, rem_rows = TRUE, rem_cols = TRUE)
setstep <- function(dat_in, ...) UseMethod('setstep')

#' @rdname setstep
#' 
#' @export
#' 
#' @method setstep swmpr
setstep.swmpr <- function(dat_in, timestep = 15, differ= NULL, ...){ 
  
  # swmpr data and attributes
  attrs <- attributes(dat_in)
  
  # run default method
  dat_in <- as.data.frame(dat_in)
  out <- setstep(dat_in, date_col = 'datetimestamp', timestep = timestep, differ = differ, ...)
  
  # back to swmpr class and exit
  out <- swmpr(out, attrs$station)
  return(out)
  
} 

#' @rdname setstep
#' 
#' @export
#' 
#' @method setstep default
setstep.default <- function(dat_in, date_col, timestep = 15, differ= NULL, ...){ 
  
  # convert timestep to numeric if chr input
  if(is.character(timestep)){
    
    # lookup values
    chr_stp <- c('years', 'quarters', 'months', 'weeks', 'days', 'hours')
    mul_fac <- c(525600, 131400, 44640, 10080, 1440, 60)
    
    # stop if chr_stp is wrong
    if(!timestep %in% chr_stp){
      
      stop(paste(
        'Character input for timestep must be one of of the following:', 
        paste(chr_stp, collapse = ', ')
      ))
      
    }
    
    # otherwise lookup
    timestep <- mul_fac[which(timestep == chr_stp)] 
    
  }
  
  if(is.null(differ)) differ <- timestep/2
  
  # sanity check
  if(timestep/2 < differ) 
    stop('Value for differ must be less than or equal to one half of timestep')
  if('Date' %in% class(dat_in[, date_col])) 
    stop('Cannot use setstep with date class')
  
  # date range
  date_rng <- range(dat_in[, date_col], na.rm = TRUE)
  timezone <- attr(dat_in[, date_col], 'tzone')
  
  # round to nearest timestep
  dts_std <- as.POSIXct(
    round(as.double(date_rng)/(timestep * 60)) * (timestep * 60),
    origin = '1970-01-01',
    tz = timezone
  )
  
  # create continuous vector
  dts_std <- seq(dts_std[1], dts_std[2], by = timestep * 60)
  dts_std <- data.frame(dts_std)
  names(dts_std) <- date_col
  
  # convert swmpr data and standardized vector to data.table for merge
  # time_dum is vector of original times for removing outside of differ
  mrg_dat <- dat_in
  mrg_dat$time_dum <- mrg_dat[, date_col]
  mrg_dat <- data.table::data.table(mrg_dat, key = date_col)
  mrg_std <- data.table::data.table(dts_std, key = date_col)
  
  # merge all the data  using  mrg_std as master
  mrg <- mrg_dat[mrg_std, roll = 'nearest']
  mrg <- data.frame(mrg)
  
  # set values outside of differ to NA
  time_diff <- abs(difftime(mrg[, date_col], mrg$time_dum, units='secs'))
  time_diff <- time_diff >= 60 * differ
  mrg[time_diff, !names(mrg) %in% c(date_col, 'time_dum')] <- NA
  
  # output
  out <- data.frame(mrg)
  out <- out[, !names(out) %in% 'time_dum']
  
  return(out)
  
} 