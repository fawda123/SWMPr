#' Combine swmpr data
#' 
#' Combine swmpr data types for a station by common time series
#' 
#' @param ... input time series data objects, from one to many
#' @param date_col chr string indicating name of the date column
#' @param timestep numeric value of time step to use in minutes, passed to \code{setstep}
#' @param differ numeric value defining buffer for merging time stamps to standardized time series, passed to \code{setstep}
#' @param method chr string indicating method of combining data.  Use \code{'union'} for all dates as continuous time series or \code{'intersect'} for only areas of overlap. If input is a  \code{swmpr} object, a \code{'station'} name can be used to combine by the date range of a given station, assuming there is overlap with the second station.  A numeric value can be supplied for the default method that specifies which data object to use for the date range based on order of execution in the function call.
#' 
#' @import data.table
#' 
#' @export 
#' 
#' @concept organize
#' 
#' @return Returns a combined swmpr object
#' 
#' @seealso \code{\link{setstep}}
#' 
#' @details
#' The \code{comb} function is used to combine multiple swmpr objects into a single object with a continuous time series at a given step. The \code{timestep} function is used internally such that \code{timestep} and \code{differ} are accepted arguments for \code{comb}. 
#' 
#' The function requires one or more swmpr objects as input as separate, undefined arguments. The remaining arguments must be called explicitly since an arbitrary number of objects can be used as input. In general, the function combines data by creating a master time series that is used to iteratively merge all swmpr objects. The time series for merging depends on the value passed to the \code{method} argument. Passing \code{'union'} to \code{method} will create a time series that is continuous starting from the earliest date and the latest date for all input objects. Passing \code{'intersect'} to \code{method} will create a time series that is continuous from the set of dates that are shared between all input objects. Finally, a seven or eight character station name passed to \code{method} will merge all input objects based on a continuous time series for the given station. The specified station must be present in the input data. Currently, combining data types from different stations is not possible, excluding weather data which are typically at a single, dedicated station.
#' 
#' @examples
#' 
#' ## get wq and met data as separate objects for the same station
#' swmp1 <- apacpnut
#' swmp2 <- apaebmet
#' 
#' ## combine nuts and wq data by union, set timestep to 120 minutes
#' \dontrun{
#' comb(swmp1, swmp2, timestep = 120, method = 'union')
#' }
comb <- function(...) UseMethod('comb')

#' @rdname comb
#' 
#' @export
#' 
#' @method comb swmpr
comb.swmpr <- function(..., timestep = 15, differ= NULL, method = 'union'){
  
  # swmp objects list and attributes
  all_dat <- list(...)
  attrs <- lapply(all_dat, attributes)
  
  ##
  # sanity checks
  
  # remove qaqc if present
  qaqc_cols <- unique(unlist(lapply(attrs, function(x) x$qaqc_cols)))
  if(any(qaqc_cols)){
    warning('QAQC columns present, removed from output')
    all_dat <- lapply(all_dat, function(x) qaqc(x, qaqc_keep = NULL))
  }
  
  # stop if from more than one timezone
  timezone <- unique(unlist(lapply(attrs, function(x) x$timezone)))
  if(length(timezone) > 1)
    stop('Input data are from multiple timezones')
  
  # stop if method is invalid
  stations <- unlist(lapply(attrs, function(x) x$station))
  
  if(!method %in% c('intersect', 'union', stations))
    stop('Method must be intersect, union, or station name')
  # get index value of station name
  if(method %in% stations) method <- which(method == stations)
  
  # stop if more than one data type
  types <- unlist(lapply(attrs, function(x) substring(x$station, 6)))
  
  if(any(duplicated(types))) 
    stop('Unable to combine duplicated data types')
  
  # convert to df for default
  all_dat <- lapply(all_dat, function(x) data.frame(x))
  res <- comb(all_dat, date_col = 'datetimestamp', timestep = timestep, differ = differ, method = method)
  
  out <- swmpr(res, stations)
  
  return(out)
  
}

#' @rdname comb
#' 
#' @export
#' 
#' @method comb default
comb.default <- function(..., date_col, timestep = 15, differ= NULL, method = 'union'){
  
  ##
  # sanity checks
  
  # create list for input data if not already
  all_dat <- as.list(...)
  if(!identical(all_dat, c(...)))
    all_dat <- list(...)
  
  # stop if from more than one timezone
  timezone <- lapply(all_dat, function(x) attr(x[, date_col], 'tzone'))
  timezone <- unique(unlist(timezone))
  if(length(timezone) > 1)
    stop('Input data are from multiple timezones')
  
  # stop if incorrect input for method
  if(is.numeric(method)){
    if(method > length(all_dat)) 
      stop('numeric value for method must specify an index for the input data')
  } else {
    if(!method %in% c('intersect', 'union'))
      stop('character value for method must be intersect or union')
  }
  
  ##
  # setstep applied to data before combining
  all_dat <- lapply(all_dat, function(x) setstep(x, date_col = date_col, timestep, differ))
  
  ##
  # dates
  date_vecs <- lapply(all_dat, function(x) x[, date_col])
  
  ## 
  # date vector for combining
  # for union, intersect
  if(method %in% c('union', 'intersect')){
    
    date_vec <- Reduce(method, date_vecs)
    date_vec <- as.POSIXct(date_vec, origin = '1970-01-01', tz = timezone)
    
    # for a numeric index
  } else {
    
    date_vec <- all_dat[[method]][, date_col]
    
  }
  
  # get default differ value, as timestep/2
  # convert timestep to numeric if chr input
  # this is needed for default differ
  if(is.character(timestep) & is.null(differ)){
    
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
    differ <- mul_fac[which(timestep == chr_stp)]/2
    
  }
  
  # differ as half timestep if timestep is not a character
  if(is.null(differ)) differ <- timestep/2
  
  # sanity check
  if(timestep/2 < differ) 
    stop('Value for differ must be less than or equal to one half of timestep')
  
  ##
  # merge stations by date_vec
  out <- data.table::data.table(datetimestamp = date_vec, key = 'datetimestamp')
  
  for(dat in rev(all_dat)){ # reverse this because data are combined from back to front
    
    # set dummy time variable and parameter id for differ check
    dat$time_dum <- dat[, date_col]
    dat_parms <- names(dat)[!names(dat) %in% c('time_dum', date_col)]
    
    # merge
    dat <- data.table::data.table(dat, key = 'datetimestamp')
    out <- dat[out, roll = 'nearest']
    
    # set values outside of differ to NA
    time_diff <- abs(difftime(out$datetimestamp, out$time_dum, units='secs'))
    time_diff <- time_diff >= 60 * differ
    out <- data.frame(out)
    out[time_diff, names(out) %in% dat_parms] <- NA
    out$time_dum <- NULL
    out <- data.table::data.table(out, key = 'datetimestamp')
    
  }
  
  # format output, return
  out <- data.frame(out)
  names(out)[names(out) %in% 'datetimestamp'] <- date_col
  
  return(out)
  
}