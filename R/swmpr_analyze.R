######
#' Aggregate swmpr data by specified time period and method
#' 
#' @param swmpr_in input swmpr object
#' @param by chr string of time period for aggregation one of \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'}
#' @param FUN aggregation function, default \code{mean}
#' @param params names of parameters to aggregate, default all
#' @param na.action function for treating missing data, default \code{na.pass}
#' 
#' @import data.table
#' 
#' @export
#' @return Returns an aggregated swmpr object. QAQC columns are removed if included with input object.
aggregate.swmpr <- function(swmpr_in, by, FUN = mean, params = NULL, 
  na.action = na.pass, ...){

  # data
  to_agg <- swmpr_in$station_data

  # attributes
  timezone <- attr(swmpr_in, 'timezone')
  parameters <- attr(swmpr_in, 'parameters')
  station  <- attr(swmpr_in, 'station')
  
  # sanity checks
  if(any(!params %in% parameters))
    stop('Aggregation parameters must be present in data')
  if(attr(swmpr_in, 'qaqc_cols'))
    warning('QAQC columns present, removed in output')
  if(!by %in% c('years', 'quarters', 'months', 'weeks', 'days', 'hours'))
    stop('Unknown value for by, see help documentation')
    
  # create agg values from datetimestamp
  # as posix if hours, as date if other
  if(by == 'hours'){
    
    to_agg$datetimestamp <- as.POSIXct(
      strftime(to_agg$datetimestamp, '%Y-%m-%d %H', 
        tz = timezone), format = '%Y-%m-%d %H',
      tz = timezone)

  } else {
    
    if(by == 'days'){
      
      to_agg$datetimestamp <- as.Date(to_agg$datetimestamp,
        tz = timezone)
      
    } else {
      
      to_agg$datetimestamp <- round(
        as.IDate(to_agg$datetimestamp, tz = timezone),
        digits = by
      )
      
      to_agg$datetimestamp <- as.Date(to_agg$datetimestamp, tz = timezone)
      
    }
   
  }
  
  # subset by parameters
  if(!is.null(params)){ 
    to_agg <- to_agg[, names(to_agg) %in% c('datetimestamp', params)]
  } else {
    to_agg <- to_agg[, names(to_agg) %in% c('datetimestamp', parameters)]
  }

  # aggregate
  form_in <- formula(. ~ datetimestamp)
  out <- aggregate(form_in, to_agg, FUN = FUN, na.action = na.action,
    simplify = F)

  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

######
#' Smooth swmpr data with a moving window average
#' 
#' @param swmpr_in input swmpr object
#' @param window numeric vector of ones defining size of smoothing window, passed to \code{filter} 
#' @param sides numeric vector defining method of averaging, passed to \code{filter}
#' @param params is chr string of swmpr parameters to smooth, default all
#' 
#' @export
#' @return Returns a filtered swmpr object. QAQC columns are removed if included with input object.
smoother <- function(x, ...) UseMethod('smoother') 
smoother.swmpr <- function(swmpr_in, window = 5, sides = 2, params = NULL){
  
  # attributes
  parameters <- attr(swmpr_in, 'parameters')
  station <- attr(swmpr_in, 'station')
  
  # sanity checks
  if(!any(params %in% parameters) & !is.null(params))
    stop('Params argument must name input columns')
  if(attr(swmpr_in, 'qaqc_cols'))
    warning('QAQC columns present, removed in output')

  # prep for filter
  if(!is.null(params)) parameters <- params
  to_filt <- swmpr_in$station_data[, c('datetimestamp', parameters), drop = F]
  datetimestamp <- to_filt$datetimestamp
  to_filt$datetimestamp <- NULL
  
  # filter
  window <- rep(1, window)/window
  out <- filter(to_filt, window, sides, method = 'convolution')
  out <- data.frame(datetimestamp, out)
  names(out) <- c('datetimestamp', parameters)
  
  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)

}