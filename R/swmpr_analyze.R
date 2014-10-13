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
#' @method aggregate swmpr
#' 
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
      
      to_agg$datetimestamp <- base::as.Date(to_agg$datetimestamp,
        tz = timezone)
      
    } else {
      
      to_agg$datetimestamp <- round(
        as.IDate(to_agg$datetimestamp, tz = timezone),
        digits = by
      )
      
      to_agg$datetimestamp <- base::as.Date(to_agg$datetimestamp, tz = timezone)
      
    }
   
  }
  
  # subset by parameters
  if(!is.null(params)) parameters <- parameters[parameters %in% params] 
  to_agg <- to_agg[, c('datetimestamp', parameters)]
  
  # aggregate
  form_in <- formula(. ~ datetimestamp)
  out <- aggregate(form_in, to_agg, FUN = FUN, na.action = na.action,
    simplify = T)

  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

######
#' Smooth swmpr data
#' 
#' Smooth swmpr data with a moving window average
#' 
#' @param swmpr_in input swmpr object
#' @param ... arguments passed to other methods
#' 
#' @export smoother
#' 
#' @return Returns a filtered swmpr object. QAQC columns are removed if included with input object.
smoother <- function(swmpr_in, ...) UseMethod('smoother') 

#' @rdname smoother
#' 
#' @param window numeric vector of ones defining size of smoothing window, passed to \code{filter} 
#' @param sides numeric vector defining method of averaging, passed to \code{filter}
#' @param params is chr string of swmpr parameters to smooth, default all
#' 
#' @method smoother swmpr
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
  if(!is.null(params)) parameters <- parameters[parameters %in% params]
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

######
#' Linearly interpolate gaps in swmpr data
#' 
#' @param swmpr_in input swmpr object
#' @param params is chr string of swmpr parameters to smooth, default all
#' @param maxgap numeric vector indicating maximum gap size to interpolate where size is numer of records, must be explicit
#' @param na.rm logical. If the result of the interpolation includes NAs, should these be removed?
#' 
#' @import plyr zoo
#' 
#' @export na.approx
#' @method na.approx swmpr
#' 
#' @return Returns a swmpr object. QAQC columns are removed if included with input object.
na.approx.swmpr <- function(swmpr_in, params = NULL, maxgap, 
  na.rm = F, ...){
  
  # attributes
  parameters <- attr(swmpr_in, 'parameters')
  station <- attr(swmpr_in, 'station')

  # sanity checks
  if(!any(params %in% parameters) & !is.null(params))
    stop('Params argument must name input columns')
  if(attr(swmpr_in, 'qaqc_cols'))
    warning('QAQC columns present, removed in output')
  
  # prep for interpolate
  if(!is.null(params)) parameters <- parameters[parameters %in% params]
  to_interp <- swmpr_in$station_data[, c('datetimestamp', parameters), 
    drop = F]
  datetimestamp <- to_interp$datetimestamp
  to_interp$datetimestamp <- NULL
  
  # interpolate column-wise
  out <- mlply(matrix(to_interp),
    .fun = function(in_col){
      
      interp <- try(zoo::na.approx(in_col, maxgap = maxgap, 
        na.rm = F), silent = T)
      
      if('try-error' %in% class(interp)) interp  <- in_col
      
      return(interp)
      
    })
  
  # format output as data frame
  out <- do.call('cbind', out)
  out <- data.frame(datetimestamp, out)
  names(out) <- c('datetimestamp', parameters)
  
  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

######
#' Plot swmpr data
#' 
#' Plot a time series of parameters in a swmpr object
#' 
#' @param swmpr_in input swmpr object
#' @param subset chr string of form 'YYYY-mm-dd HH:MM' to subset a date range.  Input can be one (requires \code{operator} or two values (a range), passed to \code{\link{subset}}.
#' @param select chr string of parameters to keep, passed to \code{\link{subset}}.
#' @param operator chr string specifiying binary operator (e.g., \code{'>'}, \code{'<='}) if subset is one date value, passed to \code{\link{subset}}.
#' @param type chr string for type of plot, default \code{'l'}.  See \code{\link[graphics]{plot}}.
#' @param ... other arguments passed to \code{par}, \code{plot.default}, \code{lines}, \code{points}
#' 
#' @method plot swmpr
plot.swmpr <- function(swmpr_in, type = 'l', subset = NULL, select, operator = NULL, ...) {
  
  if(length(select) > 1) stop('Only one parameter  can be plotted')
  
  to_plo <- subset(swmpr_in, subset, select, operator)
  parameters <- attr(to_plo, 'parameters')

  to_plo <- swmpr_in$station_data

  form_in <- formula(substitute(i ~ datetimestamp, 
    list(i = as.name(parameters))))
  plot(form_in, data = to_plo, type = type, ...)
   
}

######
#' @rdname plot.swmpr
#' 
#' @method lines swmpr
lines.swmpr <- function(swmpr_in, subset = NULL, select, operator = NULL, ...) {
    
  to_plo <- subset(swmpr_in, subset, select, operator)
  parameters <- attr(to_plo, 'parameters')

  to_plo <- swmpr_in$station_data
  form_in <- formula(substitute(i ~ datetimestamp, 
    list(i = as.name(parameters))))
  lines(form_in, data = to_plo, ...)
     
}