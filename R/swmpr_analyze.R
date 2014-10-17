#' Aggregate swmpr data
#' 
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
#' @export aggregate.swmpr
#' 
#' @method aggregate swmpr
#' 
#' @details The \code{aggregate} function summarizes or condenses parameter data for a swmpr object by set periods of observation and a user-supplied function. It is most useful for aggregating noisy data to evaluate trends on longer time scales, or to simply reduce the size of a dataset. Data can be aggregated by \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'} for the supplied function, which defaults to the \code{\link[base]{mean}}. A swmpr object is returned for the aggregated data, although the datetimestamp vector will be converted to a date object if the aggregation period is a day or longer. Days are assigned to the date vector if the aggregation period is a week or longer based on the round method for \code{\link[data.table]{IDate}} objects. This approach was used to facilitate plotting using predefined methods for Date and POSIX objects.
#' 
#' The method of treating NA values for the user-supplied function should be noted since this may greatly affect the quantity of data that are returned (see the examples). Finally, the default argument for \code{na.action} is set to \code{na.pass} for swmpr objects to preserve the time series of the input data.
#' 
#' @return Returns an aggregated swmpr object. QAQC columns are removed if included with input object.
#' 
#' @examples
#' ## get data, prep
#' path <- system.file('zip_ex', package = 'SWMPr')
#' dat <- import_local(path, 'apacpwq')
#' swmpr_in <- subset(qaqc(dat), rem_cols = T)
#'
#' ## get mean DO by quarters
#' aggregate(swmpr_in, 'quarters', params = c('do_mgl'))
#'  
#' ## get mean DO by quarters, remove NA when calculating means
#' fun_in <- function(x) mean(x, na.rm = T)
#' aggregate(swmpr_in, FUN = fun_in, 'quarters', params = c('do_mgl'))
#'
#' ## get variance of DO by years, remove NA when calculating variance
#' ## omit NA data in output
#' fun_in <- function(x)  var(x, na.rm = T)
#' aggregate(swmpr_in, FUN = fun_in, 'years', na.action = na.exclude) 
aggregate.swmpr <- function(swmpr_in, by, FUN = mean, params = NULL, na.action = na.pass, ...){
  
  # data
  to_agg <- swmpr_in

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
  out <- aggregate(form_in, data.frame(to_agg), FUN = FUN, 
    na.action = na.action, simplify = T)

  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

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
#' @export smoother.swmpr
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
  to_filt <- swmpr_in[, c('datetimestamp', parameters), drop = F]
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
#' @export na.approx.swmpr
#' 
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
  to_interp <- swmpr_in[, c('datetimestamp', parameters), 
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

#' Seasonal trend decomposition of swmpr data
#' 
#' Decompose swmpr data into seasonal, trend, and irregular components using \code{\link[stats]{stl}} and \code{\link[stats]{loess}}
#' 
#' @param swmpr_in input swmpr object
#' @param ... arguments passed to \code{stl} and other methods
#' 
#' @export decomp
#' 
#' @details
#' This function is a simple wrapper to the \code{\link[stats]{stl}} function written by Brian Ripley.  In general, \code{stl} decomposes a time series using \code{\link[stats]{loess}} smoothing into separate seasonal, trend, and remainder components. All arguments for the \code{stl} function are applicable, although each have default values excluding s.window.  All methods available for stl objects also apply to the output, including \code{plot.stl}.  
#'  
#' @references
#' R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3–73.
#' 
#' @return Returns an stl object.
decomp <- function(swmpr_in, ...) UseMethod('decomp') 

#' @rdname decomp
#' 
#' @param param chr string of swmpr parameter to decompose
#' @param s.window either the chrr string \code{"periodic"} or the span (in lags) of the loess window for seasonal extraction, which should be odd.
#' 
#' @export decomp.swmpr
#' 
#' @method decomp swmpr
decomp.swmpr <- function(swmpr_in, param, s.window, ...){
  
  # attributes
  parameters <- attr(swmpr_in, 'parameters')
  
  # sanity checks
  if(!any(param %in% parameters) & !is.null(param))
    stop('Params argument must name input columns')

  # decomp
  ts_smp <- as.ts(swmpr_in[, param])
  out <- stl(ts_smp, s.window, ...)

  # return stl object
  return(out)

}

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
#' @export plot.swmpr
#' 
#' @method plot swmpr
plot.swmpr <- function(swmpr_in, type = 'l', subset = NULL, select, operator = NULL, ...) {
  
  if(length(select) > 1) stop('Only one parameter can be plotted')
  
  to_plo <- subset(swmpr_in, subset, select, operator)
  parameters <- attr(to_plo, 'parameters')

  to_plo <- swmpr_in

  form_in <- formula(substitute(i ~ datetimestamp, 
    list(i = as.name(parameters))))
  plot(form_in, data = to_plo, type = type, ...)
   
}

#' @rdname plot.swmpr
#' 
#' @export lines.swmpr
#' 
#' @method lines swmpr
lines.swmpr <- function(swmpr_in, subset = NULL, select, operator = NULL, ...) {
    
  to_plo <- subset(swmpr_in, subset, select, operator)
  parameters <- attr(to_plo, 'parameters')

  to_plo <- swmpr_in
  form_in <- formula(substitute(i ~ datetimestamp, 
    list(i = as.name(parameters))))
  lines(form_in, data = to_plo, ...)
     
}

#' Plot swmpr using a histogram
#' 
#' Plot a histogram showing the distribution of a swmpr parameter
#' 
#' @param swmpr_in input swmpr object
#' @param subset chr string of form 'YYYY-mm-dd HH:MM' to subset a date range.  Input can be one (requires \code{operator} or two values (a range), passed to \code{\link{subset}}.
#' @param select chr string of parameters to keep, passed to \code{\link{subset}}.
#' @param operator chr string specifiying binary operator (e.g., \code{'>'}, \code{'<='}) if subset is one date value, passed to \code{\link{subset}}.
#' @param ... other arguments passed to \code{\link[graphics]{histogram}}
#' 
#' @export hist.swmpr
#' 
#' @method hist swmpr
hist.swmpr <- function(swmpr_in, subset = NULL, select, operator = NULL, ...) {
  
  if(length(select) > 1) stop('Only one parameter can be plotted')
  
  to_plo <- subset(swmpr_in, subset, select, operator)
  parameters <- attr(to_plo, 'parameters')

  to_plo <- swmpr_in
  
  # for correct default of xlab, main
  assign(select, to_plo[, select])

  eval(substitute(
    hist(i, ...), 
    list(i = as.name(select))
    ))
  
}
