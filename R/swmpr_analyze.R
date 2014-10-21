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
#' @seealso \code{\link[stats]{aggregate}}
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
#' @details The \code{smoother} function can be used to smooth parameters in a swmpr object using a specified window size. This method is a simple wrapper to \code{\link[stats]{filter}}. The window argument specifies the number of observations included in the moving average. The sides argument specifies how the average is calculated for each observation (see the documentation for \code{\link[stats]{filter}}). A value of 1 will filter observations within the window that are previous to the current observation, whereas a value of 2 will filter all observations within the window centered at zero lag from the current observation. The params argument specifies which parameters to smooth.
#' 
#' @seealso \code{\link[stats]{filter}}
#' 
#' @method smoother swmpr
#' 
#' @examples
#' ## import data
#' path <- system.file('zip_ex', package = 'SWMPr')
#' swmp1 <- import_local(path, 'apadbwq')
#' 
#' ## qaqc and subset imported data
#' dat <- qaqc(swmp1)
#' dat <- subset(dat, subset = c('2012-07-09 00:00', '2012-07-24 00:00'))
#' 
#' ## filter
#' test <- smoother(dat, window = 50, params = 'do_mgl')
#' 
#' ## plot to see the difference
#' plot(do_mgl ~ datetimestamp, data = dat, type = 'l')
#' lines(test, select = 'do_mgl', col = 'red', lwd = 2)
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

#' Linearly interpolate gaps
#' 
#' Linearly interpolate gaps in swmpr data within a maximum size 
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
#' @details A common approach for handling missing data in time series analysis is linear interpolation.  A simple curve fitting method is used to create a continuous set of records between observations separated by missing data.  A required argument for the function is \code{maxgap} which defines the maximum gap size  for interpolation. The ability of the interpolated data to approximate actual, unobserved trends is a function of the gap size.  Interpolation between larger gaps are less likely to resemble patterns of an actual parameter, whereas interpolation between smaller gaps may be more likely to resemble actual patterns.  An appropriate gap size limit depends on the unique characteristics of specific datasets or parameters.  
#' 
#' @seealso \code{\link[zoo]{na.approx}}
#' 
#' @return Returns a swmpr object. QAQC columns are removed if included with input object.
#' 
#' @examples
#' ## import data
#' path <- system.file('zip_ex', package = 'SWMPr')
#' swmp1 <- import_local(path, 'apadbwq')
#' 
#' ## qaqc and subset imported data
#' dat <- qaqc(swmp1)
#' dat <- subset(dat, subset = c('2013-01-22 00:00', '2013-01-26 00:00'))
#' 
#' ## interpolate, maxgap of 10 records
#' test <- na.approx(dat, params = 'do_mgl', maxgap = 10)
#' 
#' ## interpolate maxgap of 30 records
#' test2 <- na.approx(dat, params = 'do_mgl', maxgap = 30)
#' 
#' ## plot for comparison
#' windows(width = 6, height = 9)
#' par(mfrow = c(3,1))
#' 
#' plot(do_mgl ~ datetimestamp, dat, main = 'Raw', type = 'l')
#' 
#' plot(do_mgl ~ datetimestamp, test, col = 'red', main = 'Inteprolation - maximum gap of 10 records', type = 'l')
#' lines(dat, select = 'do_mgl')
#' 
#' plot(do_mgl ~ datetimestamp, test2, col = 'red', main = 'Interpolation - maximum gap of 30 records', type = 'l')
#' lines(dat, select = 'do_mgl')
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
#' Decompose swmpr data into trend, seasonal, and random components using \code{\link[stats]{decompose}} and \code{\link[stats]{ts}}
#' 
#' @param swmpr_in input swmpr object
#' @param ... arguments passed to \code{decompose}, \code{ts}, and other methods
#' 
#' @export decomp
#' 
#' @details
#' This function is a simple wrapper to the \code{\link[stats]{decompose}} function.  The \code{decompose} function separates a time series into additive or multiplicative components describing a trend, cyclical variation (e.g., daily or seasonal), and the remainder.  The additive decomposition assumes that the cyclical component of the time series is stationary (i.e., the variance is constant), whereas a multiplicative decomposition accounts for non-stationarity.  By default, a moving average with a symmetric window is used to filter the seasonal component.  Alternatively, a vector of filter coefficients in reverse time order can be supplied (see \code{\link[stats]{decompose}}).  
#' 
#' The \code{decompose} function requires a ts object with a specified frequency.  The \code{decomp} function converts the input swmpr vector to a ts object prior to \code{decompose}.  This requires an explicit input defining the frequency in the time series required to complete a full period of the parameter.  For example, the frequency of a parameter with diurnal periodicity would be 96 if the time step is 15 minutes (4 * 24).  The frequency of a parameter with seasonal periodicity would be 35040 (4 * 24 * 365).  For simplicity, chr strings of \code{'daily'} or \code{'seasonal'} can be supplied in place of numeric values.  A starting value of the time series must be supplied in the latter case.  Use of the \code{\link{setstep}} function is required to standardize the time step prior to decomposition.  
#' 
#' Note that the \code{decompose} function is a relatively simple approach and alternative methods should be investigated if a more sophisticated decomposition is desired.
#'  
#' @references
#' M. Kendall and A. Stuart (1983) The Advanced Theory of Statistics, Vol. 3, Griffin. pp. 410-414.
#' 
#' @seealso \code{\link[stats]{decompose}}, \code{\link[stats]{ts}}, \code{\link[stats]{stl}}
#' 
#' @return Returns a decomposed.ts object
#' 
#' @examples
#' 
#' path <- system.file('zip_ex', package = 'SWMPr')
#'
#' ## get data, qaqc
#' swmp1 <- import_local(path, 'apadbwq')
#'
#' ## subset for daily decomposition
#' dat <- subset(swmp1, subset = c('2013-07-01 00:00', '2013-07-31 00:00'))
#'
#' ## decomposition and plot
#' test <- decomp(dat, param = 'do_mgl', frequency = 'daily')
#' plot(test)
#' 
#' ## dealing with missing values
#' dat <- subset(swmp1, subset = c('2013-06-01 00:00', '2013-07-31 00:00'))
#' 
#' ## this returns an error
#' \dontrun{
#' test <- decomp(dat, param = 'do_mgl', frequency = 'daily')
#' }
#'
#' ## how many missing values?
#' sum(is.na(dat$do_mgl))
#'
#' ## use na.approx to interpolate missing data
#' dat <- na.approx(dat, params = 'do_mgl', maxgap = 10)
#'
#' ## decomposition and plot
#' test <- decomp(dat, param = 'do_mgl', frequency = 'daily')
#' plot(test)
decomp <- function(swmpr_in, ...) UseMethod('decomp') 

#' @rdname decomp
#' 
#' @param param chr string of swmpr parameter to decompose
#' @param type chr string of \code{'additive'} or \code{'multiplicative'} indicating the type of decomposition, default \code{'additive'}.
#' @param frequency chr string or numeric vector indicating the periodic component of the input parameter.  Only \code{'daily'} or \code{'seasonal'} are accepted as chr strings.  Otherwise a numeric vector specifies the number of observations required for a full cycle of the input parameter.  Defaults to \code{'daily'} for a diurnal parameter.
#' @param start numeric vector indicating the starting value for the time series given the frequency.  Only required if \code{frequency} is numeric. See \code{\link[stats]{ts}}.
#' 
#' @export decomp.swmpr
#' 
#' @method decomp swmpr
decomp.swmpr <- function(swmpr_in, param, type = 'additive', frequency = 'daily', start = NULL, ...){
  
  # attributes
  parameters <- attr(swmpr_in, 'parameters')
  timezone <- attr(swmpr_in, 'timezone')
  
  ##
  # sanity checks
  
  # stop if param not in parameters
  if(!any(param %in% parameters) & !is.null(param))
    stop('Params argument must name input columns')
  
  # stop if frequency or start are incorrect
  if(!is.numeric(frequency) & !any(frequency %in% c('daily', 'seasonal'))){
    stop("Chr string input for frequency must be 'daily' or 'seasonal'")
  } else {
    if(!is.null(start))
      stop('Start argument required if frequency is numeric')
  }
 
  # stop if time series is not standardized
  chk_step <- unique(diff(swmpr_in$datetimestamp))
  if(length(chk_step) > 1)
    stop('The time step is not standardized, use setstep')

  ##
  # get frequency and starting value if input not numeric
  start <- swmpr_in$datetimestamp[1]
  day <- as.numeric(strftime(start, '%j', tz = timezone))
  hour <- as.numeric(strftime(start, '%H', tz = timezone))
  min <- as.numeric(strftime(start, '%M', tz = timezone))
  if(frequency == 'daily'){
    frequency  <- 24 * 60 / chk_step
    start <- 1 + (hour + min / 60) * 60 / chk_step
  }
  if(frequency == 'seasonal'){
    frequency <- 365 * 24 * 60 / chk_step 
    start <- (day + hour / 24 + min / 60 / 24) * 24 * 60 / chk_step
  }
  
  # make ts and decompose
  ts_smp <- ts(swmpr_in[, param], start = c(1, start), frequency = frequency)
  out <- decompose(ts_smp, type, ...)

  # return decompose.ts
  return(out)

}

#' Plot swmpr data
#' 
#' Plot a time series of parameters in a swmpr object
#' 
#' @param swmpr_in input swmpr object
#' @param type chr string for type of plot, default \code{'l'}.  See \code{\link[graphics]{plot}}.
#' @param ... other arguments passed to \code{par}, \code{plot.default}, \code{lines}, \code{points}
#' 
#' @export plot.swmpr
#' 
#' @details The swmpr method for plotting is a convenience function for plotting a univariate time series.  Conventional plotting methods also work well since swmpr objects are also data frames.  See the examples for use with different methods.  
#' 
#' @method plot swmpr
#' 
#' @seealso \code{\link[graphics]{plot}}
#' 
#' @examples
#' ## get data
#' path <- system.file('zip_ex', package = 'SWMPr')
#' swmp1 <- import_local(path, 'apadbwq')
#' 
#' ## subset
#' dat <- subset(swmp1, select = 'do_mgl', subset = c('2013-07-01 00:00', '2013-07-31 00:00'))
#'
#' ## plot using swmpr method, note default line plot
#' plot(dat)
#' 
#' ## plot using formula method
#' plot(do_mgl ~ datetimestamp, dat)
#' 
#' ## plot using defualt, add lines
#' plot(dat$datetimestamp, dat$do_mgl)
#' lines(dat, select = 'do_mgl', col = 'red')
plot.swmpr <- function(swmpr_in, type = 'l', ...) {
  
  to_plo <- swmpr_in
  
  if(attr(to_plo, 'qaqc'))
    to_plo <- qaqc(to_plo, qaqc_keep = NULL)
  
  if(ncol(to_plo) > 2) stop('Only one parameter can be plotted')
  
  parameters <- attr(to_plo, 'parameters')

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
#' @details The swmpr method for histograms is a convenience function for the default histogram function.  Conventional histogram methods also work well since swmpr objects are also data frames.  See the examples for use with different methods.  
#' 
#' @export hist.swmpr
#' 
#' @method hist swmpr
#' 
#' @seealso \code{\link[graphics]{hist}}
#' 
#' @examples
#' ## get data
#' path <- system.file('zip_ex', package = 'SWMPr')
#' dat <- import_local(path, 'apadbwq')
#'
#' ## plot using swmpr method, note default line plot
#' hist(dat, select = 'do_mgl')
#' 
#' ## plot using defualt method
#' hist(dat$do_mgl)
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
