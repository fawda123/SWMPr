#' Aggregate swmpr data
#' 
#' Aggregate swmpr data by specified time period and method
#' 
#' @param x input swmpr object
#' @param by chr string of time period for aggregation one of \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'}
#' @param FUN aggregation function, default \code{mean} with \code{na.rm = TRUE}
#' @param params names of parameters to aggregate, default all
#' @param aggs_out logical indicating if \code{\link[base]{data.frame}} is returned of raw data with datetimestamp formatted as aggregation period, default \code{FALSE}
#' @param na.action function for treating missing data, default \code{na.pass}
#' @param ... additional arguments passed to other methods
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
#' data(apacpwq)
#' dat <- apacpwq
#' swmpr_in <- subset(qaqc(dat), rem_cols = TRUE)
#'
#' ## get mean DO by quarters
#' aggregate(swmpr_in, 'quarters', params = c('do_mgl'))
#'
#' ## get variance of DO by years, remove NA when calculating variance
#' ## omit NA data in output
#' fun_in <- function(x)  var(x, na.rm = TRUE)
#' aggregate(swmpr_in, FUN = fun_in, 'years') 
aggregate.swmpr <- function(x, by, FUN = function(x) mean(x, na.rm = TRUE), params = NULL, aggs_out = FALSE, na.action = na.pass, ...){
  
  # data
  swmpr_in <- x
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
        data.table::as.IDate(to_agg$datetimestamp, tz = timezone),
        digits = by
      )
      
      to_agg$datetimestamp <- base::as.Date(to_agg$datetimestamp, tz = timezone)
      
    }
   
  }
  
  # subset by parameters
  if(!is.null(params)) parameters <- parameters[parameters %in% params] 
  to_agg <- to_agg[, c('datetimestamp', parameters)]
  
  # return raw aggregations if true
  if(aggs_out) return(to_agg)
  
  # aggregate
  form_in <- formula(. ~ datetimestamp)
  out <- suppressWarnings(aggregate(form_in, data.frame(to_agg), FUN = FUN, 
    na.action = na.action, simplify = TRUE, ...))

  # convert columns to numeric, missing converted to NA
  datetimestamp <- out[, 1]
  nr <- nrow(out)
  nc <- ncol(out) -1
  out <- c(as.matrix(out[, -1]))
  out[is.nan(out)] <- NA
  out[out %in%  c(-Inf, Inf)] <- NA
  out <- matrix(out, nrow = nr, ncol = nc) 
  out <- data.frame(
    datetimestamp = datetimestamp,
    out
    )
  names(out) <- c('datetimestamp', parameters)
  
  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}

#' Aggregate metabolism data
#' 
#' Aggregate a metabolism attribute from swmpr data by a specified time period and method
#' 
#' @param swmpr_in input swmpr object
#' @param by chr string of time period for aggregation one of \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'}
#' @param na.action function for treating missing data, default \code{na.pass}
#' @param alpha numeric indicating alpha level of confidence interval for aggregated data
#' @param ... additional arguments passed to other methods
#' 
#' @import data.table
#' 
#' @export
#' 
#' @details The function summarizes metabolism data by averaging across set periods of observation. Confidence intervals are also returned based on the specified alpha level.  It is used within \code{\link{plot_metab}} function to view summarized metabolism results.  Data can be aggregated by \code{'years'}, \code{'quarters'}, \code{'months'}, or \code{'weeks'} for the supplied function, which defaults to the \code{\link[base]{mean}}. The method of treating NA values for the user-supplied function should be noted since this may greatly affect the quantity of data that are returned.
#' 
#' @return Returns an aggregated metabolism \code{\link[base]{data.frame}} if the \code{metabolism} attribute of the swmpr object is not \code{NULL}.
#' 
#' @seealso \code{\link[stats]{aggregate}}, \code{\link{aggregate.swmpr}}, \code{\link{ecometab}}, \code{\link{plot_metab}}
#' 
#' @examples
#' ## import water quality and weather data
#' data(apadbwq)
#' data(apaebmet)
#' 
#' ## qaqc, combine
#' wq <- qaqc(apadbwq)
#' met <- qaqc(apaebmet)
#' dat <- comb(wq, met)
#' 
#' ## estimate metabolism
#' res <- ecometab(dat, trace = TRUE)
#' 
#' ## aggregate metabolism
#' aggregate_metab(res, by = 'weeks')
#' 
#' ## change aggregatin period and alpha
#' aggregate_metab(res, by = 'months', alpha = 0.1)
aggregate_metab <- function(swmpr_in, ...) UseMethod('aggregate_metab')

#' @rdname aggregate_metab
#'
#' @export
#'
#' @method aggregate_metab swmpr
aggregate_metab.swmpr <- function(swmpr_in, by = 'weeks', na.action = na.pass, alpha = 0.05, ...){
  
  # attributes
  timezone <- attr(swmpr_in, 'timezone')
  metabolism <- attr(swmpr_in, 'metabolism')
  
  # sanity checks
  if(is.null(metabolism)) 
    stop('No metabolism data, use the ecometab function')
  if(!by %in% c('years', 'quarters', 'months', 'weeks', 'days'))
    stop('Unknown value for by, see help documentation')
    
  # data
  to_agg <- metabolism
  to_agg <- to_agg[, names(to_agg) %in% c('date', 'Pg', 'Rt', 'NEM')]

  # create agg values from date
  if(by != 'days'){
    to_agg$date <- round(
      data.table::as.IDate(to_agg$date, tz = timezone),
      digits = by
    )
    to_agg$date <- base::as.Date(to_agg$date, tz = timezone)
  }

  # long-form
  to_agg <- tidyr::gather(to_agg, Estimate, Value, -date)
  to_agg$Estimate <- as.character(to_agg$Estimate)
  
  # aggregate
  sum_fun <- function(x, alpha_in = alpha, out){
      x <- na.omit(x)
      means <- mean(x)
      margs <- suppressWarnings(
        qt(1 - alpha_in/2, length(x) - 1) * sd(x)/sqrt(length(x))
      )
      upper <- means + margs
      lower <- means - margs
      return(get(out))
    }
  aggs <- dplyr::group_by(to_agg, date, Estimate)
  aggs <- dplyr::summarize(aggs,
    means = sum_fun(Value, out = 'means'),
    lower = sum_fun(Value, out = 'lower'),
    upper = sum_fun(Value, out = 'upper')
  )
    
  # return output
  return(aggs)
  
}

#' Smooth swmpr data
#' 
#' Smooth swmpr data with a moving window average
#' 
#' @param swmpr_in input swmpr object
#' @param ... arguments passed to or from other methods
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
#' data(apadbwq)
#' swmp1 <- apadbwq
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
smoother.swmpr <- function(swmpr_in, window = 5, sides = 2, params = NULL, ...){
  
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
  to_filt <- swmpr_in[, c('datetimestamp', parameters), drop = FALSE]
  datetimestamp <- to_filt$datetimestamp
  to_filt$datetimestamp <- NULL
  
  # filter
  window <- rep(1, window)/window
  out <- stats::filter(to_filt, window, sides, method = 'convolution')
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
#' @param object input swmpr object
#' @param params is chr string of swmpr parameters to smooth, default all
#' @param maxgap numeric vector indicating maximum gap size to interpolate where size is numer of records, must be explicit
#' @param ... additional arguments passed to other methods
#' 
#' @import zoo
#'
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
#' data(apadbwq)
#' swmp1 <- apadbwq
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
#' par(mfrow = c(3,1))
#' 
#' plot(do_mgl ~ datetimestamp, dat, main = 'Raw', type = 'l')
#' 
#' plot(do_mgl ~ datetimestamp, test, col = 'red', 
#'  main = 'Inteprolation - maximum gap of 10 records', 
#'  type = 'l')
#' lines(dat, select = 'do_mgl')
#' 
#' plot(do_mgl ~ datetimestamp, test2, col = 'red', 
#'  main = 'Interpolation - maximum gap of 30 records', 
#'  type = 'l')
#' lines(dat, select = 'do_mgl')
na.approx.swmpr <- function(object, params = NULL, maxgap, ...){
  
  swmpr_in <- object
  
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
    drop = FALSE]
  datetimestamp <- to_interp$datetimestamp
  to_interp$datetimestamp <- NULL
  
  # interpolate column-wise
  out <- lapply(c(to_interp),
    FUN = function(in_col){
      
      interp <- try(zoo::na.approx(in_col, maxgap = maxgap, 
        na.rm = FALSE), silent = TRUE, ...)
      
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

#' Simple trend decomposition of swmpr data
#' 
#' Decompose swmpr data into trend, cyclical (e.g., daily, seaonal), and random components using \code{\link[stats]{decompose}} and \code{\link[stats]{ts}}
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
#' ## get data
#' data(apadbwq)
#' swmp1 <- apadbwq
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

#' Simple trend decomposition of monthly swmpr data
#' 
#' Decompose monthly SWMP time series into grandmean, annual, seasonal, and event series using \code{\link[wq]{decompTs}}, as described in Cloern and Jassby 2010.
#' 
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to decompose
#' @param vals_out logical indicating of numeric output is returned, default is \code{FALSE} to return a plot.
#' @param ... additional arguments passed to other methods, including \code{\link[wq]{decompTs}} 
#' 
#' @return  
#' A \code{\link[ggplot2]{ggplot}} object if \code{vals_out = TRUE} (default), otherwise a monthly time series matrix of class \code{\link[stats]{ts}}.
#' 
#' @details
#' This function is a simple wrapper to the \code{\link[wq]{decompTs}} function in the wq package, also described in Cloern and Jassby (2010).  The function is similar to \code{\link{decomp.swmpr}} (which is a wrapper to \code{\link[stats]{decompose}}) with a few key differences.  The \code{\link{decomp.swmpr}} function decomposes the time series into a trend, seasonal, and random components, whereas the current function decomposes into the grandmean, annual, seasonal, and events components.  For both functions, the random or events components, respectively, can be considered anomalies that don't follow the trends in the remaining categories.  
#' 
#' The \code{decomp_cj} function provides only a monthly decomposition, which is appropriate for characterizing relatively long-term trends.  This approach is meant for nutrient data that are obtained on a monthly cycle.  The function will also work with continuous water quality or weather data but note that the data are first aggregated on the monthly scale before decomposition.  Accordingly, short-term variation less than one-month will be removed.  The \code{\link{decomp.swmpr}} function can be used to decompose daily variation.     
#' 
#' Additional arguments passed to \code{\link[wq]{decompTs}} can be used with \code{decomp_cj}, such as \code{startyr}, \code{endyr}, and \code{type}.  Values passed to \code{type} are \code{mult} (default) or \code{add}, referring to multiplicative or additive decomposition.  See the documentation for \code{\link[wq]{decompTs}} for additional explanation and examples.   
#' 
#' @export
#' 
#' @import ggplot2 reshape2 wq
#' 
#' @seealso \code{\link[wq]{decompTs}}, \code{\link[stats]{ts}}
#' 
#' @references
#' Cloern, J.E., Jassby, A.D. 2010. Patterns and scales of phytoplankton variability in estuarine-coastal ecosystems. Estuaries and Coasts. 33:230-241.
#' 
#' @examples
#' ## get data
#' data(apacpnut)
#' dat <- apacpnut
#' dat <- qaqc(dat, qaqc_keep = NULL)
#' 
#' ## decomposition of chl, values as data.frame
#' decomp_cj(dat, param = 'chla_n', vals_out = TRUE)
#' 
#' ## decomposition of chl, ggplot
#' decomp_cj(dat, param = 'chla_n')
#' 
#' ## decomposition changing argumens passed to decompTs
#' decomp_cj(dat, param = 'chla_n', startyr = 2008, type = 'add')
#' 
#' ## monthly decomposition of continuous data
#' data(apacpwq)
#' dat2 <- qaqc(apacpwq)
#' 
#' decomp_cj(dat2, param = 'do_mgl')
decomp_cj <- function(swmpr_in, ...) UseMethod('decomp_cj') 

#' @rdname decomp_cj
#' 
#' @export decomp_cj.swmpr
#' 
#' @method decomp_cj swmpr
decomp_cj.swmpr <- function(swmpr_in, param, vals_out = FALSE, ...){
  
  dat <- swmpr_in
  
  ## sanity checks
  parameters <- attr(dat, 'parameters')
  if(!param %in% parameters) stop('Selected parameter not in data')
  
  # monthly ts
  dat <- aggregate(dat, by = 'months', params = param)
  dat_rng <- attr(dat, 'date_rng')
  months <- data.frame(datetimestamp = seq.Date(dat_rng[1], dat_rng[2], by = 'months'))
  dat <- merge(months, dat,  by = 'datetimestamp', all.x = T)
  
  year <- as.numeric(strftime(dat_rng[1], '%Y'))
  month <- as.numeric(strftime(dat_rng[1], '%m'))
  dat_mts <- ts(dat[, param], frequency = 12, start = c(year, month))
  
  # decomp
  out <- wq::decompTs(dat_mts, ...)
  
  # convert results to data frame
  Time <- unique(as.numeric(gsub('[A-z]| ', '', capture.output(out[, 0])[-1])))
  Time <- expand.grid(seq(1, 12), Time)
  Time <- paste(Time[, 2], Time[, 1], '01', sep = '-')
  Time <- as.Date(Time, format = '%Y-%m-%d')
  out <- data.frame(Time, out)
  
  # output, ts matrix if TRUE
  if(vals_out) return(out)
  
  # otherwise, ggplot
  to_plo <- out
  to_plo <- reshape2::melt(to_plo, id.var = 'Time')
  plo <- ggplot(to_plo, aes(x = Time, y = value, group = variable)) +
    geom_line() +
    facet_wrap(~variable, ncol = 1, scales = 'free_y') + 
    theme_bw()
  
  return(plo)
  
}

#' Plot swmpr data
#' 
#' Plot a time series of parameters in a swmpr object
#' 
#' @param x input swmpr object
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
#' data(apadbwq)
#' swmp1 <- apadbwq
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
plot.swmpr <- function(x, type = 'l', ...) {
  
  swmpr_in <- x
  
  if(attr(swmpr_in, 'qaqc_cols'))
    swmpr_in <- qaqc(swmpr_in, qaqc_keep = NULL)
  
  if(ncol(swmpr_in) > 2) stop('Only one parameter can be plotted, use subset first')
  
  parameters <- attr(swmpr_in, 'parameters')

  form_in <- formula(substitute(i ~ datetimestamp, 
    list(i = as.name(parameters))))
  plot(form_in, data = swmpr_in, type = type, ...)
   
}

#' @rdname plot.swmpr
#' 
#' @export lines.swmpr
#' 
#' @param subset chr string of form 'YYYY-mm-dd HH:MM' to subset a date range.  Input can be one (requires \code{operator} or two values (a range).  Passed to \code{\link{subset.swmpr}}.
#' @param select chr string of parameters to keep. Passed to \code{\link{subset.swmpr}}.
#' @param operator chr string specifiying binary operator (e.g., \code{'>'}, \code{'<='}) if subset is one date value. Passed to \code{\link{subset.swmpr}}.
#' 
#' @method lines swmpr
lines.swmpr <- function(x, subset = NULL, select, operator = NULL, ...) {
    
  swmpr_in <- x
  
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
#' @param x input swmpr object
#' @param subset chr string of form 'YYYY-mm-dd HH:MM' to subset a date range.  Input can be one (requires \code{operator} or two values (a range), passed to \code{\link{subset}}.
#' @param select chr string of parameters to keep, passed to \code{\link{subset}}.
#' @param operator chr string specifiying binary operator (e.g., \code{'>'}, \code{'<='}) if subset is one date value, passed to \code{\link{subset}}.
#' @param ... other arguments passed to \code{\link[graphics]{hist}}
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
#' data(apadbwq)
#' dat <- apadbwq
#'
#' ## plot using swmpr method, note default line plot
#' hist(dat, select = 'do_mgl')
#' 
#' ## plot using defualt method
#' hist(dat$do_mgl)
hist.swmpr <- function(x, subset = NULL, select, operator = NULL, ...) {
  
  if(length(select) > 1) stop('Only one parameter can be plotted')
  
  swmpr_in <- x
  
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

#' Plot graphical summaries of SWMP data
#' 
#' Plot graphical summaries of SWMP data for individual parameters, including seasonal/annual trends and anomalies
#' 
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param years numeric vector of starting and ending years to plot, default all
#' @param ... additional arguments passed to other methods, currently not used
#' 
#' @import ggplot2 gridExtra
#' 
#' @export
#' 
#' @details This function creates several graphics showing seasonal and annual trends for a given swmp parameter.  Plots include monthly distributions, monthly anomalies, and annual anomalies in multiple formats.  Anomalies are defined as the difference between the monthly or annual average from the grand mean.  Monthly anomalies are in relation to the grand mean for the same month across all years.  All data are aggregated for quicker plotting.  Nutrient data are based on monthly averages, wheras weather and water quality data are based on daily averages.  Cumulative precipitation data are based on the daily maximum.  An interactive Shiny widget is available: \url{https://beckmw.shinyapps.io/swmp_summary/}
#' 
#' @return A graphics object (Grob) of multiple \code{\link[ggplot2]{ggplot}} objects.
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}
#' 
#' @examples
#' ## import data
#' data(apacpnut)
#' dat <- qaqc(apacpnut)
#' 
#' ## plot
#' plot_summary(dat, param = 'chla_n', years = c(2007, 2013))
#' 
plot_summary <- function(swmpr_in, ...) UseMethod('plot_summary') 

#' @rdname plot_summary
#' 
#' @export plot_summary.swmpr
#' 
#' @method plot_summary swmpr
plot_summary.swmpr <- function(swmpr_in, param, years = NULL, ...){
  
  stat <- attr(swmpr_in, 'station')
  parameters <- attr(swmpr_in, 'parameters')
  date_rng <- attr(swmpr_in, 'date_rng')
  
  # sanity checks
  if(is.null(years)){
    years <- as.numeric(as.character(strftime(date_rng, '%Y')))
  } else {
    if(length(years) > 2) stop('One or two element year vector is required.')
    if(length(years) == 1) years <- c(years, years)
  }
  if(!param %in% parameters) stop('param must be included in the data')
  
  ##
  # preprocessing
  
  ## aggregate by averages for quicker plots
  # nuts are monthly
  if(grepl('nut$', stat)){
    dat <- aggregate.swmpr(swmpr_in, by = 'months', params = param)
  }
  
  # wq is monthly
  if(grepl('wq$', stat)){
    dat <- aggregate.swmpr(swmpr_in, by = 'days', params = param)
  }
  
  # met is monthly, except cumprcp which is daily max
  if(grepl('met$', stat)){
    dat <- aggregate.swmpr(swmpr_in, by = 'days', params = param)
    cumprcp <- aggregate(swmpr_in, by = 'days', FUN = function(x) max(x, na.rm = T), 
      params = 'cumprcp')
    dat$cumprcp <- cumprcp$cumprcp
  }
  
  mo_labs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  mo_levs <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  dat$year <- strftime(dat$datetimestamp, '%Y')
  dat$month <- strftime(dat$datetimestamp, '%m')
  dat$month <- factor(dat$month, labels = mo_levs, levels = mo_levs)
  
  # select years to plot
  dat_plo <- data.frame(dat[dat$year %in% seq(years[1], years[2]), ])
  
  # label lookups
  lab_look <- list(
    temp = 'Temperature (C)', 
    spcond = 'Specific conductivity (mS/cm)',
    sal = 'Salinity (psu)',
    do_pct = 'Dissolved oxyxgen (%)',
    do_mgl = 'Dissolved oxygen (mg/L)',
    depth = 'Depth (m)',
    cdepth = 'Depth (nonvented, m)',
    level = 'Referenced depth (m)',
    clevel = 'Referenced depth (nonvented, m)',
    ph = 'pH',
    turb = 'Turbidity (NTU)',
    chlfluor = 'Chl fluorescence (ug/L)',
    atemp = 'Air temperature (C)',
    rh = 'Relative humidity (%)',
    bp = 'Barometric pressure (mb)',
    wspd = 'Wind speed (m/s)',
    maxwspd = 'Max wind speed (m/s)',
    wdir = 'Wind direction (degrees)',
    sdwdir = 'Wind direction (sd, degrees)',
    totpar = 'Total PAR (mmol/m2)',
    totprcp = 'Total precipitation (mm)',
    cumprcp = 'Cumulative precipitation (mm)',
    totsorad = 'Total solar radiation (watts/m2)',
    po4f = 'Orthophosphate (mg/L)', 
    nh4f = 'Ammonium (mg/L)',
    no2f = 'Nitrite (mg/L)',
    no3f = 'Nitrate (mg/L)',
    no23f = 'Nitrite + Nitrate (mg/L)',
    chla_n = 'Chlorophyll (ug/L)'
  )
  ylab <- lab_look[[param]]
  
  # monthly, annual aggs
  agg_fun <- function(x) mean(x, na.rm = T)
  form_in <- formula(paste0(param, ' ~ month'))
  mo_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = agg_fun)
  mo_agg_med <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = function(x) median(x, na.rm = T))
  form_in <- formula(paste0(param, ' ~ year'))
  yr_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'month')], FUN = agg_fun, na.action = na.pass)
  
  ##
  # plots
  
  # universal plot setting
  my_theme <- theme(axis.text = element_text(size = 8))
  
  # browser()
  
  # plot 1 - means and obs
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg))
  cols <- cols[rank(mo_agg[, param])]
  p1 <- ggplot(dat_plo, aes_string(x = 'month', y = param)) +
    geom_point(size = 2, alpha = 0.5, 
      position=position_jitter(width=0.1)
      ) +
    theme_classic() +
    ylab(ylab) + 
    xlab('Monthly distributions and means') +
    geom_point(data = mo_agg, aes_string(x = 'month', y = param), 
      colour = 'darkgreen', fill = cols, size = 7, pch = 21) + 
    my_theme
  
  # box aggs, colored by median
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg_med))
  cols <- cols[rank(mo_agg_med[, param])]
  p2 <- ggplot(dat_plo, aes_string(x = 'month', y = param)) + 
    geom_boxplot(fill = cols) +
    theme_classic() +
    ylab(ylab) + 
    xlab('Monthly distributions and medians') +
    my_theme
  
  # month histograms
  to_plo <- dat_plo
  to_plo$month <- factor(to_plo$month, levels = rev(mo_levs), labels = rev(mo_labs))
  p3 <- ggplot(to_plo, aes_string(x = param)) + 
    geom_histogram(aes(y = ..density..), colour = 'lightblue', binwidth = diff(range(to_plo[, param], na.rm = T))/30) + 
    facet_grid(month ~ .) + 
    xlab(ylab) +
    theme_bw(base_family = 'Times') + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      strip.text.y = element_text(size = 8, angle = 90),
      strip.background = element_rect(size = 0, fill = 'lightblue')) +
    my_theme
  
  # monthly means by year
  to_plo <- dat_plo[, names(dat_plo) %in% c('month', 'year', param)]
  form_in <- as.formula(paste(param, '~ .'))
  to_plo <- aggregate(form_in, to_plo, function(x) mean(x, na.rm = T),
    na.action = na.pass)
  
  to_plo$month <- factor(to_plo$month, labels = mo_labs, levels = mo_levs)
  names(to_plo)[names(to_plo) %in% param] <- 'V1'
  midpt <- mean(to_plo$V1, na.rm = T)
  p4 <- ggplot(subset(to_plo, !is.na(V1)), 
      aes(x = year, y = month, fill = V1)) +
    geom_tile() +
    geom_tile(data = subset(to_plo, is.na(V1)), 
      aes(x = year, y = month), fill = NA
      )  +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = midpt) +
    theme_classic() +
    ylab('Monthly means') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight = 0.5)) +
    my_theme
  
  # monthly anomalies
  mo_agg$month <- factor(mo_agg$month, labels = mo_labs, levels = mo_levs)
  to_plo <- merge(to_plo, mo_agg, by = 'month', all.x = T)
  names(to_plo)[names(to_plo) %in% param] <- 'trend'
  to_plo$anom <- with(to_plo, V1 - trend)
  rngs <- max(abs(range(to_plo$anom, na.rm = T)))
  p5 <- ggplot(subset(to_plo, !is.na(anom)), 
      aes(x = year, y = month, fill = anom)) +
    geom_tile() +
    geom_tile(data = subset(to_plo, is.na(anom)), 
      aes(x = year, y = month), fill = NA
      ) +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
      limits = c(-1 * rngs, rngs)) +
    theme_classic() +
    ylab('Monthly anomalies') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight= 0.5)) +
    my_theme
  
  # annual anomalies
  yr_avg <- mean(yr_agg[, param], na.rm = T)
  yr_agg$anom <- yr_agg[, param] - yr_avg
  p6 <- ggplot(yr_agg, aes(x = year, y = anom, group = 1, fill = anom)) +
    geom_bar(stat = 'identity') +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0
      ) +
    stat_smooth(method = 'lm', se = F, linetype = 'dashed', size = 1) +
    theme_classic() +
    ylab('Annual anomalies') +
    xlab('') +
    theme(legend.position = 'none') +
    my_theme

  ##
  # combine plots
  suppressWarnings(gridExtra::grid.arrange(
    arrangeGrob(p1, p2, ncol = 1), 
    p3, 
    arrangeGrob(p4, p5, p6, ncol = 1, heights = c(1, 1, 0.8)), 
    ncol = 3, widths = c(1, 0.5, 1)
  ))

}

######
#' Ecosystem metabolism
#' 
#' Estimate ecosystem metabolism using the Odum open-water method.  Estimates of daily integrated gross production, total respiration, and net ecosystem metabolism are returned.
#' 
#' @param swmpr_in Input swmpr object which must include time series of dissolved oxygen (mg L-1) 
#' @param depth_val numeric value for station depth (m) if time series is not available
#' @param metab_units chr indicating desired units of output for oxygen, either as mmol or grams
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to other methods
#' 
#' @return The original \code{\link{swmpr}} object is returned that includes a metabolism attribute as a \code{\link[base]{data.frame}} of daily integrated metabolism estimates.  See the examples for retrieval.  
#' \describe{
#'  \item{\code{date}}{The metabolic day, defined as the 24 hour period starting at sunrise (calculated using \code{\link{metab_day}})}
#'  \item{\code{DOF_d}}{Mean DO flux during day hours, mmol m-2 hr-1. Day hours are calculated using the \code{\link{metab_day}} function.}
#'  \item{\code{D_d}}{Mean air-sea gas exchange of DO during day hours, mmol m-2 hr-1}
#'  \item{\code{DOF_n}}{Mean DO flux during night hours, mmol m-2 hr-1}
#'  \item{\code{D_n}}{Mean air-sea gas exchange of DO during night hours, mmol m-2 hr-1}
#'  \item{\code{Pg}}{Gross production, mmol m-2 d-1, calculated as ((DOF_d - D_d) - (DOF_n - D_n)) * day hours}
#'  \item{\code{Rt}}{Total respiration, mmol m-2 d-1, calculated as (DOF_n - D_n) * 24}
#'  \item{\code{NEM}}{Net ecosytem metabolism, mmol m-2 d-1, calculated as Pg + Rt}
#' }
#' 
#' @import oce reshape2 wq
#' 
#' @export
#'
#' @details 
#' Input data include both water quality and weather time series, which are typically collected with independent instrument systems.  This requires merging of the time series datasets using the \code{\link{comb}} function after creating separate swmpr objects.
#' 
#' The open-water method is a common approach to quantify net ecosystem metabolism using a mass balance equation that describes the change in dissolved oxygen over time from the balance between photosynthetic and respiration processes, corrected using an empirically constrained air-sea gas diffusion model (see Ro and Hunt 2006, Thebault et al. 2008).  The diffusion-corrected DO flux estimates are averaged separately over each day and night of the time series. The nighttime average DO flux is used to estimate respiration rates, while the daytime DO flux is used to estimate net primary production. To generate daily integrated rates, respiration rates are assumed constant such that hourly night time DO flux rates are multiplied by 24. Similarly, the daytime DO flux rates are multiplied by the number of daylight hours, which varies with location and time of year, to yield net daytime primary production. Respiration rates are subtracted from daily net production estimates to yield gross production rates.  The metabolic day is considered the 24 hour period between sunsets on two adjacent calendar days.
#' 
#' Areal rates for gross production and total respiration are based on volumetric rates normalized to the depth of the water column at the sampling location, which is assumed to be well-mixed, such that the DO sensor is reflecting the integrated processes in the entire water column (including the benthos).  Water column depth is calculated as the mean value of the depth variable across the time series in the \code{\link{swmpr}} object.  Depth values are floored at one meter for very shallow stations and 0.5 meters is also added to reflect the practice of placing sensors slightly off of the bottom.  Additionally, the air-sea gas exchange model is calibrated with wind data either collected at, or adjusted to, wind speed at 10 m above the surface. The metadata should be consulted for exact height.  The value can be changed manually using a \code{height} argument, which is passed to \code{\link{calckl}}.
#' 
#' A minimum of three records are required for both day and night periods to calculate daily metabolism estimates.  Occasional missing values for air temperature, barometric pressure, and wind speed are replaced with the climatological means (hourly means by month) for the period of record using adjacent data within the same month as the missing data.
#' 
#' All DO calculations within the function are done using molar units (e.g., mmol O2 m-3).  The output can be returned as mass units by setting \code{metab_units = 'grams'} (i.e., 1mol = 32 g O2, which multiplies all estimates by 32 g mol-1/1000 mg/g).  Input data must be in standard mass units for DO (mg L-1).
#' 
#' The specific approach for estimating metabolism with the open-water method is described in Caffrey et al. 2013 and references cited therein.
#' 
#' @references 
#' Caffrey JM, Murrell MC, Amacker KS, Harper J, Phipps S, Woodrey M. 2013. Seasonal and inter-annual patterns in primary production, respiration and net ecosystem metabolism in 3 estuaries in the northeast Gulf of Mexico. Estuaries and Coasts. 37(1):222-241.
#' 
#' Odum HT. 1956. Primary production in flowing waters. Limnology and Oceanography. 1(2):102-117.
#' 
#' Ro KS, Hunt PG. 2006. A new unified equation for wind-driven surficial oxygen transfer into stationary water bodies. Transactions of the American Society of Agricultural and Biological Engineers. 49(5):1615-1622.
#' 
#' Thebault J, Schraga TS, Cloern JE, Dunlavey EG. 2008. Primary production and carrying capacity of former salt ponds after reconnection to San Francisco Bay. Wetlands. 28(3):841-851.
#' 
#' @seealso 
#' \code{\link{calckl}} for estimating the oxygen mass transfer coefficient used with the air-sea gas exchange model, \code{\link{comb}} for combining \code{swmpr} objects, \code{\link{metab_day}} for identifying the metabolic day for each observation in the time series, and \code{\link{plot_metab}} for plotting the results
#' 
#' @examples
#' ## import water quality and weather data
#' data(apadbwq)
#' data(apaebmet)
#' 
#' ## qaqc, combine
#' wq <- qaqc(apadbwq)
#' met <- qaqc(apaebmet)
#' dat <- comb(wq, met)
#' 
#' ## estimate metabolism
#' res <- ecometab(dat, trace = TRUE)
#' 
#' ## change height (m) of weather station anemometer
#' res <- ecometab(dat, trace = TRUE, height = 5)
#' res <- attr(res, 'metabolism')
#' 
#' ## output units in grams of oxygen
#' res <- ecometab(dat, trace = TRUE, metab_units = 'grams')
#' res <- attr(res, 'metabolism')
ecometab <- function(swmpr_in, ...) UseMethod('ecometab')

#' @rdname ecometab
#' 
#' @export
#' 
#' @method ecometab swmpr
ecometab.swmpr <- function(swmpr_in, depth_val = NULL, metab_units = 'mmol', trace = FALSE, ...){
  
  stat <- attr(swmpr_in, 'station')
  
  # stop if units not mmol or grams
  if(any(!(grepl('mmol|grams', metab_units))))
    stop('Units must be mmol or grams')

  # stop if input data does not include wq and met
  if(sum(grepl('wq$|met$', stat)) != 2)
    stop('Requires water quality and weather data')
  stat <- grep('wq$', stat, value = T)
  
  # start timer
  if(trace){
    tictoc::tic()
    cat(paste0('Estimating ecosystem metabolism for ', stat, '...\n\n'))
  }
  
  # set all timesteps to one hour
  swmpr_in <- setstep(swmpr_in, timestep = 60)
  
  # columns to keep
  dat <- data.frame(swmpr_in)
  to_keep <- c('datetimestamp', 'do_mgl', 'depth', 'atemp', 'sal', 'temp', 
    'wspd', 'bp')
  dat <- dat[,names(dat) %in% to_keep]
  
  # all vals as numeric
  dat[, 2:ncol(dat)] <- apply(
      dat[, 2:ncol(dat), drop = FALSE],
      2,
      function(x) suppressWarnings(as.numeric(x))
      )
  
  #convert do from mg/L to mmol/m3
  dat$do <- dat[, 'do_mgl'] / 32 * 1000
  
  # get change in do per hour, as mmol m^-3 hr^-1
  ddo <- diff(dat$do)
  
  # take diff of each column, divide by 2, add original value
  datetimestamp <- diff(dat$datetimestamp)/2 + dat$datetimestamp[-c(nrow(dat))]
  dat <- apply(
    dat[,2:ncol(dat), drop = FALSE],
    2,
    function(x) diff(x)/2 + x[1:(length(x) -1)]
    )
  dat <- data.frame(datetimestamp, dat)
  do <- dat$do
  
  ##
  # replace missing wx values with climatological means
  # only atemp, wspd, and bp
  
  # monthly and hourly averages
  months <- as.character(format(dat$datetimestamp, '%m'))
  hours <- as.character(format(dat$datetimestamp, '%H'))
  clim_means <-dplyr:: mutate(dat, months = months, hours = hours) 
  clim_means <- dplyr::select(clim_means, months, hours, atemp, wspd, bp)
  clim_means <- dplyr::group_by(clim_means, months, hours)
  clim_means <- dplyr::summarise_each(clim_means, dplyr::funs(mean(., na.rm = T)))
  
  # merge with original data
  to_join <- data.frame(datetimestamp = dat$datetimestamp, months, 
    hours, stringsAsFactors = FALSE) 
  clim_means <- dplyr::left_join(
    to_join,
    clim_means, by = c('months','hours')
  )
  clim_means <- clim_means[order(clim_means$datetimestamp),]

  # datetimestamp order in dat must be ascending to match
  if(is.unsorted(dat$datetimestamp))
    stop('datetimestamp is unsorted')
  
  # reassign empty values to means, objects are removed later
  atemp_mix <- dat$atemp
  wspd_mix <- dat$wspd
  bp_mix <- dat$bp
  atemp_mix[is.na(atemp_mix)] <- clim_means$atemp[is.na(atemp_mix)]
  wspd_mix[is.na(wspd_mix)] <- clim_means$wspd[is.na(wspd_mix)]
  bp_mix[is.na(bp_mix)] <- clim_means$bp[is.na(bp_mix)]

  ##
  # get sigma_t estimates
  sigt <- with(dat, swSigmaT(sal, temp, mean(dat$bp/100, na.rm = T)))
  
  # dosat is do at saturation given temp (C), salinity (st. unit), and press (atm)
  # dosat as proportion
  # used to get loss of O2 from diffusion
  dosat <- with(dat, do_mgl/(oxySol(temp * (1000 + sigt)/1000, sal)))
  
  #station depth, defaults to mean depth value, floored at 1 in case not on bottom
  #uses 'depth.val' if provided
  if(is.null(depth_val))
    H <- rep(0.5 + mean(pmax(1, dat$depth), na.rm = T), nrow(dat))
  else H <- rep(depth.val, nrow(dat))
  
  #use metab_day to add columns indicating light/day, date, and hours of sunlight
  dat <- metab_day(dat, stat)
  
  #get air sea gas-exchange using wx data with climate means
  KL <- with(dat, calckl(temp, sal, atemp_mix, wspd_mix, bp_mix, ...))
  rm(list = c('atemp_mix', 'wspd_mix', 'bp_mix'))
  
  #get volumetric reaeration coefficient from KL 
  Ka <- KL / 24 / H
  
  #get exchange at air water interface
  D = Ka * (do / dosat - do)
  
  #combine all data for processing
  proc_dat <- dat[, names(dat) %in% c('metab_date', 'solar_period', 'day_hrs')]
  proc_dat <- data.frame(proc_dat, ddo, H, D)

  #get daily/nightly flux estimates for Pg, Rt, NEM estimates
  out <- lapply(
    split(proc_dat, proc_dat$metab_date),
    function(x){
      
      #filter for minimum no. of records 
      if(length(with(x[x$solar_period == 'sunrise', ], na.omit(ddo))) < 3 |
         length(with(x[x$solar_period == 'sunset', ], na.omit(ddo))) < 3 ){
        DOF_d <- NA; D_d <- NA; DOF_n <- NA; D_n <- NA
      
      } else {
        #day
        DOF_d <- mean(with(x[x$solar_period == 'sunrise', ], ddo * H), na.rm = T)
        D_d <- mean(with(x[x$solar_period == 'sunrise', ], D), na.rm = T)
        
        #night
        DOF_n <- mean(with(x[x$solar_period == 'sunset', ], ddo * H), na.rm = T)
        D_n <- mean(with(x[x$solar_period == 'sunset', ], D), na.rm = T)
        
      }
      
      # metabolism
      # account for air-sea exchange if surface station
      # else do not
      Pg <- ((DOF_d - D_d) - (DOF_n - D_n)) * unique(x$day_hrs)
      Rt <- (DOF_n - D_n) * 24
      NEM <- Pg + Rt
      Pg_vol <- Pg / mean(x$H, na.rm = T)
      Rt_vol <- Rt / mean(x$H, na.rm = T)
      
      # output
      data.frame(date = unique(x$metab_date), 
        DOF_d, D_d, DOF_n, D_n, Pg, Rt, NEM
        )
      
      }
    )
  
  out <- do.call('rbind',out)
  row.names(out) <- 1:nrow(out)

  # change units to grams
  if('grams' %in% metab_units){
    
    # convert metab data to g m^-2 d^-1
    # 1mmolO2 = 32 mg O2, 1000mg = 1g, multiply by 32/1000
    as_grams <- apply(out[, -1], 2, function(x) x * 0.032)
    out <- data.frame(date = out[, 'date'], as_grams)
    
  }
  
  # append to metabolism attribute
  attr(swmpr_in, 'metabolism') <- out
  attr(swmpr_in, 'metab_units') <- metab_units
  
  if(trace) tictoc::toc()
  
  return(swmpr_in)
  
}

######
#' Plot ecosystem metabolism for a swmpr object
#'
#' Plot gross production, total respiration, and net ecosystem metabolism for a swmpr object. 
#'
#' @param swmpr_in input swmpr object
#' @param by chr string describing aggregation period, passed to \code{\link{aggregate_metab}}. See details for accepted values.
#' @param alpha numeric indicating alpha level for confidence intervals in aggregated data. Use \code{NULL} to remove from the plot.
#' @param width numeric indicating width of top and bottom segments on error bars
#' @param pretty logical indicating use of predefined plot aesthetics
#' @param ... arguments passed to or from other methods
#'
#' @export
#' 
#' @import ggplot2
#'
#' @details 
#' A plot will only be returned if the \code{metabolism} attribute for the \code{\link{swmpr}} object is not \code{NULL}.  Daily metabolism estimates are also aggregated into weekly averages.  Accepted aggregation periods are \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, and \code{'days'} (if no aggregation is preferred).
#' 
#' By default, \code{pretty = TRUE} will return a \code{\link[ggplot2]{ggplot}} object with predefined aesthetics.  Setting \code{pretty = FALSE} will return the plot with minimal modifications to the \code{\link[ggplot2]{ggplot}} object.  Use the latter approach for easier customization of the plot.  
#' 
#' @return 
#' A \code{\link[ggplot2]{ggplot}} object which can be further modified.
#' 
#' @seealso 
#' \code{\link{aggregate_metab}}, \code{\link{ecometab}}
#' 
#' @examples
#' ## import water quality and weather data
#' data(apadbwq)
#' data(apaebmet)
#' 
#' ## qaqc, combine
#' wq <- qaqc(apadbwq)
#' met <- qaqc(apaebmet)
#' dat <- comb(wq, met)
#' 
#' ## estimate metabolism
#' res <- ecometab(dat, trace = TRUE)
#' 
#' ## plot
#' plot_metab(res)
#' 
#' ## change alpha, aggregation period, widths
#' plot_metab(res, by = 'quarters', alpha = 0.1, widths = 0)
#'
#' ## plot daily raw, no aesthetics
#' plot_metab(res, by = 'days', pretty = FALSE)
plot_metab <- function(swmpr_in, ...) UseMethod('plot_metab')

#' @rdname plot_metab
#'
#' @export
#'
#' @method plot_metab swmpr
plot_metab.swmpr <- function(swmpr_in, by = 'months', alpha = 0.05, width = 10, pretty = TRUE, ...){
  
  # get metabolism estimates
  metabolism <- attr(swmpr_in, 'metabolism')
  metab_units <- attr(swmpr_in, 'metab_units')
  
  if(is.null(metabolism)) 
    stop('No metabolism data, use the ecometab function')
  
  # aggregate metab results by time period
  to_plo <- aggregate_metab(swmpr_in, by = by, alpha = alpha)
  
  ## base plot
  p <- ggplot(to_plo, aes(x = date, y = means, group = Estimate)) +
    geom_line()
  
  # add bars if not days and alpha not null
  if(by != 'days' & !is.null(alpha))
    p <- p +
      geom_errorbar(aes(ymin = lower, ymax = upper, group = Estimate), 
      width = width) 
  
  # return blank
  if(!pretty) 
    return(p)
  
  # ylabs
  ylabs <- expression(paste('mmol ', O [2], ' ', m^-2, d^-1))
  if(metab_units == 'grams')
    ylabs <- expression(paste('g ', O [2], ' ', m^-2, d^-1))
  
  p <- p + 
    geom_line(aes(colour = Estimate)) +
    geom_point(aes(colour = Estimate)) +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(ylabs)
  
  if(by != 'days' & !is.null(alpha))
    p <- p + 
      geom_errorbar(aes(ymin = lower, ymax = upper,
        colour = Estimate, group = Estimate), width = width)
    
  return(p)
  
}