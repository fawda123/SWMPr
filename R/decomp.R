#' Simple trend decomposition
#' 
#' Decompose data into trend, cyclical (e.g., daily, annual), and random components using \code{\link[stats]{decompose}} and \code{\link[stats]{ts}}
#' 
#' @param dat_in input data object
#' @param ... arguments passed to \code{decompose}, \code{ts}, and other methods
#' 
#' @export decomp
#' 
#' @importFrom stats decompose ts
#' 
#' @concept analyze
#' 
#' @details
#' This function is a simple wrapper to the \code{\link[stats]{decompose}} function.  The \code{decompose} function separates a time series into additive or multiplicative components describing a trend, cyclical variation (e.g., daily or annual), and the remainder.  The additive decomposition assumes that the cyclical component of the time series is stationary (i.e., the variance is constant), whereas a multiplicative decomposition accounts for non-stationarity.  By default, a moving average with a symmetric window is used to filter the cyclical component.  Alternatively, a vector of filter coefficients in reverse time order can be supplied (see \code{\link[stats]{decompose}}).  
#' 
#' The \code{decompose} function requires a ts object with a specified frequency.  The \code{decomp} function converts the input swmpr vector to a ts object prior to \code{decompose}.  This requires an explicit input defining the frequency in the time series required to complete a full period of the parameter.  For example, the frequency of a parameter with diurnal periodicity would be 96 if the time step is 15 minutes (24 hours * 60 minutes / 15 minutes).  The frequency of a parameter with annual periodicity at a 15 minute time step would be 35040 (365 days * 24 hours * 60 minutes / 15 minutes).  For simplicity, chr strings of \code{'daily'} or \code{'annual'} can be supplied in place of numeric values.  A starting value of the time series must be supplied in the latter case.  Use of the \code{\link{setstep}} function is required to standardize the time step prior to decomposition.  
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
decomp <- function(dat_in, ...) UseMethod('decomp') 

#' @rdname decomp
#' 
#' @param param chr string of swmpr parameter to decompose
#' @param type chr string of \code{'additive'} or \code{'multiplicative'} indicating the type of decomposition, default \code{'additive'}.
#' @param frequency chr string or numeric vector indicating the periodic component of the input parameter.  Only \code{'daily'} or \code{'annual'} are accepted as chr strings.  Otherwise a numeric vector specifies the number of observations required for a full cycle of the input parameter.  Defaults to \code{'daily'} for a diurnal parameter.
#' @param start numeric vector indicating the starting value for the time series given the frequency.  Only required if \code{frequency} is numeric. See \code{\link[stats]{ts}}.
#' 
#' @export
#' 
#' @method decomp swmpr
decomp.swmpr <- function(dat_in, param, type = 'additive', frequency = 'daily', start = NULL, ...){
  
  # attributes
  parameters <- attr(dat_in, 'parameters')
  
  # stop if param not in parameters
  if(!any(param %in% parameters) & !is.null(param))
    stop('Params argument must name input columns')
  
  # to data frame for default
  dat_in <- as.data.frame(dat_in)
  
  decomp(dat_in, param = param, date_col = 'datetimestamp', type = type, 
         frequency = frequency, start = start)
  
}

#' @rdname decomp
#' 
#' @param date_col chr string of the name of the date column
#' 
#' @export
#' 
#' @method decomp default
decomp.default <- function(dat_in, param, date_col, type = 'additive', frequency = 'daily', start = NULL, ...){
  
  # stop if param not in input data names
  if(!param %in% names(dat_in))
    stop('Params argument must name input columns')
  
  # stop if frequency or start are incorrect
  if(!is.numeric(frequency) & !any(frequency %in% c('daily', 'annual'))){
    stop("Chr string input for frequency must be 'daily' or 'annual'")
  } else {
    if(!is.null(start))
      stop('Start argument required if frequency is numeric')
  }
  
  # stop if time series is not standardized
  chk_step <- unique(diff(dat_in[, date_col]))
  if(length(chk_step) > 1)
    stop('The time step is not standardized, use setstep')
  
  # timezone
  timezone <- attr(dat_in[, date_col], 'tzone') 
  
  ##
  # get frequency and starting value if input not numeric
  start <- dat_in[1, date_col]
  day <- as.numeric(strftime(start, '%j', tz = timezone))
  hour <- as.numeric(strftime(start, '%H', tz = timezone))
  min <- as.numeric(strftime(start, '%M', tz = timezone))
  if(frequency == 'daily'){
    frequency  <- 24 * 60 / chk_step
    start <- 1 + (hour + min / 60) * 60 / chk_step
  }
  if(frequency == 'annual'){
    frequency <- 365 * 24 * 60 / chk_step 
    start <- (day + hour / 24 + min / 60 / 24) * 24 * 60 / chk_step
  }
  
  # make ts and decompose
  ts_smp <- ts(dat_in[, param], start = c(1, start), frequency = frequency)
  out <- decompose(ts_smp, type, ...)
  
  # return decompose.ts
  return(out)
  
}