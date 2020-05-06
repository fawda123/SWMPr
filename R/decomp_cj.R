#' Simple trend decomposition of monthly swmpr data
#' 
#' Decompose monthly SWMP time series into grandmean, annual, seasonal, and event series as described in Cloern and Jassby 2010.
#' 
#' @param dat_in input data object
#' @param param chr string of variable to decompose
#' @param date_col chr string indicating the name of the date column which should be a date or POSIX object.
#' @param vals_out logical indicating of numeric output is returned, default is \code{FALSE} to return a plot.
#' @param event logical indicating if an 'events' component should be determined
#' @param type chr string indicating the type of decomposition, either additive (\code{'add'}) or multiplicative (\code{'mult'})
#' @param center chr string indicating the method of centering, either \code{'mean'} or \code{'median'}
#' @param ... additional arguments passed to or from other methods
#' 
#' @concept analyze
#' 
#' @return  
#' A \code{\link[ggplot2]{ggplot}} object if \code{vals_out = FALSE} (default), otherwise a monthly time series matrix of class \code{\link[stats]{ts}}.
#' 
#' @details
#' This function is a simple wrapper to the \code{decompTs} function in the archived wq package, also described in Cloern and Jassby (2010).  The function is similar to \code{\link{decomp.swmpr}} (which is a wrapper to \code{\link[stats]{decompose}}) with a few key differences.  The \code{\link{decomp.swmpr}} function decomposes the time series into a trend, seasonal, and random components, whereas the current function decomposes into the grandmean, annual, seasonal, and events components.  For both functions, the random or events components, respectively, can be considered anomalies that don't follow the trends in the remaining categories.  
#' 
#' The \code{decomp_cj} function provides only a monthly decomposition, which is appropriate for characterizing relatively long-term trends.  This approach is meant for nutrient data that are obtained on a monthly cycle.  The function will also work with continuous water quality or weather data but note that the data are first aggregated on the monthly scale before decomposition.  Use the \code{\link{decomp.swmpr}} function to decompose daily variation.
#' 
#' @export
#' 
#' @import ggplot2
#' 
#' @importFrom stats aggregate ts
#' @importFrom utils capture.output
#' 
#' @seealso \code{\link[stats]{ts}}
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
#' ## decomposition changing arguments passed to decompTs
#' decomp_cj(dat, param = 'chla_n', type = 'mult')
#' 
#' ## monthly decomposition of continuous data
#' data(apacpwq)
#' dat2 <- qaqc(apacpwq)
#' 
#' decomp_cj(dat2, param = 'do_mgl')
#' 
#' ## using the default method with a data frame
#' dat <- data.frame(dat)
#' decomp_cj(dat, param = 'chla_n', date_col = 'datetimestamp')
decomp_cj <- function(dat_in, ...) UseMethod('decomp_cj') 

#' @rdname decomp_cj
#' 
#' @export
#' 
#' @method decomp_cj swmpr
decomp_cj.swmpr <- function(dat_in, param, vals_out = FALSE, event = TRUE, type = c('add', 'mult'), center = c('mean', 'median'), ...){
  
  dat <- dat_in
  
  ## sanity checks
  parameters <- attr(dat, 'parameters')
  if(!param %in% parameters) stop('Selected parameter not in data')
  
  # monthly ts
  dat <- aggreswmp(dat, by = 'months', params = param)
  dat <- data.frame(dat)
  decomp_cj(dat, param = param, date_col = 'datetimestamp', vals_out = vals_out, event = event, type = type, center = center, ...)
  
}

#' @rdname decomp_cj
#' 
#' @export
#' 
#' @method decomp_cj default
decomp_cj.default <- function(dat_in, param, date_col, vals_out = FALSE, event = TRUE, type = c('add', 'mult'), center = c('mean', 'median'), ...){
  
  # select date column and parameter
  dat <- dat_in[, c(date_col, param)]
  dat[, date_col] <- as.Date(dat[, date_col])
  
  # check months to see if one value per month, if not then aggregate
  chkmos <- strftime(dat[, date_col], '%m')
  if(any(duplicated(chkmos))){
    yrs <- strftime(dat[, date_col], '%Y')
    mos <- strftime(dat[, date_col], '%m')
    toagg <- paste(yrs, mos, '01', sep = '-')
    dat[, date_col] <- toagg
    names(dat) <- c('x', 'y')
    dat <- aggregate(y ~ x, dat, FUN = mean, na.rm = TRUE)
    names(dat) <- c(date_col, param)
    dat[, date_col] <- as.Date(dat[, date_col], format = '%Y-%m-%d')
  }
  
  # create a continuous month vector so decomp is on equal step
  dat_rng <- as.Date(range(dat[, date_col], na.rm = TRUE))
  months <- data.frame(seq.Date(dat_rng[1], dat_rng[2], by = 'months'))
  names(months) <- date_col
  dat <- merge(months, dat,  by = date_col, all.x = T)
  
  # find starting year, month to create a ts object
  year <- as.numeric(strftime(dat_rng[1], '%Y'))
  month <- as.numeric(strftime(dat_rng[1], '%m'))
  dat_mts <- ts(dat[, param], frequency = 12, start = c(year, month))
  
  # decomp
  out <- decompTs(dat_mts, event = event, type = type, center = center, ...)
  
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
  plo <- ggplot(to_plo, 
                aes_string(x = 'Time', y = 'value', group = 'variable')
  ) +
    geom_line() +
    facet_wrap(~variable, ncol = 1, scales = 'free_y') + 
    theme_bw()
  
  return(plo)
  
}