#' Smooth swmpr data
#' 
#' Smooth swmpr data with a moving window average
#' 
#' @param x input object
#' @param window numeric vector defining size of the smoothing window, passed to \code{filter} 
#' @param sides numeric vector defining method of averaging, passed to \code{filter}
#' @param params is chr string of swmpr parameters to smooth, default all
#' @param ... arguments passed to or from other methods
#'  
#' @concept analyze
#' 
#' @export smoother
#' 
#' @return Returns a filtered swmpr object. QAQC columns are removed if included with input object.
#' 
#' @details The \code{smoother} function can be used to smooth parameters in a swmpr object using a specified window size. This method is a simple wrapper to \code{\link[stats]{filter}}. The window argument specifies the number of observations included in the moving average. The sides argument specifies how the average is calculated for each observation (see the documentation for \code{\link[stats]{filter}}). A value of 1 will filter observations within the window that are previous to the current observation, whereas a value of 2 will filter all observations within the window centered at zero lag from the current observation. The params argument specifies which parameters to smooth.
#' 
#' @seealso \code{\link[stats]{filter}}
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
smoother <- function(x, ...) UseMethod('smoother') 


#' @rdname smoother
#' 
#' @export
#' 
#' @method smoother default
smoother.default <- function(x, window = 5, sides = 2, ...){
  
  window <- rep(1, window)/window
  nms <- names(x)
  out <- stats::filter(x, window, sides, method = 'convolution', ...)
  out <- as.data.frame(out)
  names(out) <- nms
  
  return(out)
  
}

#' @rdname smoother
#' 
#' @export
#' 
#' @method smoother swmpr
smoother.swmpr <- function(x, params = NULL, ...){
  
  # attributes
  parameters <- attr(x, 'parameters')
  station <- attr(x, 'station')
  
  # sanity checks
  if(!any(params %in% parameters) & !is.null(params))
    stop('Params argument must name input columns')
  if(attr(x, 'qaqc_cols'))
    warning('QAQC columns present, removed in output')
  
  # prep for filter
  if(!is.null(params)) parameters <- parameters[parameters %in% params]
  to_filt <- x[, c('datetimestamp', parameters), drop = FALSE]
  to_filt <- as.data.frame(to_filt)
  datetimestamp <- to_filt$datetimestamp
  to_filt$datetimestamp <- NULL
  
  # filter
  out <- smoother(to_filt, ...)
  out <- data.frame(datetimestamp, out)
  names(out) <- c('datetimestamp', parameters)
  
  # format output as swmpr object
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
}