#' Plot swmpr data
#' 
#' Plot a time series of parameters in a swmpr object
#' 
#' @param x input swmpr object
#' @param type chr string for type of plot, default \code{'l'}.  See \code{\link[graphics]{plot}}.
#' @param ... other arguments passed to \code{par}, \code{plot.default}
#' 
#' @export
#' 
#' @importFrom stats formula
#' 
#' @concept analyze
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
#' dat <- subset(swmp1, select = 'do_mgl', 
#'  subset = c('2013-07-01 00:00', '2013-07-31 00:00'))
#'
#' ## plot using swmpr method, note default line plot
#' plot(dat)
#' 
#' ## plot using formula method
#' plot(do_mgl ~ datetimestamp, dat)
#' 
#' ## plot using defualt, add lines
#' plot(dat, type = 'n')
#' lines(dat, col = 'red')
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
#' @export
#' 
#' @importFrom stats formula
#' @importFrom graphics lines
#' 
#' @method lines swmpr
lines.swmpr <- function(x, ...) {
  
  swmpr_in <- x
  
  if(attr(swmpr_in, 'qaqc_cols'))
    swmpr_in <- qaqc(swmpr_in, qaqc_keep = NULL)
  
  if(ncol(swmpr_in) > 2) stop('Only one parameter can be plotted, use subset first')
  
  parameters <- attr(swmpr_in, 'parameters')
  
  form_in <- formula(substitute(i ~ datetimestamp, 
                                list(i = as.name(parameters))))
  lines(form_in, data = swmpr_in, ...)
  
}