#' Plot swmpr using a histogram
#' 
#' Plot a histogram showing the distribution of a swmpr parameter
#' 
#' @param x input swmpr object
#' @param ... other arguments passed to \code{\link[graphics]{hist}}
#' 
#' @details The swmpr method for histograms is a convenience function for the default histogram function.  Conventional histogram methods also work well since swmpr objects are also data frames.  The input data must contain only one parameter.
#' 
#' @export
#' 
#' @importFrom graphics hist
#' 
#' @concept analyze
#' 
#' @method hist swmpr
#' 
#' @seealso \code{\link[graphics]{hist}}
#' 
#' @examples
#' ## get data
#' data(apadbwq)
#' dat <- subset(apadbwq, select = 'do_mgl')
#'
#' ## histogram using swmpr method
#' hist(dat)
#' 
#' ## change axis labels, plot title
#' hist(dat, xlab = 'Dissolved oxygen', main = 'Histogram of DO')
#' 
#' ## plot using default method
#' hist(dat$do_mgl)
hist.swmpr <- function(x, ...) {
  
  swmpr_in <- x
  
  if(attr(swmpr_in, 'qaqc_cols'))
    swmpr_in <- qaqc(swmpr_in, qaqc_keep = NULL)
  
  if(ncol(swmpr_in) > 2) stop('Only one parameter can be plotted, use subset first')
  
  param <- attr(swmpr_in, 'parameters')
  
  hist(swmpr_in[, param], ...)
  
}