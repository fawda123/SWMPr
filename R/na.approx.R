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
#' @export
#' 
#' @method na.approx swmpr
#' 
#' @concept analyze
#' 
#' @details A common approach for handling missing data in time series analysis is linear interpolation.  A simple curve fitting method is used to create a continuous set of records between observations separated by missing data.  A required argument for the function is \code{maxgap} which defines the maximum gap size  for interpolation. The ability of the interpolated data to approximate actual, unobserved trends is a function of the gap size.  Interpolation between larger gaps are less likely to resemble patterns of an actual parameter, whereas interpolation between smaller gaps may be more likely to resemble actual patterns.  An appropriate gap size limit depends on the unique characteristics of specific datasets or parameters.  
#' 
#' @seealso \code{\link[zoo]{na.approx}}
#' 
#' @return Returns a swmpr object. QAQC columns are removed if included with input object.
#' 
#' @examples
#' data(apadbwq)
#' dat <- qaqc(apadbwq)
#' dat <- subset(dat, select = 'do_mgl', 
#'  subset = c('2013-01-22 00:00', '2013-01-26 00:00'))
#' 
#' # interpolate, maxgap of 10 records
#' fill1 <- na.approx(dat, params = 'do_mgl', maxgap = 10)
#' 
#' # interpolate maxgap of 30 records
#' fill2 <- na.approx(dat, params = 'do_mgl', maxgap = 30)
#' 
#' # plot for comparison
#' par(mfrow = c(3, 1))
#' plot(fill1, col = 'red', main = 'Interpolation - maximum gap of 10 records')
#' lines(dat)
#' plot(fill2, col = 'red', main = 'Interpolation - maximum gap of 30 records')
#' lines(dat)
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