#' Aggregate metabolism data
#' 
#' Aggregate a metabolism attribute from swmpr data by a specified time period and method
#' 
#' @param swmpr_in input swmpr object
#' @param by chr string or numeric value specifying aggregation period.  If chr string, must be \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'}. A numeric value indicates the number of days for a moving window average.  Additional arguments passed to \code{\link{smoother}} can be used if \code{by} is numeric.
#' @param na.action function for treating missing data, default \code{na.pass}
#' @param alpha numeric indicating alpha level of confidence interval for aggregated data
#' @param ... additional arguments passed to other methods
#' 
#' @import data.table
#' 
#' @importFrom stats na.omit na.pass qt sd
#' 
#' @concept analyze
#' 
#' @export
#' 
#' @details The function summarizes metabolism data by averaging across set periods of observation. Confidence intervals are also returned based on the specified alpha level.  It is used within \code{\link{plot_metab}} function to view summarized metabolism results.  Data can be aggregated by \code{'years'}, \code{'quarters'}, \code{'months'}, or \code{'weeks'} for the supplied function, which defaults to the \code{\link[base]{mean}}. The method of treating NA values for the user-supplied function should be noted since this may greatly affect the quantity of data that are returned.
#' 
#' @return Returns an aggregated metabolism \code{\link[base]{data.frame}} if the \code{metabolism} attribute of the swmpr object is not \code{NULL}.  Upper and lower confidence limits are also provided if the aggregation period was specified as a character string.
#' 
#' @seealso \code{\link[stats]{aggregate}}, \code{\link{aggreswmp}}, \code{\link{ecometab}}, \code{\link{plot_metab}}
#' 
#' @examples
#' \dontrun{
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
#' res <- ecometab(dat)
#' 
#' ## change aggregation period and alpha
#' aggremetab(res, by = 'months', alpha = 0.1)
#' 
#' ## use a moving window average of 30 days
#' aggremetab(res, by = 30)
#' 
#' ## use a left-centered window instead
#' aggremetab(res, by = 30, sides = 1)
#' }
aggremetab <- function(swmpr_in, ...) UseMethod('aggremetab')

#' @rdname aggremetab
#'
#' @export
#'
#' @method aggremetab swmpr
aggremetab.swmpr <- function(swmpr_in, by = 'weeks', na.action = na.pass, alpha = 0.05, ...){
  
  # attributes
  timezone <- attr(swmpr_in, 'timezone')
  metabolism <- attr(swmpr_in, 'metabolism')
  
  # sanity checks
  if(is.null(metabolism)) 
    stop('No metabolism data, use the ecometab function')
  
  # data
  to_agg <- metabolism
  to_agg <- to_agg[, names(to_agg) %in% c('date', 'Pg', 'Rt', 'NEM')]
  
  # if agg is a character string
  if(inherits(by, 'character')){
    
    # stop if value not accepted
    if(!by %in% c('years', 'quarters', 'months', 'weeks', 'days'))
      stop('Unknown value for by, see help documentation')
    
    # create agg values from date
    if(by != 'days'){
      to_agg$date <- round(
        data.table::as.IDate(to_agg$date, tz = timezone),
        digits = by
      )
      to_agg$date <- base::as.Date(to_agg$date, tz = timezone)
    }
    
    # long-form
    to_agg <- reshape2::melt(to_agg, measure.vars = c('Pg', 'Rt', 'NEM'))
    names(to_agg) <- c('date', 'Estimate', 'Value')
    to_agg$Estimate <- as.character(to_agg$Estimate)
    
    # aggregate
    sum_fun <- function(x, alpha_in = alpha){
      x <- na.omit(x)
      means <- mean(x)
      margs <- suppressWarnings(
        qt(1 - alpha_in/2, length(x) - 1) * sd(x)/sqrt(length(x))
      )
      upper <- means + margs
      lower <- means - margs
      
      return(c(means, upper, lower))
    }
    aggs <- stats::aggregate(Value ~ date + Estimate, to_agg, 
                             FUN = function(x) sum_fun(x, alpha_in = alpha))
    aggs_vals <- data.frame(aggs[, 'Value'])
    names(aggs_vals) <- c('val', 'lower', 'upper')
    aggs <- data.frame(aggs[, c('date', 'Estimate')], aggs_vals)
    
    # if agg is numeric
  } else {
    
    # stop if not numeric
    if(!inherits(by, c('numeric', 'integer')))
      stop('By argument must be character string of aggregation period or numeric indicating number of days')
    
    # use smoother default method
    aggs <- smoother(to_agg[, c('Pg', 'Rt', 'NEM')], window = by, ...)
    aggs <- data.frame(date = to_agg$date, aggs)
    
    # long format
    aggs <- reshape2::melt(aggs, measure.vars = c('Pg', 'Rt', 'NEM'))
    names(aggs) <- c('date', 'Estimate', 'val')
    
  }    
  
  # return output
  return(aggs)
  
}