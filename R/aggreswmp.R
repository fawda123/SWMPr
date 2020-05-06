#' Aggregate swmpr data
#' 
#' Aggregate swmpr data by specified time period and method
#' 
#' @param swmpr_in input swmpr object
#' @param by chr string of time period for aggregation one of \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'}
#' @param FUN aggregation function, default \code{mean} with \code{na.rm = TRUE}
#' @param params names of parameters to aggregate, default all
#' @param aggs_out logical indicating if \code{\link[base]{data.frame}} is returned of raw data with datetimestamp formatted as aggregation period, default \code{FALSE}
#' @param plot logical to return a plot of the summarized data, default \code{FALSE}
#' @param na.action function for treating missing data, default \code{na.pass}.  See the documentation for \code{\link[stats]{aggregate}} for options.
#' @param ... additional arguments passed to other methods
#' 
#' @concept analyze
#' 
#' @import data.table ggplot2
#' 
#' @importFrom stats aggregate formula na.pass
#' 
#' @export
#' 
#' @details The function aggregates parameter data for a swmpr object by set periods of observation and a user-supplied function. It is most useful for aggregating noisy data to evaluate trends on longer time scales, or to simply reduce the size of a dataset. Data can be aggregated by \code{'years'}, \code{'quarters'}, \code{'months'}, \code{'weeks'}, \code{'days'}, or \code{'hours'} for the supplied function, which defaults to the \code{\link[base]{mean}}. A swmpr object is returned for the aggregated data, although the datetimestamp vector will be converted to a date object if the aggregation period is a day or longer. Days are assigned to the date vector if the aggregation period is a week or longer based on the round method for \code{\link[data.table]{IDate}} objects. This approach was used to facilitate plotting using predefined methods for Date and POSIX objects.
#' 
#' The method of treating NA values for the user-supplied function should be noted since this may greatly affect the quantity of data that are returned (see the examples). Finally, the default argument for \code{na.action} is set to \code{na.pass} for swmpr objects to preserve the time series of the input data.
#' 
#' @return Returns an aggregated swmpr object. QAQC columns are removed if included with input object.  If \code{aggs_out = TRUE}, the original \code{swmpr} object is returned with the \code{datetimestamp} column formatted for the first day of the aggregation period from \code{by}.  A \code{\link[ggplot2]{ggplot}} object of boxplot summaries is returned if \code{plot = TRUE}.
#' 
#' @seealso \code{\link[stats]{aggregate}}
#' 
#' @examples
#' \dontrun{
#' ## get data, prep
#' data(apacpwq)
#' dat <- apacpwq
#' swmpr_in <- subset(qaqc(dat), rem_cols = TRUE)
#'
#' ## get mean DO by quarters
#' aggreswmp(swmpr_in, 'quarters', params = c('do_mgl'))
#'
#' ## get a plot instead
#' aggreswmp(swmpr_in, 'quarters', params = c('do_mgl'), plot = T)
#' 
#' ## plots with other variables
#' p <- aggreswmp(swmpr_in, 'months', params = c('do_mgl', 'temp', 'sal'), plot = T)
#' p
#' library(ggplot2)
#' p + geom_boxplot(aes(fill = var)) + theme(legend.position = 'none')
#'
#' ## get variance of DO by years, remove NA when calculating variance
#' ## omit NA data in output
#' fun_in <- function(x)  var(x, na.rm = TRUE)
#' aggreswmp(swmpr_in, FUN = fun_in, 'years') 
#' }
aggreswmp <- function(swmpr_in, ...) UseMethod('aggreswmp')

#' @rdname aggreswmp
#' 
#' @export
#'
#' @method aggreswmp swmpr
aggreswmp.swmpr <- function(swmpr_in, by, FUN = function(x) mean(x, na.rm = TRUE), params = NULL, aggs_out = FALSE, plot = FALSE, na.action = na.pass, ...){
  
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
  
  # return plot if true
  if(plot){
    
    toplo <- tidyr::gather(to_agg, 'var', 'val', -datetimestamp)
    
    p <- ggplot(toplo, aes(x = factor(datetimestamp), y = val)) +
      geom_boxplot() +
      facet_wrap(~ var, scales = 'free_y', ncol = 1) + 
      theme_bw() +
      theme(axis.title.y = element_blank()) + 
      scale_x_discrete(by)
    
    return(p)
    
  }
  
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