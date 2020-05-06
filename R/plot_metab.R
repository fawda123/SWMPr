#' Plot ecosystem metabolism for a swmpr object
#'
#' Plot gross production, total respiration, and net ecosystem metabolism for a swmpr object. 
#'
#' @param swmpr_in input swmpr object
#' @param by chr string describing aggregation period, passed to \code{\link{aggremetab}}. See details for accepted values.
#' @param alpha numeric indicating alpha level for confidence intervals in aggregated data. Use \code{NULL} to remove from the plot.
#' @param width numeric indicating width of top and bottom segments on error bars
#' @param pretty logical indicating use of predefined plot aesthetics
#' @param ... arguments passed to or from other methods
#'
#' @export
#' 
#' @concept analyze
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
#' \code{\link{aggremetab}}, \code{\link{ecometab}}
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
#' ## plot
#' plot_metab(res)
#' 
#' ## change alpha, aggregation period, widths
#' plot_metab(res, by = 'quarters', alpha = 0.1, widths = 0)
#'
#' ## plot daily raw, no aesthetics
#' plot_metab(res, by = 'days', pretty = FALSE)
#' 
#' ## note the difference if aggregating with a moving window average
#' plot_metab(res, by = 30)
#' }
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
  to_plo <- aggremetab(swmpr_in, by = by, alpha = alpha)
  
  ## base plot
  p <- ggplot(to_plo, aes_string(x = 'date', y = 'val', group = 'Estimate')) +
    geom_line()
  
  # add bars if not days and alpha not null
  if(inherits(by, c('numeric', 'integer'))) alpha <- NULL
  if(by != 'days' & !is.null(alpha))
    p <- p +
    geom_errorbar(
      aes_string(ymin = 'lower', ymax = 'upper', group = 'Estimate'), 
      width = width) 
  
  # return blank
  if(!pretty) 
    return(p)
  
  # ylabs
  ylabs <- expression(paste('mmol ', O [2], ' ', m^-2, d^-1))
  if(metab_units == 'grams')
    ylabs <- expression(paste('g ', O [2], ' ', m^-2, d^-1))
  
  p <- p + 
    geom_line(aes_string(colour = 'Estimate')) +
    geom_point(aes_string(colour = 'Estimate')) +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(ylabs)
  
  if(by != 'days' & !is.null(alpha))
    p <- p + 
    geom_errorbar(aes_string(ymin = 'lower', ymax = 'upper',
                             colour = 'Estimate', group = 'Estimate'), width = width)
  
  return(p)
  
}