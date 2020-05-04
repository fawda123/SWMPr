#' Plot multiple SWMP time series on the same y-axis
#' 
#' Plot multiple SWMP time series on the same y-axis, aka overplotting
#' 
#' @param dat_in input data object
#' @param date_var chr string of the name for the datetimestamp column, not required for \code{\link{swmpr}} objects 
#' @param select chr string of variable(s) to plot, passed to \code{\link{subset}}.  This is a required argument for the default method.
#' @param subset chr string of form 'YYYY-mm-dd HH:MM' to subset a date range. Input can be one (requires operator or two values (a range).  Passed to \code{\link{subset}}.
#' @param operator chr string specifiying binary operator (e.g., '>', '<=') if subset is one date value, passed to \code{\link{subset}}
#' @param ylabs chr string of labels for y-axes, default taken from \code{select} argument
#' @param xlab chr string of label for x-axis
#' @param cols chr string of colors to use for lines
#' @param lty numeric indicating line types, one value for all or values for each parameter
#' @param lwd numeric indicating line widths, one value for all or values for each parameter, used as \code{cex} for point size if \code{type = 'p'}
#' @param inset numeric of relative location of legend, passed to \code{\link[graphics]{legend}}
#' @param cex numeric of scale factor for legend, passed to \code{\link[graphics]{legend}}
#' @param xloc x location of legend, passed to \code{\link[graphics]{legend}}
#' @param yloc y location of legend, passed to \code{\link[graphics]{legend}}
#' @param pch numeric for point type of points are used
#' @param type character string indicating \code{'p'} or \code{'l'} for points or lines, as a single value for all parameters or a combined vector equal in length to the number of parameters 
#' @param ... additional arguments passed to \code{\link[graphics]{plot}}
#' 
#' @export
#' 
#' @concept analyze
#' 
#' @details One to many SWMP parameters can be plotted on the same y-axis to facilitate visual comparison.  This is commonly known as overplotting.  The building blocks of this function include \code{\link[graphics]{plot}}, \code{\link[graphics]{legend}}, \code{\link[graphics]{axis}}, and \code{\link[graphics]{mtext}}. 
#' 
#' @return An R plot created using base graphics
#' 
#' @seealso \code{\link{subset}}
#' 
#' @examples
#' ## import data
#' data(apacpwq)
#' dat <- qaqc(apacpwq)
#' 
#' ## plot
#' overplot(dat)
#' 
#' ## a truly heinous plot
#' overplot(dat, select = c('depth', 'do_mgl', 'ph', 'turb'), 
#'  subset = c('2013-01-01 0:0', '2013-02-01 0:0'), lwd = 2)
#'  
#' \dontrun{
#' ## change the type argument if plotting discrete and continuous data
#' swmp1 <- apacpnut
#' swmp2 <- apaebmet
#' dat <- comb(swmp1, swmp2, timestep = 120, method = 'union')
#' overplot(dat, select = c('chla_n', 'atemp'), subset = c('2012-01-01 0:0', '2013-01-01 0:0'), 
#'  type = c('p', 'l'))
#' }
overplot <- function(dat_in, ...) UseMethod('overplot') 

#' @rdname overplot
#' 
#' @export
#' 
#' @method overplot swmpr
overplot.swmpr <- function(dat_in, select = NULL, subset = NULL, operator = NULL, ylabs = NULL, xlab = NULL, cols = NULL, lty = NULL, lwd = NULL, pch = NULL, type = NULL, ...){
  
  # get parameters to select if null, remove qaqc cols
  if(is.null(select)) 
    select <- attr(dat_in, 'parameters')[c(1, 2)]
  if(attr(dat_in, 'qaqc_cols'))
    dat_in <- qaqc(dat_in)
  
  # subset based on input, convert to data frame for default method
  toplo <- subset(dat_in, select = select, subset = subset, operator = operator)
  toplo <- as.data.frame(toplo)  
  
  overplot(toplo, date_var = 'datetimestamp', select = select, ylab = ylabs, xlab = xlab, cols = cols, lty = lty, lwd = lwd, pch = pch, type = type, ...)
  
}

#' @rdname overplot
#' 
#' @export
#' 
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics axis axis.POSIXct legend mtext par
#' 
#' @method overplot default
overplot.default <- function(dat_in, date_var, select = NULL, ylabs = NULL, xlab = NULL, cols = NULL, lty = NULL, lwd = NULL, inset = -0.15, cex = 1, xloc = 'top', yloc = NULL, pch = NULL, type = NULL, ...){
  
  if(!inherits(dat_in[, date_var], 'POSIXct')) 
    stop('date_var must be POSIXct class')
  
  # subset data if needed
  dat_in <- dat_in[, c(date_var, select)]
  
  # number of vars to plot
  lnsel <- length(select)
  
  # fill missing arguments if not supplied
  if(!is.null(cols) & length(cols) ==1) cols <- rep(cols, lnsel)
  if(is.null(cols))
    cols <- colorRampPalette(gradcols())(lnsel)
  if(is.null(lwd)){
    lwd <- rep(1, lnsel)
  } else {
    if(length(lwd) == 1) lwd <- rep(lwd, lnsel)
    if(length(lwd) != lnsel) stop('lwd must have length equal to 1 or variables to select')
  }
  if(is.null(lty)){
    lty <- seq(1, lnsel)
  } else {
    if(length(lty) == 1) lty <- rep(lty, lnsel)
    if(length(lty) != lnsel) stop('lty must have length equal to 1 or variables to select')
  }
  if(is.null(ylabs))
    ylabs <- select
  if(is.null(xlab))
    xlab <- 'DateTimeStamp'
  if(is.null(pch)) {
    pch <- seq(1, lnsel)
  } else {
    if(length(pch) == 1) pch <- rep(pch, lnsel)
    if(length(pch) != lnsel) stop('pch must have length equal to 1 or variables to select')
  }
  if(is.null(type)){
    type <- rep('l', lnsel)
  } else {
    if(length(type) == 1) type <- rep(type, lnsel)
    if(length(type) != lnsel) stop('type must have length equal to 1 or variables to select')
  }
  
  # x dimension extension for multiple yaxix labels
  xext <- 4 * lnsel
  par(mar = c(5.1, xext, 4.1, 2.1))
  
  toplo <- dat_in
  
  # base plot
  plot(x = toplo[, date_var], y = toplo[, select[1]], type = 'n', axes = F, ylab = '', xlab = '')
  
  # initialize starting locations for y axis and text
  yline <- 0
  ytxtline <- 2
  
  # extension limits for y axes
  ylims <- diff(range(c(as.matrix(toplo[, select])), na.rm = TRUE))
  
  # plot each line
  for(parm in seq_along(select)){
    
    # add line to existing empty plot
    par(new = TRUE)
    yvar <- toplo[, select[parm]]
    plot(x = toplo[, date_var], y = yvar, type = type[parm], axes = F, 
         ylab = '', xlab = '', lty = lty[parm], lwd = lwd[parm], cex = lwd[parm], pch = pch[parm], col = cols[parm], ...)
    
    # add y axes and appropriate labels
    axis(side = 2, at = c(-2 * ylims, 2 * ylims), line = yline, labels = FALSE)
    axis(side = 2, line = yline)
    mtext(side = 2, text = ylabs[parm], line = ytxtline)
    
    # bump the locations for next line
    yline <- 3.5 + yline
    ytxtline <- 3.5 + ytxtline
    
  }
  
  # add x axis and label
  dtrng <- as.numeric(range(toplo[, date_var], na.rm = TRUE))
  axis.POSIXct(side = 1, x = toplo[, date_var])
  axis(side = 1, at = c(-200 * dtrng[1], 200 * dtrng[2]), labels = FALSE)
  mtext(side = 1, xlab, line = 2.5)
  
  # add legend in margin
  lty[type == 'p'] <- NA
  pch[type == 'l'] <- NA
  legend(x = xloc, y = yloc, inset = inset, cex = cex, legend = ylabs, col = cols, lty = lty, lwd = lwd, pch = pch,
         horiz = TRUE, xpd = TRUE, bty = 'n')
  
}