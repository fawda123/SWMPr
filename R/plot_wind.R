#' Create a wind rose 
#'
#' Create a wind rose from met data
#' 
#' @param swmpr_in input swmpr object
#' @param years numeric of years to plot, defaults to most recent
#' @param angle numeric for the number of degrees occupied by each spoke
#' @param width numeric for width of paddles if \code{paddle = TRUE}
#' @param breaks numeric for the number of break points in the wind speed
#' @param paddle logical for paddles at the ends of the spokes 
#' @param grid.line numeric for grid line interval to use
#' @param max.freq numeric for the scaling used to set the maximum value of the radial limits (like zoom)
#' @param cols chr string for colors to use for plotting, can be any palette R recognizes or a collection of colors as a vector
#' @param annotate logical indicating if text is shown on the bottom of the plot for the percentage of observations as 'calm' and mean values
#' @param main chr string for plot title, defaults to station name and year plotted
#' @param type chr string for temporal divisions of the plot, defaults to whole year.  See details. 
#' @param between list for lattice plot options, defines spacing between plots
#' @param par.settings list for optional plot formatting passed to \code{\link[lattice]{lattice.options}}
#' @param strip list for optional strip formatting passed to \code{\link[lattice]{strip.custom}}
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details This function is a convenience wrapper to \code{\link[openair]{windRose}}.  Most of the arguments are taken directly from this function.
#' 
#' The \code{type} argument can be used for temporal divisions of the plot.  Options include the entire year (\code{type = "default"}), seasons (\code{type = "season"}), months (\code{type = "month"}), or weekdays (\code{type = "weekday"}).  Combinations are also possible (see \code{\link[openair]{windRose}}).
#' 
#' @author Kimberly Cressman, Marcus Beck
#' 
#' @concept analyze
#' 
#' @return A wind rose plot
#' 
#' @examples 
#' plot_wind(apaebmet)
plot_wind <- function(swmpr_in, ...) UseMethod('plot_wind')

#' @rdname plot_wind
#'
#' @export
#' 
#' @method plot_wind swmpr
plot_wind.swmpr <- function(swmpr_in, years = NULL, angle = 45, width = 1.5, breaks = 5, paddle = FALSE, grid.line = 10, max.freq = 30, cols = 'GnBu', annotate = FALSE, main = NULL, type = 'default', between = list(x = 1, y = 1), par.settings = NULL, strip = NULL, ...){
  
  dat <- swmpr_in
  station <- attr(dat, 'station')
  dat$date <- dat$datetimestamp
  dat$yrs <- as.numeric(strftime(dat$datetimestamp, '%Y'))
  
  # fill par settings with default
  if(is.null(par.settings))
    par.settings <- list(axis.line = list(col = 'darkgray'),
                         par.main.text = list(cex=1.1),
                         strip.border = list(col = 'darkgray'),
                         layout.widths = list(right.padding = 3),
                         layout.heights = list(top.padding = 3)
    )
  
  # fill strip settings with default
  if(is.null(strip))
    strip <- lattice::strip.custom(par.strip.text=list(cex=0.8, fontface='bold'))
  
  # yrs as latest
  if(is.null(years)){
    years <- unique(dat$yrs)
    years <- years[which.max(years)]
  }
  
  #subset by yrs
  if(any(!years %in% dat$yrs)) 
    stop('Check years')
  dat <- dat[dat$yrs %in% years, ]
  
  # name from input object
  if(is.null(main)){
    
    check_sta <- grepl('met$', station)
    if(!check_sta)
      stop('No weather data')
    
    station <- station[check_sta]
    main <- paste(station, paste(years, collapse = ', '), sep = ', ')
    
  }
  
  openair::windRose(dat,  ws ='wspd', wd ='wdir', 
                    angle = angle, 
                    width = width, 
                    breaks = breaks, 
                    paddle = paddle,  
                    grid.line = grid.line, 
                    max.freq = max.freq,
                    cols = cols,
                    annotate = annotate,
                    main = main,
                    type = type, 
                    between = between,
                    par.settings = par.settings, 
                    strip = strip,
                    ...
  )
  
}