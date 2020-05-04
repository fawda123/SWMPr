#' Create a plot of data for a single year overlaid on historical data.
#' 
#' A line for a single year is plotted over ribbons ofquantiles for historical data.
#' 
#' @param swmpr_in input swmpr object.
#' @param paramtoplot chr string of parameter to plot
#' @param yr numeric of year to feature as a line on the plot
#' @param yrstart numeric of year to begin range of comparison data
#' @param yrend numeric of year to end range of comparison data
#' @param yaxislab chr string for y-axis albel. Default is \code{paramtoplot}.
#' @param maintitle chr string of plot title. Default pastes together site name, parameter name, year to feature, and range of years to use for comparison, e.g. 'GNDBHWQ 2017 Daily Average Temp overlaid on 2006-2016 daily averages'.
#' @param yrcolor chr string of line color for year of interest
#' @param bgcolor1 chr string of color for outer 50\% of data range
#' @param bgcolor2 chr string of color for middle 50\% of data range.
#' @param ... additional arguments passed to or from other methods
#' 
#' @details 
#' The plot is based on aggregates of daily average values for the entire time series. Quantiles (min, 25\%, 75\%, max) for each individual calendar day (01/01, 01/02, ... 12/31) are used to generate a ribbon plot of historical data and the selected year in \code{yr} is plotted as a line over the ribbon for historical context.
#' 
#' required packages: dplyr, lubridate, ggplot2, tibble
#' 
#' @author Kimberly Cressman, Marcus Beck
#' 
#' @concept analyze
#' 
#' @export
#'
#' @return A a \code{\link[ggplot2]{ggplot2}} object.
#'
#' @import ggplot2
#' 
#' @examples 
#' # qaqc
#' dat <- qaqc(apacpwq)
#' 
#' # generate a plot of salinity for 2013 overlaid on 2012-2013 data
#' plot_quants(dat, 'sal', yr = 2013, yrstart = 2012, yrend = 2013)
#' 
#' # change some of the defaults
#' plot_quants(dat, 'sal', yr = 2013, yrstart = 2012, yrend = 2013, 
#'  bgcolor1 = 'lightsteelblue2', bgcolor2 = 'lightsteelblue4', 
#'  yaxislab = 'Salinity (psu)')
plot_quants <- function(swmpr_in, ...) UseMethod('plot_quants')

#' @rdname plot_quants
#'
#' @export
#' 
#' @method plot_quants swmpr
plot_quants.swmpr <- function(swmpr_in, paramtoplot, yr, yrstart, yrend, yaxislab = NULL, yrcolor = 'red3',   bgcolor1 = 'lightgray', bgcolor2 = 'gray65', maintitle = NULL, ...){
  
  # swmpr attributes
  station  <- attr(swmpr_in, 'station')
  timezone <- attr(swmpr_in, 'timezone')
  
  # yaxislab is paramtoplot if not provided
  if(is.null(yaxislab)) yaxislab <- paramtoplot
  
  # plot title
  if(is.null(maintitle))
    maintitle <- paste0(station, ' ', yr, ' Daily Average ', 
                        paste0(toupper(substr(paramtoplot, 1, 1)), substr(paramtoplot, 2, nchar(paramtoplot))), '\noverlaid on ', 
                        yrstart, ' - ', yrend, ' daily averages')
  
  # pull out daily averages; name it 'dat'
  dat <- aggreswmp(swmpr_in, by = 'days', FUN = 'mean')
  
  # make a column for just mm-dd, and another column for year
  dat$month <- strftime(dat$datetimestamp, '%m')   
  dat$day <- strftime(dat$datetimestamp, '%d')
  dat$year <- strftime(dat$datetimestamp, '%Y')
  dat$monthday <- as.character(paste0(dat$month, '-', dat$day))
  
  # graphing ----
  
  # split into feature year (for red) and backdrop years(to all be gray)
  # this uses the filter() function of dplyr to subset on year
  dat_feature <- dplyr::filter(dat, year == yr)
  dat_backdrop <- dplyr::filter(dat, year >= yrstart & year <= yrend)
  
  # work with quantiles ----
  
  # do some subsetting and sorting on backdrop data (using dplyr)
  # need to pull this out so a column can be named 'paramtoplot'
  # which makes later coding easier
  dat_quantiles <- dplyr::select_(dat_backdrop, paramtoplot = paramtoplot, 'monthday', 'year')
  
  # by_doy <- group_by(dat_quantiles, monthday)
  
  # subset featured data (using dplyr)
  dat_feature <- dplyr::select_(dat_feature, paramtoplot = paramtoplot, 'monthday', 'year')
  
  # generate a summary table (using dplyr and tibble) ----
  
  # first gather quantiles for every monthday
  # could probably make this quantile(x, n, na.rm = TRUE)
  # where n is a vector that the user inputs at the beginning of the function to pull out whatever percentiles they want
  doy_sum2 <- tapply(dat_quantiles$paramtoplot, dat_quantiles$monthday, 
                     function(x) quantile(x, c(0, 0.25, 0.75, 1), na.rm = TRUE))
  # that spits out a list.
  # pull the list together with do.call and rbind:
  doy_sum3 <- do.call(rbind, doy_sum2)
  # but that's a matrix, so make it a data frame:
  doy_sum3 <- data.frame(doy_sum3)
  
  # and turn the row names into the column 'monthday', using tibble::rownames_to_column(), and name it doy_sum
  doy_sum <- data.frame(monthday = rownames(doy_sum3), doy_sum3, stringsAsFactors = FALSE)
  
  # join the two data frames into one (using dplyr)
  all_doy <- dplyr::full_join(dat_feature, doy_sum, by = 'monthday')
  
  # get monthday back into date format by adding a year and then using lubridate's mdy function
  # using 2008 as the arbitrary year so leap days go where they should
  all_doy$monthday <- paste0(all_doy$monthday, '-2008')
  all_doy$monthday <- as.Date(all_doy$monthday, format = '%m-%d-%Y')
  
  # all_doy$monthday <- 
  # make a year label for the legend
  yrlabel <- as.character(yr)
  
  # make a ribbon plot ----
  p <- ggplot(all_doy) +
    geom_ribbon(aes(x = monthday, ymin = X0., ymax = X100., fill = 'historical min-max')) +
    geom_ribbon(aes(x = monthday, ymin = X25., ymax = X75., fill = 'historical 25-75 %iles')) +
    geom_line(aes(x = monthday, y = paramtoplot, color = yrcolor), lwd = 1.3) +
    theme_minimal() +
    scale_x_date(date_labels = '%m/%d', date_breaks = '1 month', date_minor_breaks = '1 month') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = 'Day of Year', 
         y = yaxislab, 
         title = maintitle) +
    scale_color_manual(name = '',  values = yrcolor, labels = yrlabel) +
    scale_fill_manual(name = '', values = c('historical min-max' = bgcolor1, 'historical 25-75 %iles' = bgcolor2))
  
  return(p)
  
}