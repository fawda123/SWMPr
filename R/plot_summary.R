#' Plot graphical summaries of SWMP data
#' 
#' Plot graphical summaries of SWMP data for individual parameters, including seasonal/annual trends and anomalies
#' 
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param years numeric vector of starting and ending years to plot, default all
#' @param plt_sep logical if a list is returned with separate plot elements
#' @param sum_out logical if summary data for the plots is returned
#' @param ... additional arguments passed to other methods, currently not used
#' 
#' @import ggplot2 gridExtra
#' 
#' @importFrom stats aggregate as.formula formula median na.pass
#' @importFrom grDevices colorRampPalette
#' 
#' @export
#' 
#' @concept analyze
#' 
#' @details This function creates several graphics showing seasonal and annual trends for a given swmp parameter.  Plots include monthly distributions, monthly anomalies, and annual anomalies in multiple formats.  Anomalies are defined as the difference between the monthly or annual average from the grand mean.  Monthly anomalies are in relation to the grand mean for the same month across all years.  All data are aggregated for quicker plotting.  Nutrient data are based on monthly averages, wheras weather and water quality data are based on daily averages.  Cumulative precipitation data are based on the daily maximum.  An interactive Shiny widget is available: \url{https://beckmw.shinyapps.io/swmp_summary/}
#' 
#' Individual plots can be obtained if \code{plt_sep = TRUE}.  Individual plots for elements one through six in the list correspond to those from top left to bottom right in the combined plot.
#' 
#' Summary data for the plots can be obtained if \code{sum_out = TRUE}.  This returns a list with three data frames with names \code{sum_mo}, \code{sum_moyr}, and \code{sum_mo}.  The data frames match the plots as follows: \code{sum_mo} for the top left, bottom left, and center plots, \code{sum_moyr} for the top right and middle right plots, and \code{sum_yr} for the bottom right plot. 
#' 
#' @return A graphics object (Grob) of multiple \code{\link[ggplot2]{ggplot}} objects, otherwise a list of  individual \code{\link[ggplot2]{ggplot}} objects if \code{plt_sep = TRUE} or a list with data frames of the summarized data if \code{sum_out = TRUE}.
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}
#' 
#' @examples
#' ## import data
#' data(apacpnut)
#' dat <- qaqc(apacpnut)
#' 
#' ## plot
#' plot_summary(dat, param = 'chla_n', years = c(2007, 2013))
#' 
#' ## get individaul plots
#' plots <- plot_summary(dat, param = 'chla_n', years = c(2007, 2013), plt_sep = TRUE)
#' 
#' plots[[1]] # top left
#' plots[[3]] # middle
#' plots[[6]] # bottom right
#' 
#' ## get summary data
#' plot_summary(dat, param = 'chla_n', year = c(2007, 2013), sum_out = TRUE)
#' 
plot_summary <- function(swmpr_in, ...) UseMethod('plot_summary') 

#' @rdname plot_summary
#' 
#' @export
#' 
#' @method plot_summary swmpr
plot_summary.swmpr <- function(swmpr_in, param, years = NULL, plt_sep = FALSE, sum_out = FALSE, ...){
  
  stat <- attr(swmpr_in, 'station')
  parameters <- attr(swmpr_in, 'parameters')
  date_rng <- attr(swmpr_in, 'date_rng')
  
  # sanity checks
  if(is.null(years)){
    years <- as.numeric(as.character(strftime(date_rng, '%Y')))
  } else {
    if(length(years) > 2) stop('One or two element year vector is required.')
    if(length(years) == 1) years <- c(years, years)
  }
  if(!param %in% parameters) stop('param must be included in the data')
  
  ##
  # preprocessing
  
  ## aggregate by averages for quicker plots
  # nuts are monthly
  if(grepl('nut$', stat)){
    dat <- aggreswmp(swmpr_in, by = 'months', params = param)
  }
  
  # wq is monthly
  if(grepl('wq$', stat)){
    dat <- aggreswmp(swmpr_in, by = 'days', params = param)
  }
  
  # met is monthly, except cumprcp which is daily max
  if(grepl('met$', stat)){
    dat <- aggreswmp(swmpr_in, by = 'days', params = param)
    
    # summarize cumprcp as max if present
    if('cumprcp' %in% attr(swmpr_in, 'parameters')){
      cumprcp <- aggreswmp(swmpr_in, by = 'days', FUN = function(x) max(x, na.rm = TRUE), 
                           params = 'cumprcp')
      dat$cumprcp <- cumprcp$cumprcp
    }
    
  }
  
  mo_labs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  mo_levs <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  dat$year <- strftime(dat$datetimestamp, '%Y')
  dat$month <- strftime(dat$datetimestamp, '%m')
  dat$month <- factor(dat$month, labels = mo_levs, levels = mo_levs)
  
  # select years to plot
  dat_plo <- data.frame(dat[dat$year %in% seq(years[1], years[2]), ])
  
  # label lookups
  lab_look <- list(
    temp = 'Temperature (C)', 
    spcond = 'Specific conductivity (mS/cm)',
    sal = 'Salinity (psu)',
    do_pct = 'Dissolved oxyxgen (%)',
    do_mgl = 'Dissolved oxygen (mg/L)',
    depth = 'Depth (m)',
    cdepth = 'Depth (nonvented, m)',
    level = 'Referenced depth (m)',
    clevel = 'Referenced depth (nonvented, m)',
    ph = 'pH',
    turb = 'Turbidity (NTU)',
    chlfluor = 'Chl fluorescence (ug/L)',
    atemp = 'Air temperature (C)',
    rh = 'Relative humidity (%)',
    bp = 'Barometric pressure (mb)',
    wspd = 'Wind speed (m/s)',
    maxwspd = 'Max wind speed (m/s)',
    wdir = 'Wind direction (degrees)',
    sdwdir = 'Wind direction (sd, degrees)',
    totpar = 'Total PAR (mmol/m2)',
    totprcp = 'Total precipitation (mm)',
    cumprcp = 'Cumulative precipitation (mm)',
    totsorad = 'Total solar radiation (watts/m2)',
    po4f = 'Orthophosphate (mg/L)', 
    nh4f = 'Ammonium (mg/L)',
    no2f = 'Nitrite (mg/L)',
    no3f = 'Nitrate (mg/L)',
    no23f = 'Nitrite + Nitrate (mg/L)',
    chla_n = 'Chlorophyll (ug/L)'
  )
  ylab <- lab_look[[param]]
  
  # monthly, annual aggs
  agg_fun <- function(x) mean(x, na.rm = T)
  form_in <- formula(paste0(param, ' ~ month'))
  mo_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = agg_fun)
  mo_agg_med <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = function(x) median(x, na.rm = T))
  form_in <- formula(paste0(param, ' ~ year'))
  yr_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'month')], FUN = agg_fun, na.action = na.pass)
  
  ##
  # plots
  
  # universal plot setting
  my_theme <- theme(axis.text = element_text(size = 8))
  
  # plot 1 - means and obs
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg))
  cols <- cols[rank(mo_agg[, param])]
  p1 <- suppressWarnings({ggplot(dat_plo, aes_string(x = 'month', y = param)) +
      geom_point(size = 2, alpha = 0.5, 
                 position=position_jitter(width=0.1)
      ) +
      theme_classic() +
      ylab(ylab) + 
      xlab('Monthly distributions and means') +
      geom_point(data = mo_agg, aes_string(x = 'month', y = param), 
                 colour = 'darkgreen', fill = cols, size = 7, pch = 21) + 
      my_theme
  })
  
  # box aggs, colored by median
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg_med))
  cols <- cols[rank(mo_agg_med[, param])]
  p2 <- suppressWarnings({ggplot(dat_plo, aes_string(x = 'month', y = param)) + 
      geom_boxplot(fill = cols) +
      theme_classic() +
      ylab(ylab) + 
      xlab('Monthly distributions and medians') +
      my_theme
  })
  
  # month histograms
  to_plo <- dat_plo
  to_plo$month <- factor(to_plo$month, levels = rev(mo_levs), labels = rev(mo_labs))
  p3 <- suppressWarnings({ggplot(to_plo, aes_string(x = param)) + 
      geom_histogram(aes_string(y = '..density..'), colour = 'lightblue', binwidth = diff(range(to_plo[, param], na.rm = T))/30) + 
      facet_grid(month ~ .) + 
      xlab(ylab) +
      theme_bw(base_family = 'Times') + 
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            strip.text.y = element_text(size = 8, angle = 90),
            strip.background = element_rect(size = 0, fill = 'lightblue')) +
      my_theme
  })
  
  # monthly means by year
  to_plo <- dat_plo[, names(dat_plo) %in% c('month', 'year', param)]
  form_in <- as.formula(paste(param, '~ .'))
  to_plo <- aggregate(form_in, to_plo, function(x) mean(x, na.rm = T),
                      na.action = na.pass)
  
  to_plo$month <- factor(to_plo$month, labels = mo_labs, levels = mo_levs)
  names(to_plo)[names(to_plo) %in% param] <- 'V1'
  midpt <- mean(to_plo$V1, na.rm = T)
  p4 <- suppressWarnings({ggplot(subset(to_plo, !is.na(to_plo$V1)), 
                                 aes_string(x = 'year', y = 'month', fill = 'V1')) +
      geom_tile() +
      geom_tile(data = subset(to_plo, is.na(to_plo$V1)), 
                aes(x = year, y = month), fill = NA
      )  +
      scale_fill_gradient2(name = ylab,
                           low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = midpt) +
      theme_classic() +
      ylab('Monthly means') +
      xlab('') +
      theme(legend.position = 'top', legend.title = element_blank()) +
      guides(fill = guide_colorbar(barheight = 0.5)) +
      my_theme
  })
  
  # monthly anomalies
  mo_agg$month <- factor(mo_agg$month, labels = mo_labs, levels = mo_levs)
  to_plo <- merge(to_plo, mo_agg, by = 'month', all.x = T)
  names(to_plo)[names(to_plo) %in% param] <- 'trend'
  to_plo$anom <- with(to_plo, V1 - trend)
  rngs <- max(abs(range(to_plo$anom, na.rm = T)))
  p5 <- suppressWarnings({ggplot(subset(to_plo, !is.na(to_plo$anom)), 
                                 aes_string(x = 'year', y = 'month', fill = 'anom')) +
      geom_tile() +
      geom_tile(data = subset(to_plo, is.na(to_plo$anom)), 
                aes(x = year, y = month), fill = NA
      ) +
      scale_fill_gradient2(name = ylab,
                           low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
                           limits = c(-1 * rngs, rngs)) +
      theme_classic() +
      ylab('Monthly anomalies') +
      xlab('') +
      theme(legend.position = 'top', legend.title = element_blank()) +
      guides(fill = guide_colorbar(barheight= 0.5)) +
      my_theme
  })
  
  # annual anomalies
  yr_avg <- mean(yr_agg[, param], na.rm = T)
  yr_agg$anom <- yr_agg[, param] - yr_avg
  p6 <- suppressWarnings({ggplot(yr_agg, 
                                 aes_string(x = 'year', y = 'anom', group = '1', fill = 'anom')) +
      geom_bar(stat = 'identity') +
      scale_fill_gradient2(name = ylab,
                           low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0
      ) +
      stat_smooth(method = 'lm', se = F, linetype = 'dashed', size = 1) +
      theme_classic() +
      ylab('Annual anomalies') +
      xlab('') +
      theme(legend.position = 'none') +
      my_theme
  })
  
  # return plot list if TRUE
  if(plt_sep) return(list(p1, p2, p3, p4, p5, p6))
  
  # return summary list if TRUE
  if(sum_out){
    
    # month summaries
    sum_mo <- split(dat_plo, dat_plo$month)
    sum_mo <- lapply(sum_mo, function(x){
      
      vr <- var(x[, param], na.rm = TRUE)
      summ <- summary(x[, param])
      names(summ)[1:6] <- c('min', 'firstq', 'med', 'mean', 'thirdq', 'max')
      
      # manually add NA if not present
      if(length(summ) == 6)
        c(summ, `NA.s` = 0, var = vr)
      else 
        c(summ, var = vr)
      
    })
    sum_mo <- do.call('rbind', sum_mo)
    sum_mo <- data.frame(month = rownames(sum_mo), sum_mo)
    sum_mo$month <- factor(sum_mo$month, levels = mo_levs, labels = mo_labs)
    row.names(sum_mo) <- 1:nrow(sum_mo)
    
    # month, yr summaries
    sum_moyr <- to_plo
    names(sum_moyr)[names(sum_moyr) %in% 'V1'] <- 'mean'
    sum_moyr <- sum_moyr[with(sum_moyr, order(year, month)), ]
    row.names(sum_moyr) <- 1:nrow(sum_moyr)
    
    # annual summaries
    sum_yr <- yr_agg
    names(sum_yr)[names(sum_yr) %in% param] <- 'mean'
    
    return(list(sum_mo = sum_mo, sum_moyr = sum_moyr, sum_yr = sum_yr))
    
  }
  
  ##
  # combine plots
  suppressWarnings(gridExtra::grid.arrange(
    arrangeGrob(p1, p2, ncol = 1), 
    p3, 
    arrangeGrob(p4, p5, p6, ncol = 1, heights = c(1, 1, 0.8)), 
    ncol = 3, widths = c(1, 0.5, 1)
  ))
  
}