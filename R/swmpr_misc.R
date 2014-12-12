######
#' Create a wmpr object
#' 
#' Wrapper for creating a swmpr object
#' 
#' @param  stat_in \code{data.frame} of swmp data
#' @param  meta_in chr string for station code (7 or 8 characters), can be multiple stations if data are combined
#' 
#' @export swmpr
#' 
#' @return Returns a swmpr object to be used with S3 methods
#' 
#' @details 
#' This function is a simple wrapper to \code{\link[base]{structure}} that is used internally within other functions to create a swmpr object.  The function does not have to be used explicitly.    
#' 
swmpr <- function(stat_in, meta_in){
    
  if(!is.data.frame(stat_in)) 
    stop('stat_in must be data.frame')
  
  # qaqc attribute
  qaqc_cols <- FALSE
  if(any(grepl('^f_', names(stat_in)))) qaqc_cols <- TRUE
  
  # parameters attribute
  parameters <- grep('datetimestamp|^f_', names(stat_in), invert = TRUE, value = TRUE)
  
  # get stations, param_types attribtues
  param_types <- param_names()
  param_types <- unlist(lapply(param_types, function(x) any(x %in% parameters)))
  param_types <- names(param_names())[param_types]
  station <- grep(paste0(param_types, collapse = '|'), meta_in, value = TRUE)
  
  # timezone using time_vec function
  timezone <- time_vec(station_code = station, tz_only = TRUE)

  # create class, with multiple attributes
  structure(
    .Data = stat_in, 
    class = c('swmpr', 'data.frame'), 
    station = station,
    parameters = parameters, 
    qaqc_cols = qaqc_cols,
    date_rng = range(stat_in$datetimestamp),
    timezone = timezone, 
    stamp_class = class(stat_in$datetimestamp)
    )
  
}

######
#' Parse SOAP objects for swmpr
#' 
#' Parsing function for objects returned from SOAP server
#' 
#' @param  soap_in soap object returned from CDMO server
#' @param  parent_in chr string of parent nodes to parse
#' 
#' @import XML plyr
#' 
#' @export
#' 
#' @details 
#' This function parses XML objects returned from the CDMO SOAP server, which are further passed to \code{\link{swmpr}}.  It is used internally by the data retrieval functions, excluding \code{\link{import_local}}.  The function does not need to be called explicitly.
#' 
#' @return Returns a \code{data.frame} of parsed XML nodes
parser <- function(soap_in, parent_in = 'data'){
  
  # sanity check
  if(!'SOAPHTTPReply' %in% class(soap_in))
    stop('Input must be of class SOAPHTTPReply')
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(soap_in$content, useInternalNodes = TRUE)

  # get parent data nodes
  parents <- xpathSApply(
    raw,
    paste0('//', parent_in)
  )
  
  # get children nodes from data parents
  out <- ldply(parents, 
    .fun = function(x) getChildrenStrings(x)
    )

  # return output
  return(out)
  
}

######
#' Format SWMP datetimestamp
#'
#' Format the datetimestamp column of SWMP data
#' 
#' @param  chr_in chr string of datetimestamp vector
#' @param  station_code is chr string for station (three or more characters)
#' @param  tz_only logical that returns only the timezone, default \code{FALSE}
#' 
#' @export
#' 
#' @return  Returns a POSIX vector if \code{tz_only} is true, otherwise the timezone for a station is returned as a chr string
#' 
#' @details 
#' This function formats the datetimestamp column of SWMP data to the \code{\link[base]{POSIXct}} format and the correct timezone for a station.  Note that SWMP data do not include daylight savings and the appropriate location based on GMT offsets is used for formatting.  This function is used internally within data retrieval functions and does not need to be called explicitly.
time_vec <- function(chr_in = NULL, station_code, tz_only = FALSE){
  
  # lookup table for time zones based on gmt offset - no DST!
  gmt_tab <- data.frame(
    gmt_off=c(-4,-5,-6,-8,-9),
    tz = c('America/Virgin', 'America/Jamaica', 'America/Regina',
      'Pacific/Pitcairn', 'Pacific/Gambier'),
    stringsAsFactors = FALSE
    )
  
  # hard-coded gmt offset for each site, from metadata direct from CDMO
  sites <- c('ace', 'apa', 'cbm', 'cbv', 'del', 'elk', 
    'gnd', 'grb', 'gtm', 'hud', 'jac', 'job', 'kac', 
    'lks', 'mar', 'nar', 'niw', 'noc', 'owc', 'pdb', 
    'rkb', 'sap', 'sfb', 'sos', 'tjr', 'wel', 'wkb',
    'wqb')
  gmt_offsets <- c(-5, -5, -5, -5, -5, -8, -6, -5, -5, -5, -5, -4, 
    -9, -6, -6, -5, -5, -5, -5, -8, -5, -5, -8, -8, -8,
    -5, -6, -5)  
  
  # get timezone from above information
  gmt_offset <- gmt_offsets[which(sites %in% substr(station_code, 1, 3))]
  tzone <- gmt_tab[gmt_tab$gmt_off %in% gmt_offset, 'tz']

  # timezone only if T
  if(tz_only) return(tzone)
  
  # format datetimestamp
  out <- as.POSIXct(chr_in, tz = tzone, format = '%m/%d/%Y %H:%M')
  
  # return output
  return(out)
  
}

######
#' Obtain metadata for all stations
#' 
#' Obtain a \code{\link[base]{data.frame}} of metadata for all SWMP stations.
#' 
#' @export
#' 
#' @return A \code{data.frame} of SWMP metadata
#' 
#' @details This function uses the SSOAP package to retrieve data from the CDMO through a SOAP client interface.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}. This is the CDMO equivalent of \code{exportStationCodesXML}.
#' 
#' @examples
#' \dontrun{
#' 
#' ## retrieve metadata for all sites
#' site_codes()
#' 
#' }
site_codes <- function(){
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = TRUE,  type =  "source")
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )

  # get all station codes
  reply <- .SOAP(
    serv,
    method = 'exportStationCodesXMLNew',
    action="", 
    .convert = FALSE
    )

  # parse reply from server
  out <- parser(reply)
  out$params_reported <- tolower(out$params_reported)
  
  # return output
  return(out)
  
}

######
#' Obtain metadata for a single reserve
#'
#' Get metadata for all the stations at a single SWMP reserve
#' 
#' @param nerr_site_id chr string of site, three letters
#' 
#' @export
#' 
#' @return An abbreviated \code{data.frame} of the SWMP metadata for the requested site
#' 
#' @details This function uses the SSOAP package to retrieve data from the CDMO through a SOAP client interface.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}. This function is the CDMO equivalent of \code{NERRFilterStationCodesXMLNew}.
#' 
#' @examples
#' \dontrun{
#' 
#' ## retrieve metadata for all stations at a site
#' site_codes_ind('apa')
#' 
#' }
site_codes_ind <- function(nerr_site_id){
  
  # install SSOAP if not available
  check_soap <- require('SSOAP')
  if(!check_soap)
    install.packages("SSOAP", repos="http://www.omegahat.org/R", 
      dependencies = TRUE,  type =  "source")
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )

  # get all station codes
  reply <- .SOAP(
      serv,
      method = 'NERRFilterStationCodesXMLNew',
      NERRFilter = nerr_site_id,
      action="", 
      .convert = FALSE
      )

  # parse reply from server
  out <- parser(reply)
  out$params_reported <- tolower(out$params_reported)
  
  # return output
  return(out)
  
}

######
#' Get parameters of a given type
#'
#' Get parameter column names for each parameter type
#' 
#' @param  param_type chr string specifying \code{'nut'}, \code{'wq'}, or \code{'met'}.  Input can be one to three types.
#' 
#' @export
#' 
#' @return Returns a named list of parameters for the \code{param_type}.  The parameter names are lower-case strings of SWMP parameters and corresponding qaqc names (\code{'f_'} prefix)
#' 
#' @details
#' This function is used internally within several functions to return a list of the expected parameters for a given parameter type: nutrients, water quality, or meteorological.  It does not need to be called explicitly. 
param_names <- function(param_type = c('nut', 'wq', 'met')){
  
  # sanity check
  if(any(!param_type %in% c('nut', 'wq', 'met')))
    stop('param_type must chr string of nut, wq, or met')
  
  nut_nms <- c('po4f', 'chla_n', 'no3f', 'no2f', 'nh4f', 'no23f', 'ke_n',
    'urea')
  nut_nms <- paste0(c('', 'f_'), rep(nut_nms, each = 2))
  
  wq_nms <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl', 'depth', 
    'cdepth', 'level', 'clevel', 'ph', 'turb', 'chlfluor')
  wq_nms <- paste0(c('', 'f_'), rep(wq_nms, each = 2))
  
  met_nms <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd', 'wdir', 'sdwdir',
    'totpar', 'totprcp', 'cumprcp', 'totsorad')
  met_nms <- paste0(c('', 'f_'), rep(met_nms, each = 2))
  
  # get names for a given parameter type
  out <- sapply(param_type, function(x) get(paste0(x, '_nms')), simplify = FALSE)
  
  return(out)
  
}

#' Map a reserve
#' 
#' Create a map of all the stations in a reserve
#' 
#' @param nerr_site_id chr string of the reserve to map, first three characters used by NERRS
#' @param zoom numeric value for map zoom, passed to \code{\link[ggmap]{get_map}}
#' @param text_sz numeric value for text size of station names, passed to \code{\link[ggplot2]{geom_text}}
#' @param text_col chr string for text color of station names, passed to \code{\link[ggplot2]{geom_text}}
#' @param map_type chr string indicating the type of base map obtained from Google maps, values are \code{terrain} (default), \code{satellite}, \code{roadmap}, or \code{hybrid} 
#' 
#' @import ggmap ggplot2
#' 
#' @export
#' 
#' @details This function is a simple wrapper to functions in the ggmap package which returns a map of all of the stations at a NERRS reserve.  The \code{zoom} argument may have to be chosen through trial and error depending on the spatial extent of the reserve.  Additionally, station locations are returned using the \code{site_codes_ind} function if the computer making the request has the IP address registered with CDMO.  Otherwise, a local and possibly outdated file is used.  See the package documentation.
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting.
#' 
#' @seealso  \code{\link[ggmap]{get_map}}, \code{\link[ggmap]{ggmap}}, \code{\link[ggplot2]{ggplot}}, \code{site_codes_ind}
#' 
#' @examples
#' ## defaults
#' 
#' map_reserve('jac')
#' 
#' ## satellite map
#' 
#' map_reserve('jac', map_type = 'satellite')
#' 
#' ## roadmap
#' 
#' map_reserve('jac', map_type = 'roadmap')
#' 
#' ## hybrid
#' 
#' map_reserve('jac', map_type = 'hybrid')
map_reserve <- function(nerr_site_id, zoom = 11, text_sz = 6, text_col = 'black', map_type = 'terrain'){
  
  #removing trailing whit space in chr strings
  trim_trailing<-function(x) sub('^\\s+|\\s+$', '', x)
  
  # get site stations and locations, online or local 
  stats <- try(site_codes_ind(nerr_site_id), silent = TRUE)
  if('try-error' %in% class(stats)){
  
    warning('IP address not registered, using old station data')
    
    # find/load local file
    get_meta <- system.file('sampling_stations.csv', package = 'SWMPr')
    get_meta <- read.csv(get_meta, header = TRUE, stringsAsFactors = FALSE)
    names(get_meta) <- gsub('\\.', '_', tolower(names(get_meta)))
    
    stats <- get_meta[trim_trailing(get_meta$nerr_site_id) %in% nerr_site_id, ]
    
  }
  
  stats <- stats[grep('Active*', stats$status), ]
  stats$longitude <- -1 * as.numeric(stats$longitude)
  stats$latitude <- as.numeric(stats$latitude)
  stats$station_name <- trim_trailing(as.character(stats$station_name))
  stats$station_code <- tolower(substr(trim_trailing(as.character(stats$station_code)),1,5))
  stats <- unique(stats[, c('station_code', 'latitude', 'longitude')])
  
  # base map
  mapImageData <- get_map(
    location = c(lon = mean(stats$longitude),lat = mean(stats$latitude)),
    source = 'google',
    maptype = map_type,
    zoom = zoom,
    messaging = FALSE
    )
  
  # plot
  p <- ggmap(mapImageData,
    extent = "panel"
      ) + 
    geom_text(data = stats, aes(x = longitude, y = latitude, 
      label= station_code), size = text_sz, colour = text_col
      ) +
    ylab('Latitude') +
    xlab('Longitude')
  
  return(p)

}

#' Plot graphical summaries of SWMP data
#' 
#' Plot graphical summaries of SWMP data for individual parameters, including seasonal/annual trends and anomalies
#' 
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param years numeric vector of starting and ending years to plot, default all
#' @param ... additional arguments passed to other methods, currently not used
#' 
#' @import ggplot2 gridExtra plyr
#' 
#' @export
#' 
#' @details This function creates several graphics showing seasonal and annual trends for a given swmp parameter.  Plots include aggregated monthly distributions, monthly anomalies, and annual anomalies.  Anomalies are defined as the difference between the monthly or annual average from the grand mean.  Monthly anomalies are in relation to the grand mean for the same month across all years.  This function currently only works for water quality and weather data.  An interactive Shiny widget is available: \url{https://beckmw.shinyapps.io/swmp_summary/}
#' 
#' @return A graphics object (Grob) of multiple \code{\link[ggplot2]{ggplot}} objects.
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}
#' 
#' @examples
#' ## import data
#' path <- system.file('zip_ex', package = 'SWMPr')
#' dat <- import_local(path, 'apacpwq')
#' dat <- qaqc(dat)
#' 
#' ## plot
#' plot_summary(dat, param = 'temp')
#' 
#' ## plot one year
#' plot_summary(dat, param = 'temp', years = 2011))   
plot_summary <- function(swmpr_in, ...) UseMethod('plot_summary') 

#' @rdname plot_summary
#' 
#' @export plot_summary.swmpr
#' 
#' @method plot_summary swmpr
plot_summary.swmpr <- function(swmpr_in, param, years = NULL, ...){
  
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
  if('nut' %in% substring(stat, 6)) stop('Function does not work with nutrients data')
  
  ##
  # preprocessing
  
  # get daily averages for quicker plots, add year/month columns
  dat <- aggregate.swmpr(swmpr_in, by = 'days', params = param)
  
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
    totsorad = 'Total solar radiation (watts/m2)'
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
  p1 <- ggplot(dat_plo, aes_string(x = 'month', y = param)) +
    geom_point(size = 2, alpha = 0.5, 
      position=position_jitter(width=0.1)
      ) +
    theme_classic() +
    ylab(ylab) + 
    xlab('Monthly distributions and means') +
    geom_point(data = mo_agg, aes_string(x = 'month', y = param), 
      colour = 'darkgreen', fill = cols, size = 7, pch = 21) + 
    my_theme
  
  # box aggs, colored by median
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg_med))
  cols <- cols[rank(mo_agg_med[, param])]
  p2 <- ggplot(dat_plo, aes_string(x = 'month', y = param)) + 
    geom_boxplot(fill = cols) +
    theme_classic() +
    ylab(ylab) + 
    xlab('Monthly distributions and medians') +
    my_theme
  
  # month histograms
  to_plo <- dat_plo
  to_plo$month <- factor(to_plo$month, levels = rev(mo_levs), labels = rev(mo_labs))
  p3 <- ggplot(to_plo, aes_string(x = param)) + 
    geom_histogram(aes(y = ..density..), colour = 'lightblue', binwidth = diff(range(to_plo[, param], na.rm = T))/30) + 
    facet_grid(month ~ .) + 
    xlab(ylab) +
    theme_bw(base_family = 'Times') + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      strip.text.y = element_text(size = 8, angle = 90),
      strip.background = element_rect(size = 0, fill = 'lightblue')) +
    my_theme
  
  # monthly means by year
  to_plo <- plyr::ddply(dat_plo, 
    .variables = c('month', 'year'), 
    .fun = function(x) mean(x[, param],  na.rm = T)
    )
  to_plo$month <- factor(to_plo$month, labels = mo_labs, level = mo_levs)
  midpt <- mean(to_plo$V1, na.rm = T)
  p4 <- ggplot(to_plo, aes(x = year, y = month, fill = V1)) +
    geom_tile() +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = midpt) +
    theme_classic() +
    ylab('Monthly means') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight = 0.5)) +
    my_theme
  
  # monthly anomalies
  mo_agg$month <- factor(mo_agg$month, labels = mo_labs, levels = mo_levs)
  to_plo <- merge(to_plo, mo_agg, by = 'month', all.x = T)
  names(to_plo)[names(to_plo) %in% param] <- 'trend'
  to_plo$anom <- with(to_plo, V1 - trend)
  rngs <- max(abs(range(to_plo$anom, na.rm = T)))
  p5 <- ggplot(to_plo, aes(x = year, y = month, fill = anom)) +
    geom_tile() +
    scale_fill_gradient2(name = ylab,
      low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
      limits = c(-1 * rngs, rngs)) +
    theme_classic() +
    ylab('Monthly anomalies') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight= 0.5)) +
    my_theme
  
  # annual anomalies
  yr_avg <- mean(yr_agg[, param], na.rm = T)
  yr_agg$anom <- yr_agg[, param] - yr_avg
  p6 <- ggplot(yr_agg, aes(x = year, y = anom, group = 1, fill = anom)) +
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

  ##
  # combine plots
  suppressWarnings(gridExtra::grid.arrange(
    arrangeGrob(p1, p2, ncol = 1), 
    p3, 
    arrangeGrob(p4, p5, p6, ncol = 1, heights = c(1, 1, 0.8)), 
    ncol = 3, widths = c(1, 0.5, 1)
  ))

}