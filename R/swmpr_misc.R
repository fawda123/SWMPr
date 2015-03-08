######
#' Create a swmpr object
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
#' This function is a simple wrapper to \code{\link[base]{structure}} that is used internally within other functions to create a swmpr object.  The function does not have to be used explicitly.  Attributes of a swmpr object include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
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
#' Parse web results for swmpr
#' 
#' Parsing function for objects returned from CDMO web services
#' 
#' @param  resp_in web object returned from CDMO server, response class from httr package
#' @param  parent_in chr string of parent nodes to parse
#' 
#' @import XML plyr
#' 
#' @export
#' 
#' @details 
#' This function parses XML objects returned from the CDMO server, which are further passed to \code{\link{swmpr}}.  It is used internally by the data retrieval functions, excluding \code{\link{import_local}}.  The function does not need to be called explicitly.
#' 
#' @return Returns a \code{data.frame} of parsed XML nodes
parser <- function(resp_in, parent_in = 'data'){
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(resp_in, useInternalNodes = TRUE)

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
#' @details This function retrieves data from the CDMO web services.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}. This is the CDMO equivalent of \code{exportStationCodesXML}.
#' 
#' @examples
#' \dontrun{
#' 
#' ## retrieve metadata for all sites
#' site_codes()
#' 
#' }
site_codes <- function(){
  
  # access CDMO web services
  serv <- "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"

  # get all station codes
  reply <- httr::GET(
    serv,
    query = list(method = 'exportStationCodesXMLNew'),
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
#' @details This function retrieves data from the CDMO web services.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}. This function is the CDMO equivalent of \code{NERRFilterStationCodesXMLNew}.
#' 
#' @examples
#' \dontrun{
#' 
#' ## retrieve metadata for all stations at a site
#' site_codes_ind('apa')
#' 
#' }
site_codes_ind <- function(nerr_site_id){
  
  # access CDMO web services
  serv <- "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"

  # get all station codes
  reply <- httr::GET(
    serv,
    query = list(  
      method = 'NERRFilterStationCodesXMLNew',
      NERRFilter = nerr_site_id
    )
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
#' @details This function is a simple wrapper to functions in the ggmap package which returns a map of all of the stations at a NERRS reserve.  The \code{zoom} argument may have to be chosen through trial and error depending on the spatial extent of the reserve.  A local data file included with the package is used to get the latitude and longitude values of each station.  The files includes only active stations as of January 2015.
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting.
#' 
#' @seealso  \code{\link[ggmap]{get_map}}, \code{\link[ggmap]{ggmap}}, \code{\link[ggplot2]{ggplot}}
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
  
  # get site stations and locations, local 
  data(stat_locs)
  
  # subset stat_locs by reserve
  stats <- stat_locs[grepl(paste0('^', nerr_site_id), stat_locs$station_code), ]
  
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

######
#' Locations of NERRS sites
#'
#' Location of NERRS sites in decimal degress.  Only active sites as of January 2015 are included.  Sites are identified by five letters indicing the reserve and site names.  The dataset is used to plot locations with the \code{\link{map_reserve}} function. 
#' 
#' @format A \code{\link[base]{data.frame}} object with 161 rows and 3 variables:
#' \describe{
#'   \item{\code{station_code}}{chr}
#'   \item{\code{latitude}}{numeric}
#'   \item{\code{longitude}}{numeric}
#' }
#' 
#' @seealso \code{\link{map_reserve}} 
"stat_locs"

######
#' Example nutrient data for Apalachicola Bay Cat Point station.
#'
#' An example nutrient dataset for Apalachicola Bay Cat Point station.  The data are a \code{\link{smwpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 215 rows and 13 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{po4f}}{num}
#'   \item{\code{f_po4f}}{chr}
#'   \item{\code{nh4f}}{num}
#'   \item{\code{f_nh4f}}{chr}
#'   \item{\code{no2f}}{num}
#'   \item{\code{f_no2f}}{chr}
#'   \item{\code{no3f}}{num}
#'   \item{\code{f_no3f}}{chr}
#'   \item{\code{no23f}}{num}
#'   \item{\code{f_no23f}}{chr}
#'   \item{\code{chla_n}}{num}
#'   \item{\code{f_chla_n}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
"apacpnut"

######
#' Example water quality data for Apalachicola Bay Cat Point station.
#'
#' An example water quality dataset for Apalachicola Bay Cat Point station.  The data are a \code{\link{smwpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 70176 rows and 25 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{temp}}{num}
#'   \item{\code{f_temp}}{chr}
#'   \item{\code{spcond}}{num}
#'   \item{\code{f_spcond}}{chr}
#'   \item{\code{sal}}{num}
#'   \item{\code{f_sal}}{chr}
#'   \item{\code{do_pct}}{num}
#'   \item{\code{f_do_pct}}{chr}
#'   \item{\code{do_mgl}}{num}
#'   \item{\code{f_do_mgl}}{chr}
#'   \item{\code{depth}}{num}
#'   \item{\code{f_depth}}{chr}
#'   \item{\code{cdepth}}{num}
#'   \item{\code{f_cdepth}}{chr}
#'   \item{\code{level}}{num}
#'   \item{\code{f_level}}{chr}
#'   \item{\code{clevel}}{num}
#'   \item{\code{f_clevel}}{chr}
#'   \item{\code{ph}}{num}
#'   \item{\code{f_ph}}{chr}
#'   \item{\code{turb}}{num}
#'   \item{\code{f_turb}}{chr}
#'   \item{\code{chlfluor}}{num}
#'   \item{\code{f_chlfluor}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
"apacpwq"

######
#' Example water quality data for Apalachicola Bay Dry Bar station.
#'
#' An example water quality dataset for Apalachicola Bay Dry Bar station.  The data are a \code{\link{smwpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 70176 rows and 25 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{temp}}{num}
#'   \item{\code{f_temp}}{chr}
#'   \item{\code{spcond}}{num}
#'   \item{\code{f_spcond}}{chr}
#'   \item{\code{sal}}{num}
#'   \item{\code{f_sal}}{chr}
#'   \item{\code{do_pct}}{num}
#'   \item{\code{f_do_pct}}{chr}
#'   \item{\code{do_mgl}}{num}
#'   \item{\code{f_do_mgl}}{chr}
#'   \item{\code{depth}}{num}
#'   \item{\code{f_depth}}{chr}
#'   \item{\code{cdepth}}{num}
#'   \item{\code{f_cdepth}}{chr}
#'   \item{\code{level}}{num}
#'   \item{\code{f_level}}{chr}
#'   \item{\code{clevel}}{num}
#'   \item{\code{f_clevel}}{chr}
#'   \item{\code{ph}}{num}
#'   \item{\code{f_ph}}{chr}
#'   \item{\code{turb}}{num}
#'   \item{\code{f_turb}}{chr}
#'   \item{\code{chlfluor}}{num}
#'   \item{\code{f_chlfluor}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
"apadbwq"

######
#' Example weather data for Apalachicola Bay East Bay station.
#'
#' An example weather dataset for Apalachicola Bay East Bay station.  The data are a \code{\link{smwpr}} object that have been imported into R from csv files using the \code{\link{import_local}} function.  The raw data were obtained from the CDMO data portal but can also be accessed from a zip file created for this package.  See the source below.  The help file for the \code{\link{import_local}} function describes how the data can be imported from the zip file.  Attributes of the dataset include \code{names}, \code{row.names}, \code{class}, \code{station}, \code{parameters}, \code{qaqc_cols}, \code{date_rng}, \code{timezone}, and \code{stamp_class}. 
#'  
#' @format A \code{\link{swmpr}} object and \code{\link[base]{data.frame}} with 70176 rows and 23 variables:
#' \describe{
#'   \item{\code{datetimestamp}}{POSIXct}
#'   \item{\code{atemp}}{num}
#'   \item{\code{f_atemp}}{chr}
#'   \item{\code{rh}}{num}
#'   \item{\code{f_rh}}{chr}
#'   \item{\code{bp}}{num}
#'   \item{\code{f_bp}}{chr}
#'   \item{\code{wspd}}{num}
#'   \item{\code{f_wspd}}{chr}
#'   \item{\code{maxwspd}}{num}
#'   \item{\code{f_maxwspd}}{chr}
#'   \item{\code{wdir}}{num}
#'   \item{\code{f_wdir}}{chr}
#'   \item{\code{sdwdir}}{num}
#'   \item{\code{f_sdwdir}}{chr}
#'   \item{\code{totpar}}{num}
#'   \item{\code{f_totpar}}{chr}
#'   \item{\code{totprcp}}{num}
#'   \item{\code{f_totprcp}}{chr}
#'   \item{\code{cumprcp}}{num}
#'   \item{\code{f_cumprcp}}{chr}
#'   \item{\code{totsorad}}{num}
#'   \item{\code{f_totsorad}}{chr}
#' }
#' 
#' @source \url{https://s3.amazonaws.com/swmpexdata/zip_ex.zip}
"apaebmet"