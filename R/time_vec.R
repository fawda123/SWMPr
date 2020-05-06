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
    gmt_off = c(-4,-5,-6,-8,-9),
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