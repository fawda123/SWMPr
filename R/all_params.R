#' Import current station records from the CDMO
#' 
#' Import current station records from the CDMO starting with the most current date
#' 
#' @param station_code chr string of station, 7 or 8 characters
#' @param Max numeric value for number of records to obtain from the current date
#' @param param chr string for a single parameter to return, defaults to all parameters for a station type.
#' @param trace logical indicating if import progress is printed in console
#' 
#' @export
#' 
#' @import httr
#' 
#' @seealso \code{\link{all_params_dtrng}}, \code{\link{single_param}}
#' 
#' @concept retrieve
#' 
#' @return  Returns a swmpr object, all available parameters including QAQC columns
#' 
#' @details 
#' This function retrieves data from the CDMO through the web services URL.  The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}.  Function is the CDMO equivalent of \code{exportAllParamsXMLNew} but actually uses \code{\link{all_params_dtrng}}, which is a direct call to \code{exportAllParamsDateRangeXMLNew}.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## all parameters for a station, most recent
#' all_params('hudscwq')
#' 
#' }
all_params <- function(station_code, Max = 100, param = NULL, trace = TRUE){
  
  # url
  serv <- "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
  
  # get from most recent record
  dat <- try({
    httr::GET(serv, 
              query = list(
                method = 'exportAllParamsXMLNew',
                station_code = station_code, 
                recs = 1
              )
    )
  }, silent = TRUE)
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
  # parse reply from server 
  dat <- parser(dat)
  
  # starting date as character
  dtrng <- dat$datetimestamp
  dtrng <- strsplit(as.character(dtrng), ' ')[[length(dtrng)]][1]
  dtrng <- c('01/01/1970', dtrng)
  
  # pass to all_params_dtrng
  out <- all_params_dtrng(station_code, dtrng, param = param, trace = trace, Max = Max)
  
  return(out)
  
}