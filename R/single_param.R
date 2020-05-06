#' Get CDMO records for a single parameter
#' 
#' Get stations records from the CDMO for a single parameter starting with the most current date
#' 
#' @param station_code chr string of station, 7 or 8 characters
#' @param Max numeric value for number of records to obtain from the current date
#' @param param chr string for a single parameter to return.
#' @param trace logical indicating if import progress is printed in console
#' 
#' @import XML
#' 
#' @concept retrieve
#' 
#' @export
#' 
#' @return Returns a swmpr object with one parameter.  QAQC columns are not provided.
#' 
#' @details 
#' This function retrieves data from the CDMO through the web services URL. The computer making the request must have a registered IP address.  Visit the CDMO web services page for more information: \url{http://cdmo.baruch.sc.edu/webservices.cfm}.  This function is the CDMO equivalent of \code{exportSingleParamXML}.
#' 
#' @seealso \code{\link{all_params}}, \code{\link{all_params_dtrng}}
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## single parameter for a station, most recent
#' single_param('hudscwq', 'do_mgl')
#' 
#' }
single_param <- function(station_code, param, Max = 100, trace = TRUE){
  
  ##
  # access CDMO web services
  
  # url
  serv <- "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
  
  # request data
  dat <- try({
    httr::GET(
      serv,
      query = list(
        method = 'exportSingleParamXMLNew',
        station_code = station_code,
        param = param,
        recs = 1
      )
    )
  }, silent = TRUE)
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
  # parse reply from server 
  dat <- parser(dat)
  
  # sometimes data request is good, but empty data frame returned
  if(nrow(dat) == 0)
    stop('Empty data frame, check metadata for station availability')
  
  # starting date as character
  dtrng <- dat$datetimestamp
  dtrng <- strsplit(as.character(dtrng), ' ')[[length(dtrng)]][1]
  dtrng <- c('01/01/1970', dtrng)
  
  # pass to all_params_dtrng
  out <- all_params_dtrng(station_code, dtrng, param = param, trace = trace, Max = Max)
  
  return(out)
  
  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
}