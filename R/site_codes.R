#' Obtain metadata for all stations
#' 
#' Obtain a \code{\link[base]{data.frame}} of metadata for all SWMP stations.
#' 
#' @export
#' 
#' @return A \code{data.frame} of SWMP metadata
#' 
#' @concept retrieve
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