######
# functions for SWMPr

######
# generic parsing function for objects returned from SOAP server
# called internally from other functions
parser <- function(soap_in){
  
  # sanity check
  if(!'SOAPHTTPReply' %in% class(soap_in))
    stop('Input must be of class SOAPHTTPReply')
  
  library(XML)
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(soap_in$content, useInternalNodes = T)

  # get children of data parent nodes
  parents <- xpathSApply(
    raw,
    '//data'
  )
  children <- names(xmlChildren(parents[[1]]))
  
  # arrange data nodes as data.frame
  out <- adply(children, 
    1,
    .fun = function(x)
      xpathSApply(raw, paste0('//data//', x), xmlValue),
    .expand = F
    )
  out <- data.frame(t(out)[-1,])
  names(out) <- children
  
  # return output
  return(out)
  
  }


######
# data frame of station metadata
# CDMO equivalent of exportStationCodesXML
stat_codes <- function(){

  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )

  # get all station codes
  reply <- .SOAP(
    serv,
    method = 'exportStationCodesXMLNew',
    action="", 
    .convert = F
    )

  # parse reply from server
  out <- parser(reply)
  
  # return output
  return(out)
  
  }

######
# get metadata for single station
# wrapper to params
stat_code <- function(Station_Code){
  
  # get all data from params and subset
  meta <- stat_codes()
  out <- subset(meta, station_code == Station_Code)
  
  # output
  return(out)
  
  }

######
# get data for a station back to x number of records
all_params <- function(Station_Code, Max = 100){
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # get from current date
  dat <- .SOAP(
    serv,
    method = 'exportAllParamsXMLNew',
    Station_Code = Station_Code,
    Max = Max,
    action="",
    .convert = F
    )
  
  # parse reply from server 
  out <- parser(dat)
  
  # return output
  return(out)
  
  }


# # get records from date range, max of 1000 records
# tmp <- .SOAP(
#   nerrs_webserv,
#   method = 'exportAllParamsDateRangeXMLNew',
#   .soapArgs = list(
#     Station_Code = 'saphdwq',
#     mindate = '10/06/2006',
#     maxdate = '11/07/2006'
#     ), 
#   action = '',
#   .convert = F
#   )
