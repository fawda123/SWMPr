######
# functions for SWMPr

######
# generic parsing function for objects returned from SOAP server
# called internally from other functions
# 'soap_in'is soap object returned from server
# 'parent_in' is a text string of parent nodes to parse
parser <- function(soap_in, parent_in = 'data'){
  
  # sanity check
  if(!'SOAPHTTPReply' %in% class(soap_in))
    stop('Input must be of class SOAPHTTPReply')
  
  library(XML)
  library(plyr)
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(soap_in$content, useInternalNodes = T)

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
# data frame of station metadata
# CDMO equivalent of exportStationCodesXML
stat_codes <- function(){

  library(SSOAP)
  
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
# 'Station_Code' is text string of station 
stat_code <- function(Station_Code){
  
  # get all data from params and subset
  meta <- stat_codes()
  out <- subset(meta, station_code == Station_Code)
  
  # output
  return(out)
  
  }

######
# get data for a station back to x number of records
# 'Station_Code' is text string of station
# 'Max' is numeric of no. of records
all_params <- function(Station_Code, Max = 100){
  
  library(SSOAP)
  
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

######
# get records from date range, max of 1000 records
# 'Station_Code' is text string of station
# 'dtrng' is two element character vector, each of format MM/DD/YYYY
all_params_dtrng <- function(Station_Code, dtrng){
  
  library(SSOAP)
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # request data
  dat <- .SOAP(
    serv,
    method = 'exportAllParamsDateRangeXMLNew',
    .soapArgs = list(
      Station_Code = Station_Code,
      mindate = dtrng[1],
      maxdate = dtrng[2]
      ), 
    action = '',
    .convert = F
    )
  
  # parse reply from server 
  out <- parser(dat)
  
  # return output
  return(out)
  
  }

######
# get records from date range, max of 1000 records
# 'Station_Code' is text string of station
# 'Max' is numeric of no. of records
# 'param' is text string of parameter to return
single_param <- function(Station_Code, Max, param){
  
  library(SSOAP)
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # request data
  dat <- .SOAP(
    serv,
    method = 'exportSingleParamXML',
    .soapArgs = list(
      Station_Code = Station_Code,
      Max = Max,
      param = param
      ),
    action = '',
    .convert = F
    )
  
  # parse reply from server 
  out <- parser(dat, parent_in = 'data//r')
  
  # return output
  return(out)
  
  }