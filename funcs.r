######
# functions for SWMPr

######
# function for creating SWMPr object class
# used  for output in data retrieval functions
# input to other methods
SWMPr <- function(site_in, meta_in){
    
  if(!is.data.frame(site_in)) 
    stop('site_in must be data frame')
  
  structure(
    site_in, 
    class = 'SWMPr', 
    stat_meta = stat_code(meta_in)
    )
  
  } 

######
# generic parsing function for objects returned from SOAP server
# called internally from other functions
# 'soap_in' is soap object returned from server
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
# convert datetimestamp string to POSIXct, using correct tz (no daylight saving)
# uses hardcoded timezone data for each reserve
# 'chr_in' is datetimestamp vector
# 'Station_Code' is character string for station
# output is POSIX vector
time_vec <- function(chr_in, Station_Code){
  
  # lookup table for time zones based on gmt offset - no DST!
  gmt_tab <- data.frame(
    gmt_off=c(-4,-5,-6,-8,-9),
    tz = c('America/Virgin', 'America/Jamaica', 'America/Regina',
      'Pacific/Pitcairn', 'Pacific/Gambier'),
    stringsAsFactors = F
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
  gmt_offset <- gmt_offsets[which(sites %in% substr(Station_Code, 1, 3))]
  tzone <- gmt_tab[gmt_tab$gmt_off %in% gmt_offset, 'tz']

  # format datetimestamp
  out <- as.POSIXct(chr_in, tz = tzone, format = '%m/%d/%Y %H:%M')
  
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
# output is SWMPr class object
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
  
  # format datetimetamp if output is not empty
  if(ncol(out) != 0 & nrow(out) != 0){
    
    # format datetimestamp and sort
    out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], Station_Code)
    out <- out[order(out$datetimestamp), ]
    out <- data.frame(out, row.names = 1:nrow(out))
    
    }

  # assign to swmpr class
  out <- SWMPr(out, meta_in = Station_Code)
  
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
  
  # format datetimestamp and sort
  out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], Station_Code)
  out <- out[order(out$datetimestamp), ]
  out <- data.frame(out, row.names = 1:nrow(out))
  
  # return output
  return(out)
  
  }

######
# get records for a single parameter, max 100 records
# 'Station_Code' is text string of station
# 'Max' is numeric of no. of records
# 'param' is text string of parameter to return
# does not use parser as above, slight mod 
single_param <- function(Station_Code, Max = 100, param){
  
  library(SSOAP)
  library(XML)
  library(plyr)
  
  # stop if requested parameter not available for station
  params <- stat_code(Station_Code)$params_reported
  if(params != "" & !grepl(param, params))
    stop(paste0('Requested parameter must be in ', params))
  
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
  
  # convert to XMLDocumentContent for parsing
  raw <- htmlTreeParse(dat$content, useInternalNodes = T)

  # get node attributes for c after parsing
  attrs <- xpathSApply(
    raw,
    '//data//r//c',
    fun = xmlAttrs
  )
  
  # arrange as data frame
  out <- matrix(attrs, ncol = 2, byrow = T)
  colnames(out) <- out[1, ]
  out <- data.frame(out[-1,], stringsAsFactors = F)
  out[, param] <- as.numeric(out[, param])
  
  # format datetimestamp and sort
  out[, 'DateTimeStamp'] <- time_vec(out[, 'DateTimeStamp'], Station_Code)
  out <- out[order(out$DateTimeStamp), ]
  out <- data.frame(out, row.names = 1:nrow(out))
  
  # return output
  return(out)
  
  }

######
# clean the data 
# deal with qaqc flags
# fill missing data - approx
# standard time step
clean_dat <- function(SWMPr){}

######
# combine data from multiple stations
comb_dat <- function(SWMPr){}
