#######################
# functions for SWMPr #
# Sep 2014            #
#######################

########################
# misc functions
########################

######
# function for creating swmpr class
# used to create output in data retrieval functions - 'all_params', 'all_params_dtrng', 'single_param'
# generic methods defined for swmpr below
swmpr <- function(stat_in, meta_in){
    
  if(!is.data.frame(stat_in)) 
    stop('stat_in must be data.frame')

  # get meta data for the station from 'site_codes'
  meta <- site_codes()
  meta <- meta[meta$station_code %in% meta_in, ]
  meta <- data.frame(meta, row.names = 1:nrow(meta))
  
  # create class, with three attributes (station_data, class, station_meta)
  structure(
    list(station_data = stat_in), 
    class = 'swmpr', 
    station_meta = meta
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
site_codes <- function(){

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
# get metadata for single site
# 'nerr_site_id' is text string of site, three letters
site_codes_ind <- function(nerr_site_id){
  
  library(SSOAP)
  
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
      .convert = F
      )

  # parse reply from server
  out <- parser(reply)
  
  # return output
  return(out)
  
  }

########################
# retrieval functions
########################

######
# get data for a station back to x number of records
# 'Station_Code' is text string of station
# 'Max' is numeric of no. of records
# output is swmpr class object
all_params <- function(Station_Code, Max = 100){
  
  library(SSOAP)
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # get from current date
  dat <- try({
    .SOAP(
      serv,
      method = 'exportAllParamsXMLNew',
      Station_Code = Station_Code,
      tbl = Max,
      action="",
      .convert = F
      )
    }, silent = T)
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
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
  out <- swmpr(out, meta_in = Station_Code)
  
  # return output
  return(out)
  
  }

######
# get records from date range, max of 1000 records
# 'Station_Code' is text string of station
# 'dtrng' is two element character vector, each of format MM/DD/YYYY
# 'param' is character vector of a single parameter to return, see results from 'stat_codes' function for availability, default is all specific to station type
all_params_dtrng <- function(Station_Code, dtrng, param = NULL){
  
  library(SSOAP)
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # arguments to pass to function on server
  soap_args = list(
      station_code = Station_Code,
      mindate = dtrng[1],
      maxdate = dtrng[2]
      )
  
  # add a parameter argument if provided
  if(!is.null(param)) soap_args$fieldlist <- param
  
  # request data
  dat <- try({
    .SOAP(
      serv,
      method = 'exportAllParamsDateRangeXMLNew',
      .soapArgs = soap_args, 
      action = '',
      .convert = F
      )}, silent = T)
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
  # parse reply from server 
  out <- parser(dat)
  
  # sometimes data request is good, but empty data frame returned
  if(nrow(out) == 0)
    stop('Empty data frame, check metadata for station availability')
  
  # format datetimestamp and sort
  out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], Station_Code)
  out <- out[order(out$datetimestamp), ]
  out <- data.frame(out, row.names = 1:nrow(out))
  
  # assign to swmpr class
  out <- swmpr(out, meta_in = Station_Code)
  
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
  
  # sanity check
  if(Max > 100) warning('Maximum of 100 records')
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # request data
  dat <- try({
    .SOAP(
      serv,
      method = 'exportSingleParamXML',
      .soapArgs = list(
        station_code = Station_Code,
        recs = Max,
        param = param
        ),
      action = '',
      .convert = F
      )}, silent = T)
    
  
  # stop if retrieval error
  if('try-error' %in% class(dat))
    stop('Error retrieving data, check metadata for station availability.')
  
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
  
  # assign to swmpr class
  out <- swmpr(out, meta_in = Station_Code)  

  # return output
  return(out)
  
  }

########################
# organize functions
########################

######
# clean the data, generic method for swmpr class
# 'clean_dat' is the generic, 'clean_dat.swmpr' is the method applied to swmpr class
# deal with qaqc flags
# fill missing data - approx
# standard time step

clean_dat <- function(x) UseMethod('clean_dat') 
clean_dat.swmpr <- function(x) 'test'

######
# combine data from multiple stations, generic method for SWMPr class
comb_dat <- function(x) UseMethod('comb_dat')
comb_dat.swmpr <- function(x) 'test'

########################
# evaluate functions
########################