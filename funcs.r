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
# 'Station_Code' is character string for station (three or more characters)
# 'tz_only' is logical that returns only the timezone
# otherwise output is POSIX vector
time_vec <- function(chr_in, Station_Code, tz_only = F){
  
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

  # timezone only if T
  if(tz_only) return(tzone)
  
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

######
# import local data from CDMO
# data are .csv files for a station, all parameters and years, from zip downloads
# 'path' is chr string of full path with .csv files
# 'Station_Code' is 7 or 8 chr string of station to load, single parameter
# 'trace' is logical indicating if progress is sent to console
import_local <- function(path, Station_Code, trace){
  
  # sanity check
  if(!nchar(Station_Code) %in% c(7, 8)) stop('Station_Code invalid.')
  
  ##
  # find station files in path
  file_nms <- dir(path)
  expr <- paste0('^', Station_Code, '.*', '\\.csv$')
  files_in <- grep(expr, file_nms, value = T)
  
  if(length(files_in) == 0) stop('File(s) not found.')
  
  # import all data files for a station
  dat <- vector('list', length(files_in))
  names(dat) <- gsub('.csv', '', files_in)
  
  if(trace) cat('Loading files...\n\n')
  
  for(file_in in files_in){
    
    if(trace) cat(file_in, '\t')
    
    # import file
    tmp <- read.csv(file.path(path, file_in), stringsAsFactors = F)
    
    # convert date time to posix
    names(tmp)[grep('datetimestamp', names(tmp), ignore.case = T)] <- 'DateTimeStamp'
    tmp$DateTimeStamp <- time_vec(tmp$DateTimeStamp, Station_Code)
    
    # append to output list
    nm <-  gsub('.csv', '', file_in)
    dat[[nm]] <- tmp
    
    }
  
  ##
  # column names for each parameter type, used to subset combined data
  
  nut_nms <- c('PO4F', 'CHLA_N', 'NO3F', 'NO2F', 'NH4F', 'NO23F', 'Ke_N',
    'UREA')
  nut_nms <- paste0(c('', 'F_'), rep(nut_nms, each = 2))
  
  wq_nms <- c('Temp', 'SpCond', 'Sal', 'DO_pct', 'DO_mgl', 'Depth', 
    'cDepth', 'Level', 'cLevel', 'pH', 'Turb', 'ChlFluor')
  wq_nms <- paste0(c('', 'F_'), rep(wq_nms, each = 2))
  
  met_nms <- c('ATemp', 'RH', 'BP', 'WSpd', 'MaxWSpd', 'Wdir', 'SDWDir',
    'TotPAR', 'TotPrcp', 'CumPrcp', 'TotSoRad')
  met_nms <- paste0(c('', 'F_'), rep(met_nms, each = 2))
    
  # names to use
  parm <- substring(Station_Code, 6)
  nms <- get(paste0(parm, '_nms'))
  
  ##
  # convert output from 'import_local' to data frame and appropriate columns
  
  if(trace) cat('\n\nCombining data...')
  
  out <- do.call('rbind', dat)
  out <- data.frame(
    DateTimeStamp = out$DateTimeStamp,
    StatParam = parm, 
    out[, names(out) %in% nms], 
    row.names = seq(1, nrow(out))
    )
  
  if(trace) cat('\n\nData imported...')
  
  # return data frame
  return(out)
    
  }

########################
# organize functions
########################

######
# combine data for a single station
# 'clean_dat' is the generic, 'clean_dat.swmpr' is the method applied to swmpr class
# standard time step

comb_dat <- function(x) UseMethod('comb_dat')
comb_dat.swmpr <- function(station, path){

#   # find date ranges for files
#   tz <- time_vec('01/01/2014 0:00', Station_Code, T)
#   time_rng <- llply(dat, .fun = function(x) range(x$DateTimeStamp))
#   time_rng <- range(unlist(time_rng))
#   time_rng <- as.POSIXct(time_rng, origin="1970-01-01 00:00", tz = tz)
#   
#   # create continuous time vector based on step
#   times <- seq(time_rng[1], time_rng[2], by = step * 60)
#   times <- data.frame(DateTimeStamp = times)
    
  }

######
# qaqc filtering for data imported from local path
# 'dat_in' is data frame returned from 'import_local'
# 'qaqc_in' is numeric vector of qaqc flags to remove from data, default is all but zero
# 'trace' is logical for progress
qaqc_local <- function(dat_in, 
  qaqc_in = c(seq(-5, -1), seq(1,5)),
  trace = T){
  
  # sanity check
  if(!class(qaqc_in) %in% c('numeric', 'NULL'))
    stop('qaqc_in argument must be numeric or NULL')
    
  ##
  #remove values flagged by QA/QC, see cdmo website for flag numbers

  if(trace) cat('Processing QAQC columns...')
  
  #names of qaqc columns
  qaqc_sel <- grep('F_', names(dat_in), value = T)

  # remove qaqc column if NULL, otherwise process qaqc
  if(is.null(qaqc_in)){ 
    
    rm_col <- c('DateTimeStamp', 'StatParam', qaqc_sel)
    qaqc <- dat_in[, !names(dat_in) %in% rm_col]

  } else {
      
    #matrix of TF values for those that don't pass qaqc
    qaqc_vec <- dat_in[, names(dat_in) %in% qaqc_sel]
    qaqc_vec <- apply(qaqc_vec, 2, 
      function(x) grepl(paste(qaqc_in, collapse = '|'), x)
      )
    #replace T values with NA
    #qaqc is corrected
    qaqc_sel <- gsub('F_', '', qaqc_sel)
    qaqc <- dat_in[, names(dat_in) %in% qaqc_sel]
    qaqc <- data.frame(sapply(
      names(qaqc),
      function(x){
        out <- qaqc[, x]
        out[qaqc_vec[, paste0('F_',x)]] <- NA
        out
        },
      USE.NAMES = T
      ), stringsAsFactors = F)
    
    }
   
  ##
  # addl misc processing
  
  #combine with DateTimeStamp and append to output list
  out <- data.frame(DateTimeStamp = dat_in[,1], qaqc)
  
  #remove duplicate time stamps (some minutely), do not use aggregate
	out<-out[!duplicated(out$DateTimeStamp),]  
    
	#convert columns to numeric, missing converted to NA
	#NA values from qaqc still included as NA
	out <- data.frame(
    DateTimeStamp = out[,1],
    apply(out[, -1], 2 , as.numeric)
    )

  if(trace) cat('\n\nQAQC processed...')
  return(out)

  }

######
# qaqc filter for data imported from CDMO server
# 'dat_in' 
# 'qaqc_in' is numeric vector of qaqc flags to remove from data, default is all but zero
# 'trace' is logical for progress
qaqc_remote <- function(dat_in, qaqc_in = c(seq(-5, -1), seq(1,5)),
  trace = T){}

########################
# evaluate functions
########################