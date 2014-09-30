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
# 'stat_in' is data frame of swmp data
# 'meta_in' is station code, 7 or 8 characters
swmpr <- function(stat_in, meta_in){
    
  if(!is.data.frame(stat_in)) 
    stop('stat_in must be data.frame')
  
  if(any(grepl('^f_', names(stat_in)))) qaqc_cols <- T
  else qaqc_cols <- F
  
  # create class, with three attributes (station_data, class, station_meta)
  structure(
    list(station_data = stat_in), 
    class = 'swmpr', 
    station = meta_in,
    qaqc_cols = qaqc_cols,
    date_rng = range(stat_in$datetimestamp)
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
# 'station_code' is character string for station (three or more characters)
# 'tz_only' is logical that returns only the timezone
# otherwise output is POSIX vector
time_vec <- function(chr_in, station_code, tz_only = F){
  
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
  out$params_reported <- tolower(out$params_reported)
  
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
  out$params_reported <- tolower(out$params_reported)
  
  # return output
  return(out)
  
  }

######
# returns parameter columns names for each parameter type - nut, wq, met
# names are actual parameter and corresponding qaqc column name ('f_' prefix)
# used in 'import_local', 'all_params', 'all_params_dtrng' (single parameter only)
# 'param_type' is character string specifying 'nut', 'wq', or met'
param_names <- function(param_type){
  
  # sanity check
  if(!param_type %in% c('nut', 'wq', 'met'))
    stop('parm_type must chr string of nut, wq, or met')
  
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
  out <- get(paste0(param_type, '_nms'))
  
  return(out)
  
  }

########################
# retrieval functions
########################

######
# get data for a station back to x number of records
# 'station_code' is text string of station
# 'Max' is numeric of no. of records
# output is swmpr class object
all_params <- function(station_code, Max = 100){
  
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
      station_code = station_code,
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
  
  # type of parameter requested - wq, nut, or met
  parm <- substring(station_code, 6)
  nms <- param_names(parm)
  
  # format datetimetamp if output is not empty
  if(ncol(out) != 0 & nrow(out) != 0){
    
    # format datetimestamp and sort
    out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], station_code)
    out <- out[order(out$datetimestamp), ]
    out <- data.frame(
      datetimestamp = out$datetimestamp,
      out[, tolower(names(out)) %in% nms, drop = F], 
      row.names = 1:nrow(out)
      )
    names(out) <- tolower(names(out))
    
    }

  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
  }

######
# get records from date range, max of 1000 records
# 'station_code' is text string of station
# 'dtrng' is two element character vector, each of format MM/DD/YYYY
# 'param' is character vector of a single parameter to return, see results from 'stat_codes' function for availability, default is all specific to station type
all_params_dtrng <- function(station_code, dtrng, param = NULL){
  
  library(SSOAP)
  
  # access CDMO web services
  serv <- SOAPServer(
    "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
    )
  
  # arguments to pass to function on server
  soap_args = list(
      station_code = station_code,
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
  
  # type of parameter requested - wq, nut, or met, NOT the param argument
  parm <- substring(station_code, 6)
  nms <- param_names(parm)
  
  # format datetimestamp, sort, get relevant columns as data frame
  out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], station_code)
  out <- out[order(out$datetimestamp), ]
  out <- data.frame(
    datetimestamp = out$datetimestamp,
    out[, tolower(names(out)) %in% nms, drop = F],
    row.names = 1:nrow(out)
    )
  names(out) <- tolower(names(out))
  
  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
  }

######
# get records for a single parameter, max 100 records
# 'station_code' is text string of station
# 'Max' is numeric of no. of records
# 'param' is text string of parameter to return
# does not use parser as above, slight mod 
single_param <- function(station_code, Max = 100, param){
  
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
        station_code = station_code,
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
  colnames(out) <- tolower(out[1, ])
  out <- data.frame(out[-1,], stringsAsFactors = F)
  out[, param] <- as.numeric(out[, param])
  
  # type of parameter requested - wq, nut, or met, NOT the param argument
  parm <- substring(station_code, 6)
  
  # format datetimestamp and sort
  out[, 'datetimestamp'] <- time_vec(out[, 'datetimestamp'], station_code)
  out <- out[order(out$datetimestamp), ]
  out <- data.frame(
    datetimestamp = out$datetimestamp,
    out[, !tolower(names(out)) %in% 'datetimestamp', drop = F], 
    row.names = 1:nrow(out)
    )
  names(out) <- tolower(names(out))

  # convert to swmpr class
  out <- swmpr(out, station_code)
  
  # return output
  return(out)
  
  }

######
# import local data from CDMO
# data are .csv files for a station, all parameters and years, from zip downloads
# 'path' is chr string of full path with .csv files
# 'station_code' is 7 or 8 chr string of station to load, single parameter
# 'trace' is logical indicating if progress is sent to console
import_local <- function(path, station_code, trace = T){
  
  # sanity check
  if(!nchar(station_code) %in% c(7, 8)) stop('station_code invalid.')
  
  ##
  # find station files in path
  file_nms <- dir(path)
  expr <- paste0('^', station_code, '.*', '\\.csv$')
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
    names(tmp) <- tolower(names(tmp))
    
    # convert date time to posix
    names(tmp)[grep('datetimestamp', names(tmp), ignore.case = T)] <- 'datetimestamp'
    tmp$datetimestamp <- time_vec(tmp$datetimestamp, station_code)
    
    # append to output list
    nm <-  gsub('.csv', '', file_in)
    dat[[nm]] <- tmp
    
    }
  
  ##
  # column names for each parameter type, used to subset combined data
  # kept as upper case here because improted data will match, changed to lower below

  # names to use
  parm <- substring(station_code, 6)
  nms <- param_names(parm)
  
  ##
  # convert output from 'import_local' to data frame and appropriate columns
  
  if(trace) cat('\n\nCombining data...')
  
  out <- do.call('rbind', dat)
  out <- data.frame(
    datetimestamp = out$datetimestamp,
    statparam = parm, 
    out[, names(out) %in% nms], 
    row.names = seq(1, nrow(out))
    )
  
  # names as lower case
  names(out) <- tolower(names(out))
  
  # convert to swmpr class
  out <- swmpr(out, station_code)
  
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
#   tz <- time_vec('01/01/2014 0:00', station_code, T)
#   time_rng <- llply(dat, .fun = function(x) range(x$DateTimeStamp))
#   time_rng <- range(unlist(time_rng))
#   time_rng <- as.POSIXct(time_rng, origin="1970-01-01 00:00", tz = tz)
#   
#   # create continuous time vector based on step
#   times <- seq(time_rng[1], time_rng[2], by = step * 60)
#   times <- data.frame(DateTimeStamp = times)
    
  }

######
# qaqc filtering for data obtained from retrieval functions, local and remote
# qaqc is generic, qaqc.swmpr is method
# 'dat_in' is data frame returned from any of the above retrieval functions
# 'qaqc_keep' is numeric vector of qaqc flags to keep, default 'zero'
# 'trace' is logical for progress
qaqc <- function(x) UseMethod('qaqc')
qaqc.swmpr <- function(dat_in, 
  qaqc_keep = 0,
  trace = T){
  
  ##
  # sanity checks
  if(!class(qaqc_keep) %in% c('numeric', 'NULL'))
    stop('qaqc_in argument must be numeric or NULL')
  
  ##
  # swmpr data and attributes
  dat <- dat_in$station_data
  qaqc_cols <- attr(dat_in, 'qaqc_cols')
  station <- attr(dat_in, 'station')
  
  # exit function if no qaqc columns
  if(!qaqc_cols){
    warning('No qaqc columns in input data')
    return(dat_in)
    }
  
  ##
  #remove values flagged by QA/QC, see cdmo website for flag numbers

  if(trace) cat('Processing QAQC columns...')
  
  #names of qaqc columns
  qaqc_sel <- grep('f_', names(dat), value = T)
  
  qaqc_rm <- as.numeric(seq(-5,  5))
  qaqc_rm <- qaqc_rm[!qaqc_rm %in% qaqc_keep]
  
  # keep all if qaqc_in is NULL, otherwise process qaqc
  if(is.null(qaqc_keep)){ 
    
    rm_col <- c('datetimestamp', 'statparam', qaqc_sel)
    qaqc <- dat[, !names(dat) %in% rm_col]

  } else {
      
    #matrix of TF values for those that don't pass qaqc
    qaqc_vec <- dat[, names(dat) %in% qaqc_sel]
    qaqc_vec <- apply(qaqc_vec, 2, 
      function(x) grepl(paste(qaqc_rm, collapse = '|'), x)
      )
    #replace T values with NA
    #qaqc is corrected
    qaqc_sel <- gsub('f_', '', qaqc_sel)
    qaqc <- dat[, names(dat) %in% qaqc_sel]
    qaqc <- data.frame(sapply(
      names(qaqc),
      function(x){
        out <- qaqc[, x]
        out[qaqc_vec[, paste0('f_',x)]] <- NA
        out
        },
      USE.NAMES = T
      ), stringsAsFactors = F)
    
    }
   
  ##
  # addl misc processing
  
  #combine with datetimestamp and append to output list
  out <- data.frame(datetimestamp = dat[,1], qaqc)
  
  #remove duplicate time stamps (some minutely), do not use aggregate
	out<-out[!duplicated(out$datetimestamp),]  
    
	#convert columns to numeric, missing converted to NA
	#NA values from qaqc still included as NA
	out <- data.frame(
    datetimestamp = out[,1],
    apply(out[, -1], 2 , as.numeric)
    )

  # create swmpr class
  out <- swmpr(out, station)
  
  # return output
  if(trace) cat('\n\nQAQC processed...')
  return(out)

  }

######
# subset a swmpr data object by a date range or parameter
# 'subset' is chr string of form 'YYYY-MM-DD hh:mm'
subset.swmpr <- function(dat_in, subset = NULL, select = NULL, 
  operator = NULL){
    
  ##
  # swmpr data and attributes
  dat <- dat_in$station_data
  station <- attr(dat_in, 'station')
  
  ##
  # subset
  
  # create posix object from subset input
  if(!is.null(subset)){
    
    tzone <- attr(attr(tmp, 'date_rng'), 'tzone')
    subset <- as.POSIXct(subset, format = '%Y-%m-%d %H:%M', tz = tzone)
    
    # exit function if operator not provided for one subset value
    if(length(subset) == 1){
      
      if(is.null(operator))
        stop('Binary operator must be included if only one subset value is provided')
      
      date_sel <- outer(dat$datetimestamp, subset, operator)[, 1]
     
    # for two datetime values...
    } else {
      
      date_sel <- with(dat, datetimestamp >= subset[1] & datetimestamp <= subset[2])
      
      }

    }
  
  if(is.null(select)) select <- names(dat)
  
  # subset data
  out <- subset(dat, date_sel, c('datetimestamp', select))
  out <- data.frame(out, row.names = 1:nrow(out))
  
  # create swmpr class
  out <- swmpr(out, station)
  
  # return output
  return(out)
  
  }

########################
# evaluate functions
########################