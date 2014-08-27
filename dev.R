# working with SOAP

library(SSOAP)

# does not work
nerrs_webserv <- processWSDL(
  "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl",
  useInternalNodes = T)

nerrs_interface <- genSOAPClientInterface(
  def = nerrs_webserv, 
  name = nerrs_webserv@name,
  addSoapHeader = T
  )

# does not work
library(RCurl)
library(XML)
wsdl <- getURL("http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl", 
               ssl.verifypeer = T)
doc  <- xmlInternalTreeParse(wsdl)
 
def <- processWSDL(doc)
ff  <- genSOAPClientInterface(def = def)

# or do this
nerrs_webserv <- SOAPServer(
  "http://cdmo.baruch.sc.edu/webservices2/requests.cfc?wsdl"
  )

# functions that work

# get all station codes
stat_codes <- .SOAP(
  nerrs_webserv,
  method = 'exportStationCodesXMLNew',
  action="", 
  .convert = F
  )

# get codes for a single station
get_site <- function(ID) .SOAP(
  nerrs_webserv,
  method = 'NERRFilterStationCodesXMLNew',
  ID = ID,
  action="",
  .convert = F
  )

# get from current date
tmp <- .SOAP(
  nerrs_webserv,
  method = 'exportAllParamsXMLNew',
  Station_Code = 'saphdnut',
  Max = 2,
  action="",
  .convert = F
  )

# get records from date range, max of 1000 records
tmp <- .SOAP(
  nerrs_webserv,
  method = 'exportAllParamsDateRangeXMLNew',
  .soapArgs = list(
    Station_Code = 'saphdwq',
    mindate = '10/06/2006',
    maxdate = '11/07/2006'
    ), 
  action = '',
  .convert = F
  )