##SWMPr package for estuarine monitoring data

In development! 

This repository contains materials to access, prepare, and analyze estuarine monitoring data from the System Wide Monitoring Program (<a href="http://nerrs.noaa.gov/RCDefault.aspx?ID=18">SWMP</a>) implemented by the National Estuarine Research Reserve System (<a href="http://nerrs.noaa.gov/">NERRS</a>).  SwMP was initiated in 1995 to provide continuous monitoring data at over 300 stations in 28 estuaries across the United States.  SWMP data are maintained and stored online by the Centralized Data Management Office (CDMO). This R package provides several functions to access, prepare, and analyze estuary data from the CDMO.  More info on the CDMO web services are available <a href="http://cdmo.baruch.sc.edu/webservices.cfm">here</a>.  <b>Your computer's IP address must be registered with the CDMO website to use these functions, see contact info in the link.</b> 

The package has many dependencies, the most important being the SSOAP package for retrieving data from the CDMO.  The SSOAP package is currently removed from CRAN but accessible at <a href="http://www.omegahat.org/R">http://www.omegahat.org/R</a>.

##Install the Package

Fork and pull... will add more here.

##Functions

Three main categories of functions are available: retrieval, preparation, analysis.  Other miscellaneous functions are helpers/wrappers to the main functions or those used to obtain site/station metadata.

<b>misc</b>

`swmpr` Creates object of swmpr class, used internally in retrieval functions.

`parser` Parses html returned from CDMO web services, used internally in retrieval functions.

`time_vec` Converts time vectors to POSIX objects with correct time zone for a site/station, used internally in retrieval functions.

`site_codes` Metadata for all stations, wrapper to `exportStationCodesXMLNew` function on web services.

`site_codes_ind` Metadata for all stations at a single site, wrapper  to `NERRFilterStationCodesXMLNew` function on web services.

<b>retrieval</b>

`all_params` Retrieve up to 100 records starting with the most recent at a given station, all parameters.  Wrapper to `exportAllParamsXMLNew` function on web services. 

`all_params_dtrng` Retrieve records of all parameters within a given date range for a station.  Optional argument for a single parameter.  Maximum of 1000 records. Wrapper to `exportAllParamsDateRangeXMLNew`.

`single_param` Retrieve up to 100 records for a single parameter starting with the most recent at a given station.  Wrapper to `exportSingleParamXMLNew` function on web services. 

<b>preparation</b>

`clean_dat.swmpr`

`comb_dat.swmpr`

<b>analysis</b>

##Files

<code>funcs.r</code> Required functions, all categories.

<code>dev.r</code> Temporary file used for development.

<code>test_retrieval.r</code> Evaluation of retrieval functions.

##To do

create prep functions

##Random

Some stations do not have metadata for available parameters...
