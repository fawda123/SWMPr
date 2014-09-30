##SWMPr package for estuarine monitoring data

In development! 

This repository contains materials to retrieve, organize, and analyze estuarine monitoring data from the System Wide Monitoring Program (<a href="http://nerrs.noaa.gov/RCDefault.aspx?ID=18">SWMP</a>) implemented by the National Estuarine Research Reserve System (<a href="http://nerrs.noaa.gov/">NERRS</a>).  SWMP was initiated in 1995 to provide continuous monitoring data at over 300 stations in 28 estuaries across the United States.  SWMP data are maintained and stored online by the Centralized Data Management Office (CDMO). This R package provides several functions to access, prepare, and analyze estuary data from the CDMO.  More info on the CDMO web services are available <a href="http://cdmo.baruch.sc.edu/webservices.cfm">here</a>.  <b>Your computer's IP address must be registered with the CDMO website to use the data retrieval functions, see contact info in the link.</b>  All other functions can be used after obtaining data from the CDMO, as described below. 

The package has many dependencies, the most important being the SSOAP package for retrieving data from the CDMO using a SOAP client interface.  The SSOAP package is not required to use the package but is necessary for using specific functions, see below.  The SSOAP package is currently removed from CRAN but accessible at <a href="http://www.omegahat.org/SSOAP/">http://www.omegahat.org/SSOAP/</a>.  It can be installed as follows:

```{r}
install.packages("SSOAP", repos="http://www.omegahat.org/R", dependencies = T,  type =  "source")
```

##Install the Package

This package is currently under development and will be later uploaded as an official package repository following standard procedure.  This will allow use of the `install.packages` method for direct install within R.  For now, Github users can fork and pull the materials to create a local clone of the current form.  Otherwise, the `funcs.r` file contains all currently developed (and partially tested) functions.  Package dependencies can be found in `.RProfile`.  Files ending in `retrieval.r` were used to test the functions.   

##Package scope and workflow

The SWMP database provides continuous monitoring data for water quality, nutrient, and meterological parameters at numerous locations and time periods. The desired parameters, locations, and date ranges for a specific analysis are of course dependent on the question of interest.  Regardless of the needs for a particular analysis, basic steps can be used for obtaining, organizing, and processing the data.  The functions provided in this package are meant to facilitate these initial processes following a basic workflow.  The end result of this workflow is a time series with combined water quality, nutrient, and meteorological data for a single station. For simplicity, the station is considered a relevant unit of analysis because observations are spatially and temporally synoptic, although it is acknowledged that weather stations do not always spatially co-occur with water quality or nutrient stations. Moreover, all paramaters are included as output from the data organization functions to allow greater flexibility in any following analyses.  That is, the researcher may choose to analyze a single parameter or multiple parameters from the resulting data.  The following discussion describes basic steps that should be taken to retrieve and organize SWMP data for analysis.

SWMP data can be obtained directly from the CDMO through an online query or by using the retrieval functions provided in this package.  In either case, the metadata should be consulted to determine the parameters and date ranges that are available for each station.  Metadata are included as a .csv file with data requested from the CDMO and can also be obtained using the `site_codes` or `site_codes_ind` functions in the package.  Throughout, a station refers to either a nutrient, water quality, or meteorological station that occurs at a site or reserve to be combined as a single dataset that represents all data for a station.  Due to server constraints, the retrieval functions in this package return a limited number of records.  The functions are more amenable to analyses with short time periods, although savvy users could use these functions iteratively (i.e., with `for` or `while` loops) to obtain longer time series.  For larger requests, it is easier to obtain data using the online query system on the CDMO.  Date can be retrieved from the CDMO several ways.  Data from single stations can be requested from the <a href="http://cdmo.baruch.sc.edu/get/export.cfm">data export system</a>, whereas data from multiple stations can be requested from the <a href="http://cdmo.baruch.sc.edu/aqs/">advanced query system</a>.  The most general format for these  data are a .csv file for single station with rows as data records and columns indicating the time stamp, parameters, and quality assurance/quality control (QAQC) flags.  

Several functions are provided for organizing the data after they are obtained using either the retrieval functions or through a direct query to the CDMO.  Your computer's IP address must be registered with the CDMO to use the retrieval functions.  The organization functions are generally used to combine the data at a single station (weather, nutrients, water quality).  Additional methods are available for dealing with QAQC flags, missing data, and incomplete time series. 

##Functions

Three main categories of functions are available: retrieve, organize, analyze.  Other miscellaneous functions are helpers/wrappers to the main functions or those used to obtain site/station metadata.

<b>misc</b>

`swmpr` Creates object of swmpr class, used internally in retrieval functions.

`parser` Parses html returned from CDMO web services, used internally in retrieval functions.

`time_vec` Converts time vectors to POSIX objects with correct time zone for a site/station, used internally in retrieval functions.

`site_codes` Metadata for all stations, wrapper to `exportStationCodesXMLNew` function on web services.

`site_codes_ind` Metadata for all stations at a single site, wrapper  to `NERRFilterStationCodesXMLNew` function on web services.

`param_names` Vector of column names for a given parameter type (nutrients, weather, or water quality).  Includes QAQC columns with 'F_' prefix. Used internally in retrieval functions.

<b>retrieval</b>

`all_params` Retrieve up to 100 records starting with the most recent at a given station, all parameters.  Wrapper to `exportAllParamsXMLNew` function on web services. 

`all_params_dtrng` Retrieve records of all parameters within a given date range for a station.  Optional argument for a single parameter.  Maximum of 1000 records. Wrapper to `exportAllParamsDateRangeXMLNew`.

`single_param` Retrieve up to 100 records for a single parameter starting with the most recent at a given station.  Wrapper to `exportSingleParamXMLNew` function on web services. 

`import_local` Import files from a local path.  The files must be in a specific format, specifically those returned from the CDMO using the <a href="http://cdmo.baruch.sc.edu/aqs/zips.cfm">zip downloads</a> option for a reserve.  Data returned from this function are generally not in a usable format and are processed with other functions.

<b>organize</b>

`qaqc` Remove QAQC columns and remove data based on QAQC flag values.  This function is used on data returned from any of the retrieval functions.  Only applies if QAQC columns are present.  

<b>analyze</b>

Not yet available.

##Files

`funcs.r` Required functions, all categories.

`dev.r` Temporary file used for development.

`test_retrieval.r` Evaluation of retrieval functions.

`test_organize.r` Evaluation of organize funcitons.

`.Rprofile` File that is run after opening the project in R, contains all package dependencies.

`.gitignore` List of files that Git ignores, not on repository.

`SWMPr.Rproj` RStudio project file used to create the package.

##Forthcoming

Actual 'package' repository after all functions are complete.

Add parameters attribute to swmpr class

Organize functions... test/debug subset function, combine functions with common time step, CDMO and remote data

Analysis functions... aggregations on diff time periods, EDA, etc.
