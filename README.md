# SWMPr: An R package for the National Estuarine Research Reserve System
Marcus W. Beck, beck.marcus@epa.gov  

Linux: [![Travis-CI Build Status](https://travis-ci.org/fawda123/SWMPr.png?branch=master)](https://travis-ci.org/fawda123/SWMPr)

Windows: [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/fawda123/SWMPr?branch=master)](https://ci.appveyor.com/project/fawda123/SWMPr)

#Overview 

The System Wide Monitoring Program ([SWMP](http://nerrs.noaa.gov/RCDefault.aspx?ID=18)) was implemented by the National Estuarine Research Reserve System ([NERRS](http://nerrs.noaa.gov/)) in 1995 to provide continuous monitoring data at over 300 continuous monitoring stations in 28 estuaries across the United States.  SWMPr (pronounced "swamper") is an R package for retrieving, organizing, and analyzing estuary monitoring data from SWMP. Please cite as follows:

*Beck MW. 2015. SWMPr: An R package for the National Estuarine Research Reserve System.  Version 1.9.2. https://github.com/fawda123/SWMPr*

#Installing the package

This package is currently under development and can be installed from Github:


```r
install.packages('devtools')
library(devtools)
install_github('fawda123/SWMPr')
library(SWMPr)
```

#Using the package

Please see the [sister repository](https://github.com/fawda123/swmpr_manu) for a [draft manuscript](https://github.com/fawda123/swmpr_manu/blob/master/swmpr_manu.pdf) that describes package use in detail.  A brief description of the available functions is provided below. See help documentation for more details on each function (e.g., `?all_params`).

##Retrieve

`all_params` Retrieve up to 100 records starting with the most recent at a given station, all parameters.  Wrapper to `exportAllParamsXMLNew` function on web services. 

`all_params_dtrng` Retrieve records of all parameters within a given date range for a station.  Optional argument for a single parameter.  Maximum of 1000 records. Wrapper to `exportAllParamsDateRangeXMLNew`.

`import_local` Import files from a local path.  The files must be in a specific format, specifically those returned from the CDMO using the [zip downloads](http://cdmo.baruch.sc.edu/aqs/zips.cfm) option for a reserve.

`import_remote` Import SWMP site data from a remote independent server.  These files have been downloaded from CDMO, processed using functions in this package, and uploaded to an Amazon server for quicker import into R.  

`single_param` Retrieve up to 100 records for a single parameter starting with the most recent at a given station.  Wrapper to `exportSingleParamXMLNew` function on web services. 

##Organize

`comb.swmpr` Combines swmpr objects to a common time series using setstep, such as combining the weather, nutrients, and water quality data for a single station. Only different data types can be combined.

`qaqc.swmpr` Remove QAQC columns and remove data based on QAQC flag values for a swmpr object.  Only applies if QAQC columns are present.  

`qaqcchk.swmpr` View a summary of the number of observations in a swmpr object that are assigned to different QAQC flags used by CDMO.  The output is used to inform further processing but is not used explicitly. 

`rem_reps.swmpr` Remove replicate nutrient data that occur on the same day.  The default is to average replicates.

`setstep.swmpr` Format data from a swmpr object to a continuous time series at a given timestep.  The function is used in `comb.swmpr` and can also be used with individual stations.

`subset.swmpr` Subset by dates and/or columns for a swmpr object.  This is a method passed to the generic `subset' function provided in the base package.

##Analyze

`aggreswmp.swmpr` Aggregate swmpr objects for different time periods - years, quarters, months,  weeks, days, or hours.  Aggregation function is user-supplied but defaults to mean. 

`aggremetab.swmpr` Aggregate metabolism data from a swmpr object.  This is primarly used within `plot_metab` but may be useful for simple summaries of raw daily data.

`ecometab.swmpr` Estimate ecosystem metabolism for a combined water quality and weatehr dataset using the open-water method.

`decomp.swmpr` Decompose a swmpr time series into trend, seasonal, and residual components.  This is a simple wrapper to `decompose`.  Decomposition of monthly or daily trends is possible.

`decomp_cj.swmpr` Decompose a swmpr time series into grandmean, annual, seasonal, and events components.  This is a simple wrapper to `decompTs` in the wq package.  Only monthly decomposition is possible.

`hist.swmpr` Plot a histogram for a swmpr object.

`lines.swmpr` Add lines to an existing swmpr plot.

`na.approx.swmpr` Linearly interpolate missing data (`NA` values) in a swmpr object. The maximum gap size that is interpolated is defined as a maximum number of records with missing data. 

`plot.swmpr` Plot a univariate  time series for a swmpr object.  The parameter name must be specified.

`plot_metab` Plot ecosystem metabolism estimates after running `ecometab` on a swmpr object.  

`plot_summary` Create summary plots of seasonal/annual trends and anomalies for a water quality or weather parameter.

`smoother.swmpr` Smooth swmpr objects with a moving window average.  Window size and sides can be specified, passed to `filter`.

##Miscellaneous

`calcKL` Estimate the reaeration coefficient for air-sea gas exchange.  This is only used within the `ecometab` function.

`map_reserve` Create a map of all stations in a reserve using the ggmap package.

`metab_day` Identify the metabolic day for each approximate 24 period in an hourly time series.  This is only used within the `ecometab` function.

`param_names` Returns column names as a list for the parameter type(s) (nutrients, weather, or water quality).  Includes QAQC columns with 'f_' prefix. Used internally in other functions.

`parser` Parses html returned from CDMO web services, used internally in retrieval functions.

`site_codes` Metadata for all stations, wrapper to `exportStationCodesXMLNew` function on web services.

`site_codes_ind` Metadata for all stations at a single site, wrapper  to `NERRFilterStationCodesXMLNew` function on web services.

`swmpr` Creates object of swmpr class, used internally in retrieval functions.

`time_vec` Converts time vectors to POSIX objects with correct time zone for a site/station, used internally in retrieval functions.
