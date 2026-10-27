# Overview

##### *Marcus W. Beck, <mbeck@tbep.org>*

## Overview

The System Wide Monitoring Program
([SWMP](https://coast.noaa.gov/digitalcoast/data/nerr.html)) was
implemented by the National Estuarine Research Reserve System
([NERRS](https://coast.noaa.gov/nerrs/)) in 1995 to provide continuous
monitoring data at over 140 continuous monitoring stations in 28
estuaries across the United States. SWMPr (pronounced “swamper”) is an R
package for retrieving, organizing, and analyzing estuary monitoring
data from SWMP. Please cite the package as follows:

*Beck MW. 2016. SWMPr: An R package for retrieving, organizing, and
analyzing environmental data for estuaries. The R Journal. 8(1):219-232.
<https://journal.r-project.org/news/RJ-2016-1-beck/RJ-2016-1-beck.pdf>*

## Installing the package

Install the package from CRAN as follows:

``` r

install.packages('SWMPr')
library(SWMPr)
```

The development (unstable) version of this package can be installed from
Github:

``` r

install.packages('devtools')
library(devtools)
install_github('fawda123/SWMPr', ref = 'development')
library(SWMPr)
```

## Using the package

A manuscript describing full use of the package is available from the [R
Journal](https://journal.r-project.org/articles/RJ-2016-015/index.html).
All source materials for the manuscript are available
[here](https://github.com/fawda123/swmpr_manu). A brief description of
the available functions is provided below. See help documentation for
more details on each function (e.g.,
[`?all_params`](http://fawda123.github.io/SWMPr/reference/all_params.md)).
All functions are grouped into respective categories that describe their
use: retrieve, organize, and analyze. Help files for functions in each
category can be viewed as follows:

``` r

help.search('retrieve', package = 'SWMPr')
help.search('organize', package = 'SWMPr')
help.search('analyze', package = 'SWMPr')
```

#### Retrieve

|  |  |
|----|----|
| `all_params` | Retrieve any number of records starting with the most recent at a given station, all parameters. Wrapper to `exportAllParamsXMLNew` function on web services. |
| `all_params_dtrng` | Retrieve records of all parameters within a given date range for a station. Optional argument for a single parameter. Wrapper to `exportAllParamsDateRangeXMLNew`. |
| `import_local` | Import files from a local path. The files must be in a specific format, specifically those returned from the CDMO using the [zip downloads](https://cdmo.baruch.sc.edu/aqs/zips.cfm) option for a reserve. |
| `import_remote` | Import SWMP site data from a remote independent server. These files have been downloaded from CDMO, processed using functions in this package, and uploaded to an Amazon server for quicker import into R. |
| `single_param` | Retrieve any number of records for a single parameter starting with the most recent at a given station. Wrapper to `exportSingleParamXMLNew` function on web services. |
| `site_codes` | Metadata for all stations, wrapper to `exportStationCodesXMLNew` function on web services. |
| `site_codes_ind` | Metadata for all stations at a single site, wrapper to `NERRFilterStationCodesXMLNew` function on web services. |

#### Organize

|  |  |
|----|----|
| `cens_id` | Creates additional columns in a swmpr object that identify observations below, within, or above the detection limit. |
| `comb` | Combines swmpr objects to a common time series using setstep, such as combining the weather, nutrients, and water quality data for a single station. Only different data types can be combined. |
| `qaqc` | Remove QAQC columns and remove data based on QAQC flag values for a swmpr object. Only applies if QAQC columns are present. |
| `qaqcchk` | View a summary of the number of observations in a swmpr object that are assigned to different QAQC flags used by CDMO. The output is used to inform further processing but is not used explicitly. |
| `rem_reps` | Remove replicate nutrient data that occur on the same day. The default is to average replicates. |
| `setstep` | Format data from a swmpr object to a continuous time series at a given timestep. The function is used in `comb` and can also be used with individual stations. |
| `subset` | Subset by dates and/or columns for a swmpr object. This is a method passed to the generic \`subset’ function provided in the base package. |

#### Analyze

|  |  |
|----|----|
| `aggreswmp` | Aggregate swmpr objects for different time periods - years, quarters, months, weeks, days, or hours. Aggregation function is user-supplied but defaults to mean. |
| `aggremetab` | Aggregate metabolism data from a swmpr object. This is primarily used within `plot_metab` but may be useful for simple summaries of raw daily data. |
| `ecometab` | Estimate ecosystem metabolism for a combined water quality and weather dataset using the open-water method. |
| `decomp` | Decompose a swmpr time series into trend, seasonal, and residual components. This is a simple wrapper to `decompose`. Decomposition of monthly or daily trends is possible. |
| `decomp_cj` | Decompose a swmpr time series into grandmean, annual, seasonal, and events components. This is a simple wrapper to `decompTs` in the wq package. Only monthly decomposition is possible. |
| `hist` | Plot a histogram for a swmpr object. |
| `lines` | Add lines to an existing swmpr plot. |
| `map_reserve` | Create a map of all stations in a reserve using the ggmap package. |
| `na.approx` | Linearly interpolate missing data (`NA` values) in a swmpr object. The maximum gap size that is interpolated is defined as a maximum number of records with missing data. |
| `overplot` | Plot multiple SWMP time series on the same y-axis. |
| `plot` | Plot a univariate time series for a swmpr object. The parameter name must be specified. |
| `plot_metab ` | Plot ecosystem metabolism estimates after running `ecometab` on a swmpr object. |
| `plot_summary` | Create summary plots of seasonal/annual trends and anomalies for a water quality or weather parameter. |
| `plot_wind` | Create a wind rose using weather data. |
| `smoother` | Smooth swmpr objects with a moving window average. Window size and sides can be specified, passed to `filter`. |

#### Miscellaneous

|  |  |
|----|----|
| `calcKL` | Estimate the reaeration coefficient for air-sea gas exchange. This is only used within the `ecometab` function. |
| `metab_day` | Identify the metabolic day for each approximate 24 period in an hourly time series. This is only used within the `ecometab` function. |
| `param_names` | Returns column names as a list for the parameter type(s) (nutrients, weather, or water quality). Includes QAQC columns with ‘f\_’ prefix. Used internally in other functions. |
| `parser` | Parses html returned from CDMO web services, used internally in retrieval functions. |
| `swmpr` | Creates object of swmpr class, used internally in retrieval functions. |
| `time_vec` | Converts time vectors to POSIX objects with correct time zone for a site/station, used internally in retrieval functions. |
