## SWMPr 2.4.2

* Added `fill` argument to `plot_summary` as option to fill missing values with long term monthly averages or linear interpolation

* Added `base_size` argument for global text size in `plot_summary`

* Fix to annual anomalies in `plot_summary` as average of monthly sums

* Enhancement to `qaqc` to work if angle brackets not included in qualifier codes

* Only required functions from data.table dependency imported

## SWMPr 2.4.1

* Fix to `map_reserve` function for updates to ggmap

* Added Pacific/Honolulu time zone and `'hee'` site code to `time_vec` function for He'eia reserve

* Added `colsleft`, `colsmid`, and `colsright` arguments to `plot_summary` for separate colors on left, mid, right portions of plot

* URL fix to `metab_day` documentation

## SWMPr 2.4.0

* Each function is now in its own file instead of grouped by categories (thanks Kim!)

* Fix to gas exchange estimate for correct units in `ecometab` function

* Fixing broken URLs in vignette and package documentation

## SWMPr 2.3.1

* Added `collMethd` argument to `import_local` function to specify diel or standard monthly sampling for nutrient samples

* Minor fix to `swmpr` function if trailing white spaces found at end of each qaqc column

* Minor fix to help files if searching by concept, e.g., `help.search('analyze', package = "SWMPr")`

## SWMPr 2.3.0

* Added informative error message if IP address not registered for using CDMO web services

* Added `plot_quants` function to evaluate trends relative to long-term averages

* All numeric columns are forced to numeric atomic vectors on input

* Documentation updated to indicate a return value in the `calckl` function

* Fixed bug with `comb` function if differ argument is incorrect

* Added option in `qaqc` to included additional columns specifying censored values (via `cens_id` function)

## SWMPr 2.2.0

* Added `plot_wind` function for wind roses from weather data

* Fixes to `decomp_cj` function, including grandmean back to output

* Added point and line options for `overplot`

* Trailing whitespace in qaqc columns are removed in `swmpr` argument

* Removed dependency to deprecated wq package

* Safety stop in `import_local` if data type not in station code

* Removed use of `setstep` function in `ecometab` that caused less precise results

* Fixed bug with sanity check of `depth_val` argument in `ecometab`

* Fixed bug with `metab_day` if original time series was not continuous

* Default S3 method added to `ecometab`

## SWMPr 2.1.5

* `parser` now uses `xmlTreeParse` making CDMO retrieval functions much faster 

## SWMPr 2.1.4

* Fixed case issue with `import_local`

## SWMPr 2.1.3

* Fixed bug with `qaqc` function that treated integer flags the same regardless of sign

## SWMPr 2.1.2

* Default method for `smoother` was added, 

* `aggremetab` can now use moving window smoothing with appropriate changes in `plot_metab`

## SWMPr 2.1.1

* Fixed bug with character input for `timestep` argument in `setstep`.  The default value for `differ` was causing an error.

## SWMPr 2.1.0

* Version update for push to CRAN, previous was 2.0.0.  See the notes below about each version change for updates since the last push.

## SWMPr 2.0.14
* Fixed issue with default method of `comb` that caused error if the input was not a list of objects to combine.

## SWMPr 2.0.13
* Default method for `comb` added so it can be used with non-swmpr objects.

## SWMPr 2.0.12
* Default method for `setstep` added so it can be used with non-swmpr objects. 

## SWMPr 2.0.11
* Default method for `decomp` added so it can be used with non-swmpr objects. 

## SWMPR 2.0.10
* Default method for `decomp_cj` added so it can be used with non-swmpr objects. 

## SWMPr 2.0.9
* `overplot` function added that allows plotting of multiple variables on the sample plot with different y-axes.

## SWMPr 2.0.8
* `qaqc` function now uses string matching to keep flags.  This is a more flexible approach to keeping desired flags, the previous version used hard coded integers for each flag.

## SWMPr 2.0.7
* Fixed bug in `plot_summary` that returned an error if a cumulative precipitation column was present.  This is no longer collected by SWMP but older datasets may have have it included.

## SWMPr 2.0.6
* Some tweaks to `import_local` to handle more flexible text input for the station code.  Function doesn't break if the file extension is included and the date is stripped from the station attribute of the imported swmpr object.

## SWMPr 2.0.5
* `import_local` function can now import files directly from a zipped folder, i.e., it doesn't have to be decompressed first. 

* SWMPr functions can be searched by concepts - retrieve, organize, analyse, e.g., `help.search('retrieve', package = 'SWMPr')`  

## SWMPr 2.0.4
* `single_param` now retreives more than 100 records by using `all_params_dtrng` internally.

## SWMPr 2.0.3
* `all_params` now retreives more than 100 records by using `all_params_dtrng` internally.

## SWMPr 2.0.2
* `all_params_dtrng` function now retrieves more than 1000 records from the CDMO.  In theory, all records for a station can be retrieved but this would take a long time. 

## SWMPr 2.0.1
* A character string input can be used to specify the value for the `timestep` argument in the `setstep` function, e.g., `"hours"`.  See the help file. 