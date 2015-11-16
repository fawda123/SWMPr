#### SWMPr 2.1.5

* `parser` now uses `xmlTreeParse` making CDMO retrieval functions much faster 

#### SWMPr 2.1.4

* Fixed case issue with `import_local`

#### SWMPr 2.1.3

* Fixed bug with `qaqc` function that treated integer flags the same regardless of sign

#### SWMPr 2.1.2

* Default method for `smoother` was added, 

* `aggremetab` can now use moving window smoothing with appropriate changes in `plot_metab`

#### SWMPr 2.1.1

* Fixed bug with character input for `timestep` argument in `setstep`.  The default value for `differ` was causing an error.

#### SWMPr 2.1.0

* Version update for push to CRAN, previous was 2.0.0.  See the notes below about each version change for updates since the last push.

#### SWMPr 2.0.14
* Fixed issue with default method of `comb` that caused error if the input was not a list of objects to combine.

#### SWMPr 2.0.13
* Default method for `comb` added so it can be used with non-swmpr objects.

#### SWMPr 2.0.12
* Default method for `setstep` added so it can be used with non-swmpr objects. 

#### SWMPr 2.0.11
* Default method for `decomp` added so it can be used with non-swmpr objects. 

#### SWMPR 2.0.10
* Default method for `decomp_cj` added so it can be used with non-swmpr objects. 

#### SWMPr 2.0.9
* `overplot` function added that allows plotting of multiple variables on the sample plot with different y-axes.

#### SWMPr 2.0.8
* `qaqc` function now uses string matching to keep flags.  This is a more flexible approach to keeping desired flags, the previous version used hard coded integers for each flag.

#### SWMPr 2.0.7
* Fixed bug in `plot_summary` that returned an error if a cumulative precipitation column was present.  This is no longer collected by SWMP but older datasets may have have it included.

#### SWMPr 2.0.6
* Some tweaks to `import_local` to handle more flexible text input for the station code.  Function doesn't break if the file extension is included and the date is stripped from the station attribute of the imported swmpr object.

#### SWMPr 2.0.5
* `import_local` function can now import files directly from a zipped folder, i.e., it doesn't have to be decompressed first. 

* SWMPr functions can be searched by concepts - retrieve, organize, analyse, e.g., `help.search('retrieve', package = 'SWMPr')`  

#### SWMPr 2.0.4
* `single_param` now retreives more than 100 records by using `all_params_dtrng` internally.

#### SWMPr 2.0.3
* `all_params` now retreives more than 100 records by using `all_params_dtrng` internally.

#### SWMPr 2.0.2
* `all_params_dtrng` function now retrieves more than 1000 records from the CDMO.  In theory, all records for a station can be retrieved but this would take a long time. 

#### SWMPr 2.0.1
* A character string input can be used to specify the value for the `timestep` argument in the `setstep` function, e.g., `"hours"`.  See the help file. 