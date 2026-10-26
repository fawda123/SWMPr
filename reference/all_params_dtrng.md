# Get CDMO records within a date range

Get station records from the CDMO within a date range

## Usage

``` r
all_params_dtrng(station_code, dtrng, param = NULL, trace = TRUE, Max = NULL)
```

## Arguments

- station_code:

  chr string of station, 7 or 8 characters

- dtrng:

  two element chr string, each of format MM/DD/YYYY

- param:

  chr string for a single parameter to return, defaults to all
  parameters for a station type.

- trace:

  logical indicating if import progress is printed in console

- Max:

  numeric indicating maximum number of records to return

## Value

Returns a swmpr object, all parameters for a station type (nutrients,
water quality, or meteorological) or a single parameter if specified.
QAQC columns are not provided for single parameters.

## Details

This function retrieves data from the CDMO through the web services URL.
The computer making the request must have a registered IP address. Visit
the CDMO web services page for more information:
<https://cdmo.baruch.sc.edu/webservices.cfm>. This function is the CDMO
equivalent of `exportAllParamsDateRangeXMLNew`. Download time may be
excessive for large requests.

## See also

[`all_params`](http://fawda123.github.io/SWMPr/reference/all_params.md),
[`single_param`](http://fawda123.github.io/SWMPr/reference/single_param.md)

## Examples

``` r

if (FALSE) { # \dontrun{

## get all parameters within a date range
all_params_dtrng('apaebwq', c('01/01/2013', '02/01/2013'))

## get single parameter within a date range
all_params_dtrng('apaebwq', c('01/01/2013', '02/01/2013'), 
   param = 'do_mgl')

} # }
```
