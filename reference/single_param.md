# Get CDMO records for a single parameter

Get stations records from the CDMO for a single parameter starting with
the most current date

## Usage

``` r
single_param(station_code, param, Max = 100, trace = TRUE)
```

## Arguments

- station_code:

  chr string of station, 7 or 8 characters

- param:

  chr string for a single parameter to return.

- Max:

  numeric value for number of records to obtain from the current date

- trace:

  logical indicating if import progress is printed in console

## Value

Returns a swmpr object with one parameter. QAQC columns are not
provided.

## Details

This function retrieves data from the CDMO through the web services URL.
The computer making the request must have a registered IP address. Visit
the CDMO web services page for more information:
<https://cdmo.baruch.sc.edu/webservices.cfm>. This function is the CDMO
equivalent of `exportSingleParamXML`.

## See also

[`all_params`](http://fawda123.github.io/SWMPr/reference/all_params.md),
[`all_params_dtrng`](http://fawda123.github.io/SWMPr/reference/all_params_dtrng.md)

## Examples

``` r

if (FALSE) { # \dontrun{

## single parameter for a station, most recent
single_param('hudscwq', 'do_mgl')

} # }
```
