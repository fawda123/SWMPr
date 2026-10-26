# Import current station records from the CDMO

Import current station records from the CDMO starting with the most
current date

## Usage

``` r
all_params(station_code, Max = 100, param = NULL, trace = TRUE)
```

## Arguments

- station_code:

  chr string of station, 7 or 8 characters

- Max:

  numeric value for number of records to obtain from the current date

- param:

  chr string for a single parameter to return, defaults to all
  parameters for a station type.

- trace:

  logical indicating if import progress is printed in console

## Value

Returns a swmpr object, all available parameters including QAQC columns

## Details

This function retrieves data from the CDMO through the web services URL.
The computer making the request must have a registered IP address. Visit
the CDMO web services page for more information:
<https://cdmo.baruch.sc.edu/webservices.cfm>. Function is the CDMO
equivalent of `exportAllParamsXMLNew` but actually uses
[`all_params_dtrng`](http://fawda123.github.io/SWMPr/reference/all_params_dtrng.md),
which is a direct call to `exportAllParamsDateRangeXMLNew`.

## See also

[`all_params_dtrng`](http://fawda123.github.io/SWMPr/reference/all_params_dtrng.md),
[`single_param`](http://fawda123.github.io/SWMPr/reference/single_param.md)

## Examples

``` r

if (FALSE) { # \dontrun{

## all parameters for a station, most recent
all_params('hudscwq')

} # }
```
