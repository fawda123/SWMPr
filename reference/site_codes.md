# Obtain metadata for all stations

Obtain a [`data.frame`](https://rdrr.io/r/base/data.frame.html) of
metadata for all SWMP stations.

## Usage

``` r
site_codes()
```

## Value

A `data.frame` of SWMP metadata

## Details

This function retrieves data from the CDMO web services. The computer
making the request must have a registered IP address. Visit the CDMO web
services page for more information:
<https://cdmo.baruch.sc.edu/webservices.cfm>. This is the CDMO
equivalent of `exportStationCodesXML`.

## Examples

``` r
if (FALSE) { # \dontrun{

## retrieve metadata for all sites
site_codes()

} # }
```
