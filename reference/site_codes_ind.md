# Obtain metadata for a single reserve

Get metadata for all the stations at a single SWMP reserve

## Usage

``` r
site_codes_ind(nerr_site_id)
```

## Arguments

- nerr_site_id:

  chr string of site, three letters

## Value

An abbreviated `data.frame` of the SWMP metadata for the requested site

## Details

This function retrieves data from the CDMO web services. The computer
making the request must have a registered IP address. Visit the CDMO web
services page for more information:
<https://cdmo.baruch.sc.edu/webservices.cfm>. This function is the CDMO
equivalent of `NERRFilterStationCodesXMLNew`.

## Examples

``` r
if (FALSE) { # \dontrun{

## retrieve metadata for all stations at a site
site_codes_ind('apa')

} # }
```
