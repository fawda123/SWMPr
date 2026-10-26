# Format SWMP datetimestamp

Format the datetimestamp column of SWMP data

## Usage

``` r
time_vec(chr_in = NULL, station_code, tz_only = FALSE)
```

## Arguments

- chr_in:

  chr string of datetimestamp vector

- station_code:

  is chr string for station (three or more characters)

- tz_only:

  logical that returns only the timezone, default `FALSE`

## Value

Returns a POSIX vector if `tz_only` is true, otherwise the timezone for
a station is returned as a chr string

## Details

This function formats the datetimestamp column of SWMP data to the
[`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) format and the
correct timezone for a station. Note that SWMP data do not include
daylight savings and the appropriate location based on GMT offsets is
used for formatting. This function is used internally within data
retrieval functions and does not need to be called explicitly.
