# Identify metabolic days in a time series

Identify metabolic days in a time series based on sunrise and sunset
times for a location and date. The metabolic day is considered the 24
hour period between sunsets for two adjacent calendar days. Sunrise and
sunset times are estimated using
[`sun_times`](http://fawda123.github.io/SWMPr/reference/sun_times.md).

## Usage

``` r
metab_day(dat_in, ...)

# Default S3 method
metab_day(dat_in, tz, lat, long, ...)
```

## Arguments

- dat_in:

  data.frame

- ...:

  arguments passed to or from other methods

- tz:

  chr string for timezone, e.g., 'America/Chicago'

- lat:

  numeric for latitude

- long:

  numeric for longitude (negative west of prime meridian)

## Details

This function is only used within
[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md) and
should not be called explicitly.

## See also

[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md),
[`sun_times`](http://fawda123.github.io/SWMPr/reference/sun_times.md)
