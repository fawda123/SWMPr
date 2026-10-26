# Estimate sunrise and sunset times

Estimate sunrise and sunset times for a location and date. This is a
stripped down version of `getSunlightTimes` from the suncalc package,
which implements the SunCalc algorithm by Vladimir Agafonkin
(<https://github.com/mourner/suncalc>).

## Usage

``` r
sun_times(date, lat, long, tz = "UTC")
```

## Arguments

- date:

  Date vector, or a vector that can be converted with
  [`as.Date`](https://rdrr.io/r/base/as.Date.html)

- lat:

  numeric for latitude

- long:

  numeric for longitude (negative west of prime meridian)

- tz:

  chr string for timezone of the output, e.g., 'America/Chicago'

## Value

A `data.frame` with columns `date`, `sunrise`, and `sunset`. The latter
two are `POSIXct` in the timezone given by `tz`. Values are `NA` if the
sun does not rise or set on a date, e.g., polar day or night.

## Details

Times are estimated for a solar altitude of -0.833 degrees, which
accounts for atmospheric refraction and the radius of the solar disc.
Estimates are typically within one to two minutes of those from the NOAA
solar calculator for mid-latitude locations. Times for each date are
based on the solar transit nearest to noon UTC on that date.

This function is used within
[`metab_day`](http://fawda123.github.io/SWMPr/reference/metab_day.md) to
identify metabolic days for
[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md).

## See also

[`metab_day`](http://fawda123.github.io/SWMPr/reference/metab_day.md)

## Examples

``` r
# Apalachicola Bay NERR, Florida
sun_times(as.Date('2024-06-21'), lat = 29.67, long = -85.06, tz = 'America/Jamaica')
#>         date             sunrise              sunset
#> 1 2024-06-21 2024-06-21 05:41:52 2024-06-21 19:44:54

# a sequence of dates
dts <- seq(as.Date('2024-01-01'), as.Date('2024-01-07'), by = 'days')
sun_times(dts, lat = 29.67, long = -85.06, tz = 'America/Jamaica')
#>         date             sunrise              sunset
#> 1 2024-01-01 2024-01-01 07:36:30 2024-01-01 17:52:56
#> 2 2024-01-02 2024-01-02 07:36:44 2024-01-02 17:53:38
#> 3 2024-01-03 2024-01-03 07:36:57 2024-01-03 17:54:20
#> 4 2024-01-04 2024-01-04 07:37:08 2024-01-04 17:55:03
#> 5 2024-01-05 2024-01-05 07:37:17 2024-01-05 17:55:47
#> 6 2024-01-06 2024-01-06 07:37:25 2024-01-06 17:56:31
#> 7 2024-01-07 2024-01-07 07:37:31 2024-01-07 17:57:17
```
