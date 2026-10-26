# Combine swmpr data

Combine swmpr data types for a station by common time series

## Usage

``` r
comb(...)

# S3 method for class 'swmpr'
comb(..., timestep = 15, differ = NULL, method = "union")

# Default S3 method
comb(..., date_col, timestep = 15, differ = NULL, method = "union")
```

## Arguments

- ...:

  input time series data objects, from one to many

- timestep:

  numeric value of time step to use in minutes, passed to `setstep`

- differ:

  numeric value defining buffer for merging time stamps to standardized
  time series, passed to `setstep`

- method:

  chr string indicating method of combining data. Use `'union'` for all
  dates as continuous time series or `'intersect'` for only areas of
  overlap. If input is a `swmpr` object, a `'station'` name can be used
  to combine by the date range of a given station, assuming there is
  overlap with the second station. A numeric value can be supplied for
  the default method that specifies which data object to use for the
  date range based on order of execution in the function call.

- date_col:

  chr string indicating name of the date column

## Value

Returns a combined swmpr object

## Details

The `comb` function is used to combine multiple swmpr objects into a
single object with a continuous time series at a given step. The
`timestep` function is used internally such that `timestep` and `differ`
are accepted arguments for `comb`.

The function requires one or more swmpr objects as input as separate,
undefined arguments. The remaining arguments must be called explicitly
since an arbitrary number of objects can be used as input. In general,
the function combines data by creating a master time series that is used
to iteratively merge all swmpr objects. The time series for merging
depends on the value passed to the `method` argument. Passing `'union'`
to `method` will create a time series that is continuous starting from
the earliest date and the latest date for all input objects. Passing
`'intersect'` to `method` will create a time series that is continuous
from the set of dates that are shared between all input objects.
Finally, a seven or eight character station name passed to `method` will
merge all input objects based on a continuous time series for the given
station. The specified station must be present in the input data.
Currently, combining data types from different stations is not possible,
excluding weather data which are typically at a single, dedicated
station.

## See also

[`setstep`](http://fawda123.github.io/SWMPr/reference/setstep.md)

## Examples

``` r

## get wq and met data as separate objects for the same station
swmp1 <- apacpnut
swmp2 <- apaebmet

## combine nuts and wq data by union, set timestep to 120 minutes
if (FALSE) { # \dontrun{
comb(swmp1, swmp2, timestep = 120, method = 'union')
} # }
```
