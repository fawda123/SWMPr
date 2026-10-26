# Format a swmpr time vector

Create a continuous time vector at set time step for a swmpr object

## Usage

``` r
setstep(dat_in, ...)

# S3 method for class 'swmpr'
setstep(dat_in, timestep = 15, differ = NULL, ...)

# Default S3 method
setstep(dat_in, date_col, timestep = 15, differ = NULL, ...)
```

## Arguments

- dat_in:

  input data object

- ...:

  arguments passed to or from other methods

- timestep:

  numeric value of time step to use in minutes. Alternatively, a chr
  string indicating `'years'`, `'quarters'`, `'months'`, `'days'`, or
  `'hours'` can also be used. A character input assumes 365 days in a
  year and 31 days in a month.

- differ:

  numeric value defining buffer for merging time stamps to standardized
  time series, defaults to one half of the timestep

- date_col:

  chr string for the name of the date/time column, e.g., `"POSIXct"` or
  `"POSIXlt"` objects

## Value

Returns a data object for the specified time step

## Details

The setstep function formats a swmpr object to a continuous time series
at a given time step. This function is not necessary for most stations
but can be useful for combining data or converting an existing time
series to a set interval. The first argument of the function,
`timestep`, specifies the desired time step in minutes starting from the
nearest hour of the first observation. The second argument, `differ`,
specifies the allowable tolerance in minutes for matching existing
observations to user-defined time steps in cases where the two are
dissimilar. Values for `differ` that are greater than one half the value
of timestep are not allowed to prevent duplication of existing data.
Likewise, the default value for differ is one half the time step. Rows
that do not match any existing data within the limits of the differ
argument are not discarded. Output from the function can be used with
`subset` and to create a time series at a set interval with empty data
removed.

## See also

[`comb`](http://fawda123.github.io/SWMPr/reference/comb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## import data
data(apaebmet)
dat <- apaebmet

## convert time series to two hour invervals
## tolerance of +/- 30 minutes for matching existing data
setstep(dat, timestep = 120, differ = 30)

## convert a nutrient time series to a continuous time series
## then remove empty rows and columns
data(apacpnut)
dat_nut <- apacpnut
dat_nut <- setstep(dat_nut, timestep = 60)
subset(dat_nut, rem_rows = TRUE, rem_cols = TRUE)
} # }
```
