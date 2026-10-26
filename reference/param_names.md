# Get parameters of a given type

Get parameter column names for each parameter type

## Usage

``` r
param_names(param_type = c("nut", "wq", "met"))
```

## Arguments

- param_type:

  chr string specifying `'nut'`, `'wq'`, or `'met'`. Input can be one to
  three types.

## Value

Returns a named list of parameters for the `param_type`. The parameter
names are lower-case strings of SWMP parameters and corresponding qaqc
names (`'f_'` prefix)

## Details

This function is used internally within several functions to return a
list of the expected parameters for a given parameter type: nutrients,
water quality, or meteorological. It does not need to be called
explicitly.
