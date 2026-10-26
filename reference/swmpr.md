# Create a swmpr object

Wrapper for creating a swmpr object

## Usage

``` r
swmpr(stat_in, meta_in)
```

## Arguments

- stat_in:

  `data.frame` of swmp data

- meta_in:

  chr string for station code (7 or 8 characters), can be multiple
  stations if data are combined

## Value

Returns a swmpr object to be used with S3 methods

## Details

This function is a simple wrapper to
[`structure`](https://rdrr.io/r/base/structure.html) that is used
internally within other functions to create a swmpr object. The function
does not have to be used explicitly. Attributes of a swmpr object
include `names`, `row.names`, `class`, `station`, `parameters`,
`qaqc_cols`, `cens_cols`, `date_rng`, `timezone`, `stamp_class`,
`metabolism` (if present), and `metab_units` (if present).
