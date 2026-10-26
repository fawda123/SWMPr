# Locations of NERRS sites

Location of NERRS sites in decimal degrees and time offset from
Greenwich mean time. Only active sites are included. Sites are
identified by five letters indexing the reserve and site names. The
dataset is used to plot locations with the
[`map_reserve`](http://fawda123.github.io/SWMPr/reference/map_reserve.md)
function and to identify metabolic days with the
[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md)
function. Created from sampling_locations.csv provided by CDMO.

## Usage

``` r
stat_locs
```

## Format

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) object with 161
rows and 4 variables:

- `station_code`:

  chr

- `latitude`:

  numeric

- `longitude`:

  numeric

- `gmt_off`:

  int

## See also

[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md),
[`map_reserve`](http://fawda123.github.io/SWMPr/reference/map_reserve.md)
