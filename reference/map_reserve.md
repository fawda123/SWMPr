# Map a reserve

Create a map of all the stations in a reserve

## Usage

``` r
map_reserve(nerr_site_id, text_sz = 6, text_col = "black", f = 0.2)
```

## Arguments

- nerr_site_id:

  chr string of the reserve to map, first three characters used by NERRS
  or vector of stations to map using the first five characters

- text_sz:

  numeric value for text size of station names, passed to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)

- text_col:

  chr string for text color of station names, passed to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)

- f:

  numeric passed to
  [`make_bbox`](https://rdrr.io/pkg/ggmap/man/make_bbox.html) as
  expansion around the bounding box for stations at a NERR site,
  increase to expand map zoom chr

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
for plotting.

## Details

This function is a simple wrapper to functions in the ggmap package
which returns a map of all of the stations at a NERRS reserve. The `f`
argument may have to be chosen through trial and error depending on the
spatial extent of the reserve. A local data file included with the
package is used to get the latitude and longitude values of each
station. The files includes only active stations as of January 2015.

## See also

[`get_map`](https://rdrr.io/pkg/ggmap/man/get_map.html),
[`ggmap`](https://rdrr.io/pkg/ggmap/man/ggmap.html),
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
if (FALSE) { # \dontrun{
map_reserve('jac')
} # }
```
