# Get colors for plots

Get gradient default colors for plots

## Usage

``` r
gradcols(col_vec = NULL)
```

## Arguments

- col_vec:

  chr string of plot colors to use. Any color palette from RColorBrewer
  can be used as a named input. Palettes from grDevices must be supplied
  as the returned string of colors for each palette.

## Value

A character vector of colors in hexidecimal notation.

## Details

This is a convenience function for retrieving a color palette. Palettes
from RColorBrewer will use the maximum number of colors. The default
palette is 'Spectral'.
