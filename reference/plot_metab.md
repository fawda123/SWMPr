# Plot ecosystem metabolism for a swmpr object

Plot gross production, total respiration, and net ecosystem metabolism
for a swmpr object.

## Usage

``` r
plot_metab(swmpr_in, ...)

# S3 method for class 'swmpr'
plot_metab(
  swmpr_in,
  by = "months",
  alpha = 0.05,
  width = 10,
  pretty = TRUE,
  ...
)
```

## Arguments

- swmpr_in:

  input swmpr object

- ...:

  arguments passed to or from other methods

- by:

  chr string describing aggregation period, passed to
  [`aggremetab`](http://fawda123.github.io/SWMPr/reference/aggremetab.md).
  See details for accepted values.

- alpha:

  numeric indicating alpha level for confidence intervals in aggregated
  data. Use `NULL` to remove from the plot.

- width:

  numeric indicating width of top and bottom segments on error bars

- pretty:

  logical indicating use of predefined plot aesthetics

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
which can be further modified.

## Details

A plot will only be returned if the `metabolism` attribute for the
[`swmpr`](http://fawda123.github.io/SWMPr/reference/swmpr.md) object is
not `NULL`. Daily metabolism estimates are also aggregated into weekly
averages. Accepted aggregation periods are `'years'`, `'quarters'`,
`'months'`, `'weeks'`, and `'days'` (if no aggregation is preferred).

By default, `pretty = TRUE` will return a
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
with predefined aesthetics. Setting `pretty = FALSE` will return the
plot with minimal modifications to the
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
Use the latter approach for easier customization of the plot.

## See also

[`aggremetab`](http://fawda123.github.io/SWMPr/reference/aggremetab.md),
[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## import water quality and weather data
data(apadbwq)
data(apaebmet)

## qaqc, combine
wq <- qaqc(apadbwq)
met <- qaqc(apaebmet)
dat <- comb(wq, met)

## estimate metabolism
res <- ecometab(dat)

## plot
plot_metab(res)

## change alpha, aggregation period, widths
plot_metab(res, by = 'quarters', alpha = 0.1, widths = 0)

## plot daily raw, no aesthetics
plot_metab(res, by = 'days', pretty = FALSE)

## note the difference if aggregating with a moving window average
plot_metab(res, by = 30)
} # }
```
