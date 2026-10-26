# Aggregate metabolism data

Aggregate a metabolism attribute from swmpr data by a specified time
period and method

## Usage

``` r
aggremetab(swmpr_in, ...)

# S3 method for class 'swmpr'
aggremetab(swmpr_in, by = "weeks", na.action = na.pass, alpha = 0.05, ...)
```

## Arguments

- swmpr_in:

  input swmpr object

- ...:

  additional arguments passed to other methods

- by:

  chr string or numeric value specifying aggregation period. If chr
  string, must be `'years'`, `'quarters'`, `'months'`, `'weeks'`,
  `'days'`, or `'hours'`. A numeric value indicates the number of days
  for a moving window average. Additional arguments passed to
  [`smoother`](http://fawda123.github.io/SWMPr/reference/smoother.md)
  can be used if `by` is numeric.

- na.action:

  function for treating missing data, default `na.pass`

- alpha:

  numeric indicating alpha level of confidence interval for aggregated
  data

## Value

Returns an aggregated metabolism
[`data.frame`](https://rdrr.io/r/base/data.frame.html) if the
`metabolism` attribute of the swmpr object is not `NULL`. Upper and
lower confidence limits are also provided if the aggregation period was
specified as a character string.

## Details

The function summarizes metabolism data by averaging across set periods
of observation. Confidence intervals are also returned based on the
specified alpha level. It is used within
[`plot_metab`](http://fawda123.github.io/SWMPr/reference/plot_metab.md)
function to view summarized metabolism results. Data can be aggregated
by `'years'`, `'quarters'`, `'months'`, or `'weeks'` for the supplied
function, which defaults to the
[`mean`](https://rdrr.io/r/base/mean.html). The method of treating NA
values for the user-supplied function should be noted since this may
greatly affect the quantity of data that are returned.

## See also

[`aggregate`](https://rdrr.io/r/stats/aggregate.html),
[`aggreswmp`](http://fawda123.github.io/SWMPr/reference/aggreswmp.md),
[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md),
[`plot_metab`](http://fawda123.github.io/SWMPr/reference/plot_metab.md)

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

## change aggregation period and alpha
aggremetab(res, by = 'months', alpha = 0.1)

## use a moving window average of 30 days
aggremetab(res, by = 30)

## use a left-centered window instead
aggremetab(res, by = 30, sides = 1)
} # }
```
