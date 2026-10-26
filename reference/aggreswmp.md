# Aggregate swmpr data

Aggregate swmpr data by specified time period and method

## Usage

``` r
aggreswmp(swmpr_in, ...)

# S3 method for class 'swmpr'
aggreswmp(
  swmpr_in,
  by,
  FUN = function(x) mean(x, na.rm = TRUE),
  params = NULL,
  aggs_out = FALSE,
  plot = FALSE,
  na.action = na.pass,
  ...
)
```

## Arguments

- swmpr_in:

  input swmpr object

- ...:

  additional arguments passed to other methods

- by:

  chr string of time period for aggregation one of `'years'`,
  `'quarters'`, `'months'`, `'weeks'`, `'days'`, or `'hours'`

- FUN:

  aggregation function, default `mean` with `na.rm = TRUE`

- params:

  names of parameters to aggregate, default all

- aggs_out:

  logical indicating if
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) is returned of
  raw data with datetimestamp formatted as aggregation period, default
  `FALSE`

- plot:

  logical to return a plot of the summarized data, default `FALSE`

- na.action:

  function for treating missing data, default `na.pass`. See the
  documentation for
  [`aggregate`](https://rdrr.io/r/stats/aggregate.html) for options.

## Value

Returns an aggregated swmpr object. QAQC columns are removed if included
with input object. If `aggs_out = TRUE`, the original `swmpr` object is
returned with the `datetimestamp` column formatted for the first day of
the aggregation period from `by`. A
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
of boxplot summaries is returned if `plot = TRUE`.

## Details

The function aggregates parameter data for a swmpr object by set periods
of observation and a user-supplied function. It is most useful for
aggregating noisy data to evaluate trends on longer time scales, or to
simply reduce the size of a dataset. Data can be aggregated by
`'years'`, `'quarters'`, `'months'`, `'weeks'`, `'days'`, or `'hours'`
for the supplied function, which defaults to the
[`mean`](https://rdrr.io/r/base/mean.html). A swmpr object is returned
for the aggregated data, although the datetimestamp vector will be
converted to a date object if the aggregation period is a day or longer.
Days are assigned to the date vector if the aggregation period is a week
or longer based on the round method for
[`IDate`](https://rdrr.io/pkg/data.table/man/IDateTime.html) objects.
This approach was used to facilitate plotting using predefined methods
for Date and POSIX objects.

The method of treating NA values for the user-supplied function should
be noted since this may greatly affect the quantity of data that are
returned (see the examples). Finally, the default argument for
`na.action` is set to `na.pass` for swmpr objects to preserve the time
series of the input data.

## See also

[`aggregate`](https://rdrr.io/r/stats/aggregate.html)

## Examples

``` r
if (FALSE) { # \dontrun{
## get data, prep
data(apacpwq)
dat <- apacpwq
swmpr_in <- subset(qaqc(dat), rem_cols = TRUE)

## get mean DO by quarters
aggreswmp(swmpr_in, 'quarters', params = c('do_mgl'))

## get a plot instead
aggreswmp(swmpr_in, 'quarters', params = c('do_mgl'), plot = T)

## plots with other variables
p <- aggreswmp(swmpr_in, 'months', params = c('do_mgl', 'temp', 'sal'), plot = T)
p
library(ggplot2)
p + geom_boxplot(aes(fill = var)) + theme(legend.position = 'none')

## get variance of DO by years, remove NA when calculating variance
## omit NA data in output
fun_in <- function(x)  var(x, na.rm = TRUE)
aggreswmp(swmpr_in, FUN = fun_in, 'years') 
} # }
```
