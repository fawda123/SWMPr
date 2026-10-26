# Plot swmpr using a histogram

Plot a histogram showing the distribution of a swmpr parameter

## Usage

``` r
# S3 method for class 'swmpr'
hist(x, ...)
```

## Arguments

- x:

  input swmpr object

- ...:

  other arguments passed to
  [`hist`](https://rdrr.io/r/graphics/hist.html)

## Details

The swmpr method for histograms is a convenience function for the
default histogram function. Conventional histogram methods also work
well since swmpr objects are also data frames. The input data must
contain only one parameter.

## See also

[`hist`](https://rdrr.io/r/graphics/hist.html)

## Examples

``` r
## get data
data(apadbwq)
dat <- subset(apadbwq, select = 'do_mgl')

## histogram using swmpr method
hist(dat)


## change axis labels, plot title
hist(dat, xlab = 'Dissolved oxygen', main = 'Histogram of DO')


## plot using default method
hist(dat$do_mgl)
```
