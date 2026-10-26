# QAQC filtering for SWMP data

QAQC filtering for SWMP data obtained from retrieval functions, local
and remote

## Usage

``` r
qaqc(swmpr_in, ...)

# S3 method for class 'swmpr'
qaqc(swmpr_in, qaqc_keep = "0", trace = FALSE, ...)
```

## Arguments

- swmpr_in:

  input swmpr object

- ...:

  arguments passed to or from other methods

- qaqc_keep:

  character string of qaqc flags to keep, default `'0'`, any number of
  flag codes can be supplied including three character error codes (see
  examples)

- trace:

  logical for progress output on console, default `FALSE`

## Value

Returns a swmpr object with `NA` values for records that did not match
`qaqc_keep`. QAQC columns are also removed.

## Details

The qaqc function is a simple screen to retain values from the data with
specified QAQC flags, described online:
<https://cdmo.baruch.sc.edu/data/qaqc.cfm>. Each parameter in the swmpr
data typically has a corresponding QAQC column of the same name with the
added prefix 'f\_'. Values in the QAQC column specify a flag from -5 to
5. Generally, only data with the '0' QAQC flag should be used, which is
the default option for the function. Data that do not satisfy QAQC
criteria are converted to `NA` values. Additionally, simple filters are
used to remove obviously bad values, e.g., wind speed values less than
zero or pH values greater than 12. Erroneous data entered as -99 are
also removed. Processed data will have QAQC columns removed, in addition
to removal of values in the actual parameter columns that do not meet
the criteria.

The data are filtered by matching the flag columns with the character
string provided by `qaqc_keep`. A single combined string is created by
pasting each element together using the '\|' operator, then using
partial string matching with [`grepl`](https://rdrr.io/r/base/grep.html)
to compare the actual flags in the QAQC columns. Values that can be
passed to the function are those described online:
<https://cdmo.baruch.sc.edu/data/qaqc.cfm>.

## See also

[`qaqcchk`](http://fawda123.github.io/SWMPr/reference/qaqcchk.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## get data
data(apadbwq)
dat <- apadbwq

## retain only '0' and '-1' flags
qaqc(dat, qaqc_keep = c('0', '-1'))

## retain observations with the 'CSM' error code
qaqc(dat, qaqc_keep = 'CSM')
} # }
```
