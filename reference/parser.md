# Parse web results for swmpr

Parsing function for objects returned from CDMO web services

## Usage

``` r
parser(resp_in, parent_in = "data")
```

## Arguments

- resp_in:

  web object returned from CDMO server, response class from httr package

- parent_in:

  chr string of parent nodes to parse

## Value

Returns a `data.frame` of parsed XML nodes

## Details

This function parses XML objects returned from the CDMO server, which
are further passed to
[`swmpr`](http://fawda123.github.io/SWMPr/reference/swmpr.md). It is
used internally by the data retrieval functions, excluding
[`import_local`](http://fawda123.github.io/SWMPr/reference/import_local.md).
The function does not need to be called explicitly.
