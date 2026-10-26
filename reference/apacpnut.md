# Example nutrient data for Apalachicola Bay Cat Point station.

An example nutrient dataset for Apalachicola Bay Cat Point station. The
data are a [`swmpr`](http://fawda123.github.io/SWMPr/reference/swmpr.md)
object that have been imported into R from csv files using the
[`import_local`](http://fawda123.github.io/SWMPr/reference/import_local.md)
function. The raw data were obtained from the CDMO data portal but can
also be accessed from a zip file created for this package. See the
source below. The help file for the
[`import_local`](http://fawda123.github.io/SWMPr/reference/import_local.md)
function describes how the data can be imported from the zip file.
Attributes of the dataset include `names`, `row.names`, `class`,
`station`, `parameters`, `qaqc_cols`, `date_rng`, `timezone`, and
`stamp_class`.

## Usage

``` r
apacpnut
```

## Format

A [`swmpr`](http://fawda123.github.io/SWMPr/reference/swmpr.md) object
and [`data.frame`](https://rdrr.io/r/base/data.frame.html) with 215 rows
and 13 variables:

- `datetimestamp`:

  POSIXct

- `po4f`:

  num

- `f_po4f`:

  chr

- `nh4f`:

  num

- `f_nh4f`:

  chr

- `no2f`:

  num

- `f_no2f`:

  chr

- `no3f`:

  num

- `f_no3f`:

  chr

- `no23f`:

  num

- `f_no23f`:

  chr

- `chla_n`:

  num

- `f_chla_n`:

  chr

## Source

<https://s3.amazonaws.com/swmpexdata/zip_ex.zip>

## Examples

``` r
data(apacpnut)
```
