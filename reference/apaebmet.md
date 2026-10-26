# Example weather data for Apalachicola Bay East Bay station.

An example weather dataset for Apalachicola Bay East Bay station. The
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
apaebmet
```

## Format

A [`swmpr`](http://fawda123.github.io/SWMPr/reference/swmpr.md) object
and [`data.frame`](https://rdrr.io/r/base/data.frame.html) with 70176
rows and 23 variables:

- `datetimestamp`:

  POSIXct

- `atemp`:

  num

- `f_atemp`:

  chr

- `rh`:

  num

- `f_rh`:

  chr

- `bp`:

  num

- `f_bp`:

  chr

- `wspd`:

  num

- `f_wspd`:

  chr

- `maxwspd`:

  num

- `f_maxwspd`:

  chr

- `wdir`:

  num

- `f_wdir`:

  chr

- `sdwdir`:

  num

- `f_sdwdir`:

  chr

- `totpar`:

  num

- `f_totpar`:

  chr

- `totprcp`:

  num

- `f_totprcp`:

  chr

- `cumprcp`:

  num

- `f_cumprcp`:

  chr

- `totsorad`:

  num

- `f_totsorad`:

  chr

## Source

<https://s3.amazonaws.com/swmpexdata/zip_ex.zip>

## Examples

``` r
data(apaebmet)
```
