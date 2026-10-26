# Example water quality data for Apalachicola Bay Dry Bar station.

An example water quality dataset for Apalachicola Bay Dry Bar station.
The data are a
[`swmpr`](http://fawda123.github.io/SWMPr/reference/swmpr.md) object
that have been imported into R from csv files using the
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
apadbwq
```

## Format

A [`swmpr`](http://fawda123.github.io/SWMPr/reference/swmpr.md) object
and [`data.frame`](https://rdrr.io/r/base/data.frame.html) with 70176
rows and 25 variables:

- `datetimestamp`:

  POSIXct

- `temp`:

  num

- `f_temp`:

  chr

- `spcond`:

  num

- `f_spcond`:

  chr

- `sal`:

  num

- `f_sal`:

  chr

- `do_pct`:

  num

- `f_do_pct`:

  chr

- `do_mgl`:

  num

- `f_do_mgl`:

  chr

- `depth`:

  num

- `f_depth`:

  chr

- `cdepth`:

  num

- `f_cdepth`:

  chr

- `level`:

  num

- `f_level`:

  chr

- `clevel`:

  num

- `f_clevel`:

  chr

- `ph`:

  num

- `f_ph`:

  chr

- `turb`:

  num

- `f_turb`:

  chr

- `chlfluor`:

  num

- `f_chlfluor`:

  chr

## Source

<https://s3.amazonaws.com/swmpexdata/zip_ex.zip>

## Examples

``` r
data(apadbwq)
```
