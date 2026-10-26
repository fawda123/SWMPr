# Summary of QAQC flags in SWMP data

Summary of the number of observations with a given QAQC flag for each
parameter column of a swmpr object

## Usage

``` r
qaqcchk(swmpr_in)

# S3 method for class 'swmpr'
qaqcchk(swmpr_in)
```

## Arguments

- swmpr_in:

  input swmpr object

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html) with
columns for swmpr parameters and row counts indicating the number of
observations in each parameter assigned to a flag value.

## Details

Viewing the number of observations for each parameter that are assigned
to a QAQC flag may be useful for deciding how to process the data qith
qaqc. The `qaqcchk` function can be used to view this information.
Consult the online documentation for a description of each QAQC flag:
<https://cdmo.baruch.sc.edu/data/qaqc.cfm>

## See also

[`qaqc`](http://fawda123.github.io/SWMPr/reference/qaqc.md)

## Examples

``` r
## get data
data(apadbwq)
dat <- apadbwq

## view the number observations in each QAQC flag
qaqcchk(dat)
#>                flag f_cdepth f_chlfluor f_depth f_do_mgl f_do_pct f_level  f_ph
#> 1                       1399          0       0        0        0       0     0
#> 2              <-1>        0      70176       0        0        0   70176     0
#> 3              <-2>        0          0    1403     1403     1403       0  1403
#> 4        <-2> [GCM]        6          0       0        0        0       0     0
#> 5        <-3> (CBF)        0          0       0        0        0       0     0
#> 6        <-3> (CSM)        0          0       0        0        0       0  1456
#> 7        <-3> [GCR]     1832          0       0        0        0       0     0
#> 8  <-3> [SDG] (CSM)        0          0       0        0        0       0  1327
#> 9  <-3> [SSD] (CBF)        0          0       0       21       21       0    21
#> 10 <-3> [SSM] (CSM)        0          0     448      448      448       0  1885
#> 11       <-3> [STF]        0          0    1349     1349     1349       0  1349
#> 12 <-3> [STF] (CND)        0          0       1        1        1       0     1
#> 13       <-3> [STS]        0          0       0        0        0       0     0
#> 14 <-3> [STS] (CBF)        0          0       0        0        0       0     0
#> 15 <-3> [STS] (CRE)        0          0       0        0        0       0     0
#> 16 <-3> [STS] (CSM)        0          0       0        0        0       0     0
#> 17              <0>        0          0   66557    65451    65452       0 59825
#> 18        <0> (CDA)        0          0       0      298      298       0     0
#> 19        <0> (CND)        0          0      40       40       40       0    32
#> 20        <0> (CRE)        0          0     230        0        0       0     0
#> 21        <0> (CSM)        0          0      89        0        0       0     0
#> 22        <0> (CTS)        0          0       0        1        0       0     0
#> 23        <0> (CWE)        0          0      59        0        0       0     0
#> 24  <0> [STS] (CRE)        0          0       0        0        0       0     0
#> 25  <0> [STS] (CSM)        0          0       0        0        0       0     0
#> 26        <1> (CAF)        0          0       0        0        0       0     0
#> 27        <1> (CBF)        0          0       0        0        0       0     0
#> 28        <1> (CND)        0          0       0        0        0       0     1
#> 29        <1> (CSM)        0          0       0        0        0       0  2876
#> 30        <1> [SDO]        0          0       0     1164     1164       0     0
#> 31        <1> [SIC]        0          0       0        0        0       0     0
#> 32        <1> [SSM]        0          0       0        0        0       0     0
#> 33  <1> [SSM] (CND)        0          0       0        0        0       0     0
#> 34  <1> [SSM] (CSM)        0          0       0        0        0       0     0
#> 35        <1> [STS]        0          0       0        0        0       0     0
#> 36  <1> [STS] (CND)        0          0       0        0        0       0     0
#> 37  <1> [STS] (CRE)        0          0       0        0        0       0     0
#> 38  <1> [STS] (CSM)        0          0       0        0        0       0     0
#> 39              <3>    66939          0       0        0        0       0     0
#>    f_sal f_spcond f_temp f_turb
#> 1      0        0      0      0
#> 2      0        0      0      0
#> 3   1403     1403   1403   1403
#> 4      0        0      0      0
#> 5      0        0      0   1523
#> 6      0        0      0     54
#> 7      0        0      0      0
#> 8      0        0      0      0
#> 9      0        0      0      0
#> 10   448      448    448    448
#> 11  1349     1349   1349   1349
#> 12     1        1      1      1
#> 13     0        0      0    274
#> 14     0        0      0    532
#> 15     0        0      0     38
#> 16     0        0      0    965
#> 17 66793    66915  59615  54887
#> 18     0        0      0      0
#> 19    40       40     35     37
#> 20   122        0      0    634
#> 21     0        0      0    282
#> 22     0        0      0   2605
#> 23     0        0      0      0
#> 24     0        0      0   1133
#> 25     0        0      0     38
#> 26     0        0      0    712
#> 27    20       20     20      0
#> 28     0        0      0      1
#> 29     0        0      0   1296
#> 30     0        0      0      0
#> 31     0        0      0     24
#> 32     0        0   6138      0
#> 33     0        0      5      0
#> 34     0        0   1162      0
#> 35     0        0      0    765
#> 36     0        0      0      1
#> 37     0        0      0      1
#> 38     0        0      0   1173
#> 39     0        0      0      0
```
