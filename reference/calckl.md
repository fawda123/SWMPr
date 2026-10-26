# Calculate oxygen mass transfer coefficient

Calculate oxygen mass transfer coefficient using equations in Thiebault
et al. 2008. Output is used to estimate the volumetric reaeration
coefficient for ecosystem metabolism.

## Usage

``` r
calckl(temp, sal, atemp, wspd, bp, height = 10)
```

## Arguments

- temp:

  numeric for water temperature (C)

- sal:

  numeric for salinity (ppt)

- atemp:

  numeric for air temperature (C)

- wspd:

  numeric for wind speed (m/s)

- bp:

  numeric for barometric pressure (mb)

- height:

  numeric for height of anemometer (meters)

## Value

Returns numeric value for oxygen mass transfer coefficient (m d-1).

## Details

This function is used within the
[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md)
function and should not be used explicitly.

## References

Ro KS, Hunt PG. 2006. A new unified equation for wind-driven surficial
oxygen transfer into stationary water bodies. Transactions of the
American Society of Agricultural and Biological Engineers.
49(5):1615-1622.

Thebault J, Schraga TS, Cloern JE, Dunlavey EG. 2008. Primary production
and carrying capacity of former salt ponds after reconnection to San
Francisco Bay. Wetlands. 28(3):841-851.

## See also

[`ecometab`](http://fawda123.github.io/SWMPr/reference/ecometab.md)
