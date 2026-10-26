# Dissolved oxygen at saturation

Finds dissolved oxygen concentration in equilibrium with water-saturated
air. Function and documentation herein are from archived wq package.

## Usage

``` r
oxySol(t, S, P = NULL)
```

## Arguments

- t:

  numeric for temperature, degrees C

- S:

  numeric for salinity, on the Practical Salinity Scale

- P:

  numeric for pressure, atm

## Value

Dissolved oxygen concentration in mg/L at 100% saturation. If
`P = NULL`, saturation values at 1 atm are calculated.

## Details

Calculations are based on the approach of Benson and Krause (1984),
using Green and Carritt's (1967) equation for dependence of water vapor
partial pressure on `t` and `S`. Equations are valid for temperature in
the range 0-40 C and salinity in the range 0-40.

## References

Benson, B.B. and Krause, D. (1984) The concentration and isotopic
fractionation of oxygen dissolved in fresh-water and seawater in
equilibrium with the atmosphere. *Limnology and Oceanography* **29,**
620-632.

Green, E.J. and Carritt, D.E. (1967) New tables for oxygen saturation of
seawater. *Journal of Marine Research* **25,** 140-147.
