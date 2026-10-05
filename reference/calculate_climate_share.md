# Calculate Climate Mitigation Share for Norfund DIM (internal function)

This is an \*\*internal helper function\*\* and is not exported for
direct use. It calculates the two-year climate mitigation share for the
Norfund DIM data. Removes rows with NA climate_share (2014, the first
year)

## Usage

``` r
calculate_climate_share(df_norfund_dim)
```

## Value

A data frame containing the climate mitigation shares.
