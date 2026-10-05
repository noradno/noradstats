# Prepare Norfund DIM Data (internal function)

This is an \*\*internal helper function\*\* and is not exported for
direct use. It imports statsys data and process the data to include only
relevant Norfund DIM data (Development Investment Mandate (DIM).
Excludes CIF agreements from 2022 and onwards (keeping old agreements
before CIM was created - as there are old agreements also transfered
from DIM to CIM in 2022)

## Usage

``` r
prepare_norfund_dim_data(df_cim)
```

## Value

A data frame of the Norfund DIM data.
