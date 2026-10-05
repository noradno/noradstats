# Read PTA Disbursement level data table from DuckDB into R

This function imports from the DuckDB database a data frame of PTA
Disbursement level

## Usage

``` r
read_pta_disbursement_level()
```

## Value

Returns a tibble with many columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the pta_disbursement_level table from the DuckDB database:
df_pta_disbursement_level <- read_pta_disbursement_level()

# Display the first few rows of the data:
head(df_pta_disbursement_level)
} # }
```
