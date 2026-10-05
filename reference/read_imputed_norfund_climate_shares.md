# Read Imputed Norfund Climate Shares data into R

This function imports from the DuckDB database a data frame of the
annual imputed Norfund climate shares (2 year averages) from 2015 and
later.

## Usage

``` r
read_imputed_norfund_climate_shares()
```

## Value

Returns a tibble with two columns: \`agreement_number\`, \`year\`, and
\`climate_share\` from the 'imputed_norfund_climate_share' table in the
DuckDB database.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the imputed_norfund_climate_share table from the DuckDB database:
df_imputed_norfund_climate_shares <- read_imputed_norfund_climate_shares()

# Display the first few rows of the data:
head(df_imputed_norfund_climate_shares)
} # }
```
