# Read Norfund DIM Portfolio Climate Share data into R

This function imports a data frame of the annual climate mitigation
share (2-year averages) of the Norfund DIM (Development Investment
Mandate) portfolio from the DuckDB database.

## Usage

``` r
read_norfund_dim_portfolio_climate_share()
```

## Value

Returns a tibble with two columns: \`year\` and \`climate_share\`, from
the 'norfund_dim_portfolio_climate_share' table in the DuckDB database.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the Norfund DIM Portfolio Climate Share from the DuckDB database:
df_climate_share <- read_norfund_dim_portfolio_climate_share()

# Display the first few rows of the data:
head(df_climate_share)
} # }
```
