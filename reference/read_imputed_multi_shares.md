# Read Imputed Multilateral Shares data into R

This function imports from the DuckDB database a data frame of the
annual imputed multilateral shares calculated by the OECD-secretariate
(except for Norad estimates for the last year). A column named Marker
identifies the Marker share, for instance: climate.

## Usage

``` r
read_imputed_multi_shares()
```

## Value

Returns a tibble with two columns: \`aid_type\`, \`agreement_partner\`,
\`marker\`, \`year\` and \`share\`, from the 'imputed_multi_shares'
table in the DuckDB database.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the imputed_multi_shares table from the DuckDB database:
df_imputed_multi_shares <- read_imputed_multi_shares()

# Display the first few rows of the data:
head(df_imputed_multi_shares)
} # }
```
