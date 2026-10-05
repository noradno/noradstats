# Read PTA Agreement Totals data table from DuckDB into R

This function imports from the DuckDB database a data frame of PTA
Agreement Totals from 1960 to today.

## Usage

``` r
read_pta_agreement_totals()
```

## Value

Returns a tibble with 7 columns: \`agreement_no\`, \`agreement_title\`,
\`agreement_partner\`, \`agreement_period\`, \`agreement_period_from\`,
\`agreement_period_to\` and \`expected_agreement_total\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the pta_agreement_totals table from the DuckDB database:
df_pta_agreement_totals <- read_pta_agreement_totals()

# Display the first few rows of the data:
head(df_pta_agreement_totals)
} # }
```
