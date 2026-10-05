# Read Imputed Multilateral Income Group Shares by oganisation data into R

This function imports from the DuckDB database a data frame of the
annual imputed multilateral org income group shares calculated by the
OECD-secretariate (except for Norad estimates for the last year). A
column named income_gropu identifies the income_group share, for
instance: LDCs

## Usage

``` r
read_imputed_multi_org_incomegroup_shares()
```

## Value

Returns a tibble with two columns: \`aid_type\`, \`agreement_partner\`,
\`income_group\`, \`year\` and \`share\`, from the
'imputed_multi_org_incomegroup_shares' table in the DuckDB database.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read the imputed_multi_org_incomegroup_shares table from the DuckDB database:
df_imputed_multi_org_incomegroup_shares <- read_imputed_multi_org_incomegroup_shares()

# Display the first few rows of the data:
head(df_imputed_multi_org_incomegroup_shares)
} # }
```
