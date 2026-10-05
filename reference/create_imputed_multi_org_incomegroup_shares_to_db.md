# Import and save imputed multilateral to income group shares by organisation into DuckDB database

This is the exported main function that calls internal helper functions
to import and save the imputed multilateral org income group shares data
into the DuckDB database.

## Usage

``` r
create_imputed_multi_org_incomegroup_shares_to_db(filepath)
```

## Arguments

- filepath:

  A string. The path to the Excel file containing the imputed
  multilateral org income group shares data. The Excel file must contain
  the following columns: - \`aid_type\`: Character string representing
  the type of aid. - \`agreement_partner\`: Character string
  representing the partner involved in the agreement. -
  \`income_group\`: Character string representing the multilateral
  marker, meaning health, education etc. - \`year\`: Integer
  representing the year. - \`share\`: Numeric value representing the
  share of the sectors

## Value

A data frame containing the imported multilateral org income group
shares data with the following columns: \`aid_type\`,
\`agreement_partner\`, \`income_group\`, \`year\`, and \`share\`.

## Details

Steps: 1. Imports the imputed multilateral org income group shares data
from an Excel spreadsheet. The file must contain the columns:
\`aid_type\`, \`agreement_partner\`, \`income_group\`, \`year\`, and
\`share\`. The imported spreadsheet should not include any observations
with NA share, only real values. The share for each agreement partner is
calculated by the OECD secretariate and available online. 2. Saves the
imported data into the DuckDB database under the table name
'imputed_multi_org_incomegroup_shares'.

This function imports an Excel spreadsheet containing imputed
multilateral shares data, which must have the following columns:
\`aid_type\`, \`agreement_partner\`, \`income_group\`, \`year\`, and
\`share\`.

Once imported, the data is saved into a table named
\`imputed_multi_org_incomegroup_shares\` in the DuckDB database. This
table will overwrite any existing data with the same name in the
database.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Import and save multilateral org income group shares data from a file
create_imputed_multi_org_incomegroup_shares_to_db("path/to/imputed_multi_org_incomegroup_shares.xlsx")
} # }
```
