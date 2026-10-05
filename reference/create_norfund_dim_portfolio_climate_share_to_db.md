# Create a table of the Norfund DIM Portfolio Climate Share and store it in the DuckDB database

This function calculates the annual climate mitigation share (2 year
averages) of the Norfund DIM (development mandate) portfolio and saves
the year-share data frame in the DuckDB database. The user must provide
an Excel file containing the Norfund CIM (Climate Investment Mandate)
agreements to be excluded.

## Usage

``` r
create_norfund_dim_portfolio_climate_share_to_db(cim_filepath)
```

## Arguments

- cim_filepath:

  A string. The path to the Excel file containing the CIM agreements to
  exclude. Default is "avtalenr_cim.xlsx". The file must contain the two
  columns \*agreement_number\* and \*cim_dim\*.

## Value

This function does not return a value. It saves the climate share table
to the DuckDB database and displays a success message upon completion.

## Details

After running this function, the 'norfund_dim_climate_share' table will
be created or overwritten in the DuckDB database, and a success message
will be displayed. The
\`create_norfund_dim_portfolio_climate_share_to_db()\` function performs
the following steps using internal helper functions:

- \`import_cim_data()\`: Imports the CIM agreements data from an Excel
  file.

- \`prepare_norfund_dim_data()\`: Prepares the Norfund DIM data by
  importing statsys data, filtering out CIM agreements from 2022
  onwards, and performing necessary data processing.

- \`calculate_climate_share()\`: Calculates the two-year climate
  mitigation share.

- \`save_climate_share_to_db()\`: Saves the calculated climate share
  data to the DuckDB database.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use default CIM file path
create_norfund_dim_portfolio_climate_share_to_db()

# Specify a different CIM file path
create_norfund_dim_portfolio_climate_share_to_db("path/to/cim_file.xlsx")
} # }
```
