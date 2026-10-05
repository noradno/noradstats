# Function to import, save and overwrite Statsys Data to DuckDB Database

Reads Statsys data from a CSV file, processes it by adding basic and
country columns, cleans the column names, and then saves the data to a
DuckDB database. The existing data in the "statsys" table in the
database will be overwritten.

## Usage

``` r
create_statsys_data_to_db(input_csv, version = NULL)
```

## Arguments

- input_csv:

  A string. Path to the input CSV file containing the Statsys data.

## Details

Before running this function, it is recommended that users first inspect
the data by running the \`read_statsys_from_csv()\` function to ensure
that the data has the expected structure and column types.

The function checks if both the CSV file and the DuckDB database file
exist. If either file is not accessible, the function will stop and
return an error message. It uses internal \`noradstats\` functions to
read and process the data by adding basic and country-related columns.
The \`janitor\` package is used to clean column names. The resulting
data is then written to the DuckDB database, overwriting the existing
"statsys" table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
# Path to the CSV file containing the Statsys data
input_csv <- "path/to/your_statsys_data.csv"

# Call the function to save and overwrite the Statsys data
create_statsys_data_to_db(input_csv, version = "statsys_official")
} # }
```
