# Get the local path to the DuckDB database file

Returns the user-specific file path to the shared DuckDB database. The
database file is stored in a SharePoint-synced folder, but is accessed
purely as a local file on disk (no network or API access).

## Usage

``` r
get_duckdb_path()
```

## Value

A character string containing the full local path to the DuckDB database
file.

## Details

This function is used by all read\_\*() and create\_\*() functions that
interact with the DuckDB database.
