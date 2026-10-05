# Read Statsys data into R

This function imports all data from the statsys table in the DuckDB
database. This data includes Norwegian official development assistance
(ODA), ODA frame agreement level data, Other official flows(OOF), export
credits and private flows. The data covers 1960 to recent year. The
DuckDB database file is located on Norads Microsoft Sharepoint site and
is expected to be synced via Microsoft Teams to to the users local
directory.

## Usage

``` r
read_statsys(version = "statsys_official")
```

## Arguments

- version:

  A character string specifying which table to read from. If
  "statsys_official", the function reads from the "statsys_official"
  table. If "statsys_active", the function reads from the
  "statsys_active" table. Defaults to "statsys_official".

## Value

Returns a tibble of ODA, OOF and PF data from the statsys table in the
DuckDB database.

## Examples

``` r
?read_statsys()
```
