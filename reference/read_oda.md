# Read Norwegian ODA data into R

This function imports all Norwegian Official Development (ODA) from the
Statsys table in the DuckDB database. This include ODA data from 1960 to
the recent year. Frame agreement level data is excluded. The DuckDB
database file is located on Norads Microsoft Sharepoint site and is
expected to be synced via Microsoft Teams to to the users local
directory.

## Usage

``` r
read_oda()
```

## Value

Returns a tibble of Norwegian ODA data from 1960 to the recent year.
Frame agreement level data is excluded.

## Examples

``` r
?read_oda()
```
