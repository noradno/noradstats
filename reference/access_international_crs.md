# Access database of international CRS data from R

This function creates a proxy tibble connected to the CRS table in the
DuckDB database. This data includes CRS (Creditor Reporting System)
activity level data of Official Development Finance (ODA/OOF) from
official donors (DAC, non-DAC, multilaterals) and private philantropy,
starting from 1973 to the recent year. The DuckDB database file is
located on Norads Microsoft Sharepoint site and is expected to be synced
via Microsoft Teams to to the users local directory. Use
DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.
Data source: https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1

## Usage

``` r
access_international_crs()
```

## Value

Returns a proxy tibble connected to the CRS table in the DuckDB
database.

## Examples

``` r
?access_crs()
#> Error in .helpForCall(topicExpr, parent.frame()): no methods for ‘access_crs’ and no documentation for it as a function
```
