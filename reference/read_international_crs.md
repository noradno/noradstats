# Read international CRS data into R

This function connects to a specified DuckDB database table to retrieve
CRS (Creditor Reporting System) activity level data of Official
Development Finance (ODA/OOF) from official donors (DAC, non-DAC,
multilaterals) and private philantropy, starting from 1960 to the recent
year. It returns all the data in the database table as a tibble. The
database file is expected to be located in a synchronized SharePoint
directory on the user's local computer. Data source:
https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1

## Usage

``` r
read_international_crs()
```

## Value

A tibble containing international CRS data.

## Examples

``` r
?read_oda()
```
