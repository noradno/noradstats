# Read aidresults.no data into R from user-specified CSV file

This function reads a CSV file of ODA statistics downloaded from
aidresults.no: https://resultater.norad.no/microdata The function
excepcts the CSV to be delimited by semicolons (;) commas (,) as decimal
marks and UTF-16LE encoding. It expects the CSV file to be at the
specified path and returns a tibble containing the data. Make sure the
path is correctly specified to avoid errors. The function checks for
valid path input before attempting to read the file.

## Usage

``` r
read_aidresults_from_csv(path)
```

## Arguments

- path:

  Required path to CSV file of Offical Norwegian ODA data downloaded
  from aidresults.no: https://resultater.norad.no/microdata

## Value

Returns a tibble of Norwegian ODA data

## Examples

``` r
?read_aidresults_from_csv
```
