# Read Norwegian imputed multilateral ODA to sectors data into R

This function connects to the OECD SDMX API and extracts Norwegian
imputed multilateral ODA to sectors data by year, including metadata.
The amounts are USD million and caclulates NOK million using the
exchange rate data from the OECD API.

## Usage

``` r
read_imputed_sectors(startyear = 2020, endyear = 2020)
```

## Arguments

- startyear:

  Specify a numeric value of the first year in time period. Default
  value is *2011*.

- endyear:

  Specity a numeric value of the last year in time period. Default value
  is *2020*.

## Value

Returns a tibble dataframe of Norwegian imputed multilateral ODA to
sectors

## Examples

``` r
?df_imputed_sectors <- read_imputed_sectors()
```
