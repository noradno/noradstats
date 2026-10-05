# Read Norwegian imputed multilateral ODA to countries and regions data into R

This function connects to the OECD SDMX API and extracts Norwegian
imputed multilateral ODA to countries and regions data by year,
including metadata. The amounts are USD million and caclulates NOK
million using the exchange rate data from the OECD API.

## Usage

``` r
read_imputed_countries(startyear = 2011, endyear = 2020)
```

## Arguments

- startyear:

  Specify a numeric value of the first year in time period. Default
  value is *2011*.

- endyear:

  Specity a numeric value of the last year in time period. Default value
  is *2020*.

## Value

Returns a dataframe (tibble) of Norwegian imputed multilateral ODA to
countries and regions

## Examples

``` r
?df_imputed_countries <- read_imputed_countries()
```
