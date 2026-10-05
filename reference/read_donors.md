# Read international ODA data from OECD/DAC donors to countries and regions into R

This function connects to the OECD SDMX API and extracts data
DAC-members' ODA to countries and regions by year (DAC2a), including
metadata. Amounts in USD million

## Usage

``` r
read_donors(startyear = 2020, endyear = 2020)
```

## Arguments

- startyear:

  Specify a numeric value of the first year in time period. Default
  value is *2020*.

- endyear:

  Specity a numeric value of the last year in time period. Default value
  is *2020*.

## Value

Returns a dataframe (tibble) on OECD DAC donors ODA to countries and
regions (DAC2a)

## Examples

``` r
?df_donors <- read_donors()
```
