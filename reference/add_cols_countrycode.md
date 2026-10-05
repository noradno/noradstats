# Add ISO3 country code columns to dataframe

Add ISO3 country code columns to dataframe

## Usage

``` r
add_cols_countrycode(data)
```

## Arguments

- data:

  Input data frame of Norwegian development assistance, with column
  *Recipient country* containing country names.

## Value

Returns data frame with additional column:

- iso3: iso3 character code, identified using package *countrycode*
  based on column *Recipient country*. Non-matches are given NA values
  and are returned in a warning message.

## Examples

``` r
?add_cols_countrycode()
```
