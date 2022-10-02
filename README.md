
# noradstats

<!-- badges: start -->
<!-- badges: end -->

The R package `noradstats` provides functions to access, import and analyze Norwegian development aid data.

## Installation

Install the `noradstats` package from [GitHub](https://github.com/noradno/noradstats):

``` r
devtools::install_github("noradno/noradstats")
```

## Usage

``` r
library(noradstats)

## noradstats::find_aiddata()

## noradstats::get_aiddata("statsys_ten.csv")

## df <- noradstats::read_aiddata("statsys_ten.csv")

## df <- noradstats::add_cols_basic(df)

## df <- noradstats::add_cols_climate(df)

## df_imputed_countries <- noradstats::get_imputed_countries()

## df_imputed_sectors <- noradstats::get_imputed_sectors()

## df_donors <- noradstats::get_donors()

```

