
# noradstats

<!-- badges: start -->
<!-- badges: end -->

The goal of the R package noradstats is to access Norwegian aid data for internal use in Norad

## Installation

You can install the development version of noradstats from [GitHub](https://github.com/noradno/noradstats) with:

``` r
# install.packages("devtools")
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

## df_imputed <- noradstats::get_imputed()

```

