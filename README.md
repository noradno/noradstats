# noradstats


## Overview

Access Norwegian and international development aid data for internal use
in Norad.

- read\_\* functions for importing data into R as a tibble data frame

- access\_\* functions for using a remote tibble data frame linked to a
  database table

- add_cols\_\* functions for add columns to tibble data frames

## Installation

``` r
remotes::install_github("noradno/noradstats", build_vignettes = TRUE)
```

## Usage

Loading libraries for these examples.

``` r
library(noradstats)
library(dplyr)
```

### Norwegian ODA data

Read Norwegian ODA data into R, returning a tibble data frame.

``` r
df_oda <- noradstats::read_oda()

df_oda |> 
  filter(year == max(year))
```

Add climate-specific columns to Norwegian ODA data frame

``` r
df_oda |> 
  add_cols_climate_clean()
```

Access database of Norwegian ODA data from R, returning a tibble data
frame linked to the DuckDB database table. Allows for dplyr queries, and
use collect() to return data as tibble data frame. Use
DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.

``` r
df_remote_oda <- noradstats::access_oda()

df_remote_oda |> 
  filter(year == max(year)) |> 
  collect()

DBI::dbDisconnect(con, shutdown=TRUE)
```

### Norwegian imputed multilateral data

Read Norwegian imputed multilateral ODA to countries and regions from
the OECD SDMX Database API, returning a tibble data frame. Provide start
and end years as arguments.

``` r
df_imputed <- read_imputed_countries(startyear = 2020, endyear = 2022)
```

Read Norwegian imputed multilateral ODA to sectors from the OECD SDMX
Database API, returning a tibble data frame. Provide start and end years
as arguments.

``` r
df_imputed <- read_imputed_sectors(startyear = 2015, endyear = 2022)
```

### International CRS data

Read international ODA data into R, returning a tibble data frame. This
can take a while, approximately 30 seconds.

``` r
df_crs <- read_international_crs()

df_crs |> 
  filter(year == max(year))
```

Access database of international ODA data from R, returning a remote
tibble linked to the crs DuckDB database file. Allows for dplyr queries,
and use collect() to return data as tibble data frame. Use
DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.

``` r
df_remote_crs <- access_international_crs()

df_remote_crs |> 
  filter(year == max(year)) |> 
    collect()

DBI::dbDisconnect(con, shutdown=TRUE)
```

### International ODA data from DAC-countries to countries and regions

International ODA data from DAC-member countries to recipient countries
and regions. Provide start and end years as arguments.

``` r
df_dac <- read_donors(startyear = 2013, endyear = 2022)
```
