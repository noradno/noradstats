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

In addition to `noradstats`, for the examples below we use the packages
`dplyr`, `DBI` and `duckplyr`. `dplyr` provides a wide range of data
manipulation functions for data frames, `DBI` helps connecting R to
databases, and the `duckplyr` package handles the under-the-hood
translation of dplyr operations to SQL queries, optimized for DuckDB
databases (compared to the more general `dbplyr` package). The packages
`DBI` and `duckplyr` are only necessary when using the `access_*`
functions, as these functions retrieve data from a database.

``` r
library(noradstats)
library(dplyr)
library(DBI)
library(duckplyr)
```

### Norwegian ODA data

#### Read Norwegian ODA data into R, returning a tibble data frame.

In the example below we find the sum of Norwegian ODA in the most recent
year.

``` r
df_oda <- read_oda()

df_oda |> 
  filter(year == max(year)) |> 
  summarise(nok_mrd = sum(disbursed_mrd_nok, na.rm = T))
```

#### Access database of Norwegian ODA

Instead of loading all the data of Norwegian ODA data into the R
environment (in-memory), we can use the `access_oda()` function to
create a proxy data frame linked to the DuckDB database table containing
the Norwegian ODA data. The proxy data frame can be manipulated using
`dplyr` operations (through `duckplyr` translation to SQL queries),
executed directly on the database. Use `collect()` to return the query
results in the R environment. This approach is more efficient for
handling large datasets by reducing memory usage and faster data
processing than loading all data into memory. However, the Norwegian ODA
data is relatively small, so the difference in performance is not as
noticeable as with larger datasets.

In the example below we find the sum of Norwegian ODA in the most recent
year.

``` r
df_proxy_oda <- access_oda()

df_proxy_oda |> 
  filter(year == max(year)) |> 
  summarise(nok_mrd = sum(disbursed_mrd_nok, na.rm = T)) |> 
  collect()
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

Instead of loading all the data of international CRS data into the R
environment (in-memory), we can use the `access_international_crs()`
function to create a proxy data frame linked to the DuckDB database
table containing the international CRS data. The proxy data frame can be
manipulated using `dplyr` operations (through `duckplyr` translation to
SQL queries), executed directly on the database. Use `collect()` to
return the query results in the R environment. This approach is more
efficient for handling large datasets by reducing memory usage and
faster data processing than loading all data into memory.

``` r
df_proxy_crs <- access_international_crs()

df_proxy_crs |> 
  filter(year == max(year)) |> 
    collect()
```

### International ODA data from DAC-countries to countries and regions

International ODA data from DAC-member countries to recipient countries
and regions. Provide start and end years as arguments.

``` r
df_dac <- read_donors(startyear = 2013, endyear = 2022)
```
