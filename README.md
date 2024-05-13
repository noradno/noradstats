# noradstats


## Overview

Easily import Norwegian and international development finance data to R.

- Use the `read_*()` functions for importing all data from a specific
  database table or CSV into the R environment

  - `read_oda()` to import all Norwegian ODA data from Norad database

  - `read_statsys()` to import all Norwegian ODA, OOF and PF data from
    Norad database

  - `read_international_crs()` to import all international CRS data from
    Norad database

  - `read_donors()` to import DAC donors ODA by recipient countries and
    regions from an OECD database

  - `read_imputed_countries()` to import Norwegian imputed multilateral
    ODA by recipient countries and regions from an OECD database

  - `read_imputed_sectors()` to import Norwegian imputed multilateral
    ODA by sectors from an OECD database

  - `read_statsys_from_csv()` to import all Norwegian ODA, OOF and PF
    data from CSV

  - `read_aidresults_from_csv()` to import data from an
    aidresults.no-produced CSV

- Use the `access_*()` functions for creating a proxy data frame linked
  to a specific Norad database table

  - `access_oda()` to access Norwegian ODA data in a Norad database

  - `access_statsys()` to access Norwegian ODA, OOF and PF data in Norad
    database

  - `access_international_crs()` to access all international CRS data in
    Norad database

- \[Under development\] Use the `add_cols_*` functions to add extra
  column to an existing data of produced by `read_oda()`,
  `read_statsys()`, `read_statsys_from_csv()` and the
  `access_*()`-equivalents. `add_cols_basic()` columns are already
  included as columns in the data frames produced by `read_oda()`,
  `read_statsys()` and `access_*()`-equvalents.

  - `add_cols_basic()` adds columns of

  - `add_cols_climate()` add climate relevant columns

  - `add_cols_climate_clean()` add climate relevant columns

  - `add_cols_countrycode()` add iso3c country codes using the
    countrycodes package

  - `add_cols_violence()` add conflict relevant columns using UCDP
    conflict data

## Installation

``` r
remotes::install_github("noradno/noradstats", build_vignettes = TRUE)
```

## Usage

### Prerequisites

A number of functions in the noradstats package retrieve data from a
database file located in the users local directory:
*C:\Users\[userid\]\Norad\Norad-Avd-Kunnskap - Statistikk og analyse\06.
Statistikkdatabaser\3. Databasefiler.* Make sure the file path exists
locally by synchronizing the Teams channel *Norad-Avd-Kunnskap -
Statistikk og analyse.*

### Loading packages

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

The function read_oda() reads all Norwegian ODA data from the DuckDB
database into R.

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
