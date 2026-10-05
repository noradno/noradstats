# noradstats

## Overview

Easily import Norwegian and international development finance data into
R using the `read_*()` functions or `access_*()` functions. Add extra
columns using the `add_cols*()`. The data is stored in databases. Some
functions use user-specified CSV files as data source.

- Use the `read_*()` functions for importing all existing data from a
  specific database table or CSV into the R environment.

  - [`read_oda()`](https://noradno.github.io/noradstats/reference/read_oda.md)
    to import all Norwegian ODA data from the database

  - [`read_statsys()`](https://noradno.github.io/noradstats/reference/read_statsys.md)
    to import all Norwegian ODA, OOF and PF data from the database

  - [`read_international_crs()`](https://noradno.github.io/noradstats/reference/read_international_crs.md)
    to import all international CRS data from the database

  - [`read_donors()`](https://noradno.github.io/noradstats/reference/read_donors.md)
    to import DAC donors ODA by recipient countries and regions from an
    OECD database

  - [`read_imputed_countries()`](https://noradno.github.io/noradstats/reference/read_imputed_countries.md)
    to import Norwegian imputed multilateral ODA by recipient countries
    and regions from the OECD database

  - [`read_imputed_sectors()`](https://noradno.github.io/noradstats/reference/read_imputed_sectors.md)
    to import Norwegian imputed multilateral ODA by sectors from the
    OECD database

  - [`read_statsys_from_csv()`](https://noradno.github.io/noradstats/reference/read_statsys_from_csv.md)
    to import all Norwegian ODA, OOF and PF data from a user-specified
    CSV

  - [`read_aidresults_from_csv()`](https://noradno.github.io/noradstats/reference/read_aidresults_from_csv.md)
    to import data from a user-specified CSV downloaded from
    aidresults.no

- Use the `access_*()` functions for creating a proxy data frame linked
  to a specific database table. This allows for using dplyr operations
  for querying directly on the database before retrieving the resulting
  data into memory.

  - [`access_oda()`](https://noradno.github.io/noradstats/reference/access_oda.md)
    to access Norwegian ODA data in the database

  - [`access_statsys()`](https://noradno.github.io/noradstats/reference/access_statsys.md)
    to access Norwegian ODA, OOF and PF data in the database

  - [`access_international_crs()`](https://noradno.github.io/noradstats/reference/access_international_crs.md)
    to access all international CRS data in the database

- \[Under development\] Use the `add_cols_*` functions to add extra
  column to an existing data of produced by
  [`read_oda()`](https://noradno.github.io/noradstats/reference/read_oda.md),
  [`read_statsys()`](https://noradno.github.io/noradstats/reference/read_statsys.md),
  [`read_statsys_from_csv()`](https://noradno.github.io/noradstats/reference/read_statsys_from_csv.md)
  and the `access_*()`-equivalents.
  [`add_cols_basic()`](https://noradno.github.io/noradstats/reference/add_cols_basic.md)
  columns are already included as columns in the data frames produced by
  [`read_oda()`](https://noradno.github.io/noradstats/reference/read_oda.md),
  [`read_statsys()`](https://noradno.github.io/noradstats/reference/read_statsys.md)
  and `access_*()`-equvalents.

  - [`add_cols_basic()`](https://noradno.github.io/noradstats/reference/add_cols_basic.md)
    adds columns of

  - `add_cols_climate()` add climate relevant columns

  - `add_cols_climate_clean()` add climate relevant columns

  - [`add_cols_countrycode()`](https://noradno.github.io/noradstats/reference/add_cols_countrycode.md)
    add iso3c country codes using the countrycodes package

  - [`add_cols_violence()`](https://noradno.github.io/noradstats/reference/add_cols_violence.md)
    add conflict relevant columns using UCDP conflict data

## Installation

``` r

remotes::install_github("noradno/noradstats")
```

## Usage

### Prerequisites

Several of the functions in the noradstats package conntects to a DuckDB
database file. The DuckDB database file is located on Norads Microsoft
Sharepoint site and is expected to be synced via Microsoft Teams to to
the users local directory: *C:\[userid\]-Avd-Kunnskap - Statistikk og
analyse\06. Statistikkdatabaser\3. Databasefiler.*

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

#### Read Norwegian ODA data to a tibble

The function
[`read_oda()`](https://noradno.github.io/noradstats/reference/read_oda.md)
imports all Norwegian ODA data from the DuckDB database into R.

In the example below we find the sector allocation of Norwegian ODA in
the most recent year.

``` r

df_oda <- read_oda()

df_oda |> 
  filter(year == max(year)) |> 
  summarise(disbursed_mill_nok = sum(disbursed_mill_nok, na.rm = T), .by = target_area) |> 
  arrange(desc(disbursed_mill_nok))
```

#### Access database of Norwegian ODA

Instead of loading all the data of Norwegian ODA data into the R
environment (in-memory), we can use the `access_*()` functions to create
a proxy data frame linked to the DuckDB database.

The functiokn
[`access_oda()`](https://noradno.github.io/noradstats/reference/access_oda.md)
creates a proxy tibble connected to the Statsys table in the DuckDB
database, and filters the proxy tibble to only include Norwegian
Official Development (ODA). Frame agreement level data is excluded. The
data covers 1960 to recent year.

The proxy data frame can be manipulated using `dplyr` operations
(through `duckplyr` translation to SQL queries), executed directly on
the database. Use
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) to
return the query results in the R environment. This approach is more
efficient for handling large datasets by reducing memory usage and
faster data processing than loading all data into memory. However, the
Norwegian ODA data is relatively small, so the difference in performance
is not as noticeable as with larger datasets.

In the example below we find the sector allocation of Norwegian ODA in
the most recent year.

``` r

df_proxy_oda <- access_oda()

df_proxy_oda |> 
  filter(year == max(year)) |> 
  summarise(disbursed_mill_nok = sum(disbursed_mill_nok, na.rm = T), .by = target_area) |> 
  arrange(desc(disbursed_mill_nok)) |> 
  collect()
```

### Norwegian imputed multilateral data

Import Norwegian imputed multilateral ODA to countries and regions from
the OECD SDMX Database API, returning a tibble. Provide start and end
years as arguments.

``` r

df_imputed_countries <- read_imputed_countries(startyear = 2020, endyear = 2022)
```

Read Norwegian imputed multilateral ODA to sectors from the OECD SDMX
Database API, returning a tibble. Provide start and end years as
arguments.

``` r

df_imputed_sectors <- read_imputed_sectors(startyear = 2015, endyear = 2022)
```

### International CRS data

Read international ODA data into R, returning a tibble data frame. It is
over 5 million rows. This can take a while, but should be less than a
minute.

``` r

df_crs <- read_international_crs()

df_crs |> 
  filter(year == max(year),
         category == 10) |> 
  summarise(usd_disbursement = sum(usd_disbursement, na.rm = T), .by = donor_name) |> 
  slice_max(usd_disbursement, n = 10)
```

Instead of loading all the data of international CRS data into the R
environment (in-memory), we can use the
[`access_international_crs()`](https://noradno.github.io/noradstats/reference/access_international_crs.md)
function to create a proxy data frame linked to the DuckDB database
table containing the international CRS data.

As mentioned above, the proxy data frame can be manipulated using
`dplyr` operations (through `duckplyr` translation to SQL queries),
executed directly on the database. Use
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) to
return the query results in the R environment. This approach is more
efficient for handling large datasets by reducing memory usage and
faster data processing than loading all data into memory.

In this example we find the topten ODA donors (bilateral donors or
multilateral donors) in the most recent year. This is much faster than
in the example above where we started by importing all the 5 million
rows.

``` r

df_proxy_crs <- access_international_crs()

df_proxy_crs |> 
  filter(year == max(year),
         category == 10) |> 
  summarise(usd_disbursement = sum(usd_disbursement, na.rm = T), .by = donor_name) |> 
  slice_max(usd_disbursement, n = 10) |> 
  collect()
```

### International ODA data from DAC-countries to countries and regions

International ODA data from DAC-member countries to recipient countries
and regions. Provide start and end years as arguments.

``` r

df_dac <- read_donors(startyear = 2013, endyear = 2022)
```

### Create files for bistandsresultater.no

The `noradstats` package includes functions for preparing the main data
files used in data loads to bistandsresultater.no.

The workflow is:

**PTA / Statsys → CSV → DuckDB → `noradstats` → processed and anonymised
Excel files**

Raw extracts from PTA and Statsys are first loaded into DuckDB using:

``` r

create_pta_agreement_totals_to_db()
create_pta_disbursement_level_to_db()
create_statsys_data_to_db()
```

The final files are then created with:

``` r

filepath <- "path/to/where/you/want/the/files"

export_aidresults_xlsx(
  path_dir = filepath,
  lang = "en"
)
```

This creates the three main files:

- `pta_agreements_total.xlsx`
- `pta_disbursement_level_BCD_actual.xlsx`
- `statsys_aktiv.xlsx`

The export function applies the required processing and anonymisation
rules.

See the vignette **Preparing a data load for bistandsresultater.no** for
the full procedure, including how to extract the source data from PTA
and Statsys and load it into DuckDB.
