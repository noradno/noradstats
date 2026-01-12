# noradstats

## Overview

Easily import Norwegian and international development finance data into
R using the `read_*()` functions or `access_*()` functions. Add extra
columns using the `add_cols*()`. The data is stored in databases. Some
functions use user-specified CSV files as data source.

- Use the `read_*()` functions for importing all existing data from a
  specific database table or CSV into the R environment.

  - `read_oda()` to import all Norwegian ODA data from the database
  - `read_statsys()` to import all Norwegian ODA, OOF and PF data from
    the database
  - `read_international_crs()` to import all international CRS data from
    the database
  - `read_donors()` to import DAC donors ODA by recipient countries and
    regions from an OECD database
  - `read_imputed_countries()` to import Norwegian imputed multilateral
    ODA by recipient countries and regions from the OECD database
  - `read_imputed_sectors()` to import Norwegian imputed multilateral
    ODA by sectors from the OECD database
  - `read_statsys_from_csv()` to import all Norwegian ODA, OOF and PF
    data from a user-specified CSV
  - `read_aidresults_from_csv()` to import data from a user-specified
    CSV downloaded from aidresults.no

- Use the `access_*()` functions for creating a proxy data frame linked
  to a specific database table. This allows for using dplyr operations
  for querying directly on the database before retrieving the resulting
  data into memory.

  - `access_oda()` to access Norwegian ODA data in the database
  - `access_statsys()` to access Norwegian ODA, OOF and PF data in the
    database
  - `access_international_crs()` to access all international CRS data in
    the database

- [Under development] Use the `add_cols_*` functions to add extra
  columns to existing data produced by `read_oda()`, `read_statsys()`,
  `read_statsys_from_csv()` and the `access_*()` equivalents.

  - `add_cols_basic()` adds basic classification columns
  - `add_cols_climate()` adds climate relevant columns
  - `add_cols_climate_clean()` adds cleaned climate relevant columns
  - `add_cols_countrycode()` adds ISO3 country codes
  - `add_cols_violence()` adds conflict relevant columns using UCDP data

## Installation

``` r
remotes::install_github("noradno/noradstats")
```

## Usage

### Prerequisites

Several of the functions in the noradstats package connect to a DuckDB
database file. The DuckDB database file is located on Norad’s Microsoft
SharePoint site and is expected to be synced via Microsoft Teams to the
user’s local directory:

`C:/Users/[userid]/Norad/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/3. Databasefiler/`

The database is accessed as a **local file on disk**.

### Loading packages

In addition to `noradstats`, the examples below use `dplyr`, `DBI` and
`duckplyr`. `duckplyr` translates dplyr verbs to SQL queries optimized
for DuckDB.

``` r
library(noradstats)
library(dplyr)
library(DBI)
library(duckplyr)
```

### Norwegian ODA data

#### Read Norwegian ODA data to a tibble

The function `read_oda()` imports all Norwegian ODA data from the DuckDB
database into R.

``` r
df_oda <- read_oda()

df_oda |>
  filter(year == max(year)) |>
  summarise(disbursed_mill_nok = sum(disbursed_mill_nok, na.rm = TRUE),
            .by = target_area) |>
  arrange(desc(disbursed_mill_nok))
```

#### Access database of Norwegian ODA

Instead of loading all data into memory, use `access_*()` functions to
query the DuckDB database directly and collect results when needed.

``` r
df_proxy_oda <- access_oda()

df_proxy_oda |>
  filter(year == max(year)) |>
  summarise(disbursed_mill_nok = sum(disbursed_mill_nok, na.rm = TRUE),
            .by = target_area) |>
  arrange(desc(disbursed_mill_nok)) |>
  collect()
```

### Norwegian imputed multilateral data

``` r
df_imputed_countries <- read_imputed_countries(startyear = 2020, endyear = 2022)
df_imputed_sectors   <- read_imputed_sectors(startyear = 2015, endyear = 2022)
```

### International CRS data

``` r
df_crs <- read_international_crs()

df_crs |>
  filter(year == max(year), category == 10) |>
  summarise(usd_disbursement = sum(usd_disbursement, na.rm = TRUE),
            .by = donor_name) |>
  slice_max(usd_disbursement, n = 10)
```

``` r
df_proxy_crs <- access_international_crs()

df_proxy_crs |>
  filter(year == max(year), category == 10) |>
  summarise(usd_disbursement = sum(usd_disbursement, na.rm = TRUE),
            .by = donor_name) |>
  slice_max(usd_disbursement, n = 10) |>
  collect()
```

### International ODA data from DAC countries

``` r
df_dac <- read_donors(startyear = 2013, endyear = 2022)
```

## SharePoint-based results datasets

In addition to reading existing datasets, `noradstats` can **build and
store results datasets** based on data from Norad’s SharePoint Lists
(e.g. agreement and portfolio assessments).

These datasets are created by:
- reading SharePoint Lists via the Microsoft Graph API
- transforming them into analysis-ready tables
- writing the results to the shared DuckDB database

### Creating results tables

``` r
create_sharepoint_results_to_db()
```

This function:
- reads multiple SharePoint Lists in one pipeline
- writes each result as a separate DuckDB table
- requires authenticated access to SharePoint (handled by `Microsoft365R`)

SharePoint access is only required when **building** the results tables.

### Reading results datasets

Once created, all SharePoint-based results tables can be read from
DuckDB using:

``` r
results <- read_sharepoint_results()
names(results)
```

This returns a named list of tibbles, for example:

``` r
results$agr_assessments
results$portfolio_analysis
```

Reading results from DuckDB does **not** require SharePoint access.
