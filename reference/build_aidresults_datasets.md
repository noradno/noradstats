# Build all aidresults datasets

Builds all datasets used by aidresults and returns them as a named list.
Column names are returned in English by default.

## Usage

``` r
build_aidresults_datasets(lang = "en")
```

## Arguments

- lang:

  Language for official column names. Defaults to "en".

## Value

A named list with tibbles for each dataset.
