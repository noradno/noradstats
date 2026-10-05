# Official column name mapping (internal -\> official)

Returns the mapping between internal (snake_case) column names and
official (public) labels for a given dataset and language.

## Usage

``` r
official_colname_map(dataset, lang = c("en", "no"))
```

## Arguments

- dataset:

  Dataset key (character) identifying which official naming scheme to
  use. Must match a value in the `dataset` column of
  `inst/extdata/official_colnames.csv`.

- lang:

  Language for official labels. One of `"en"` or `"no"`.

## Value

A tibble with columns `dataset`, `internal`, and `official`.

## Details

Valid dataset keys currently include:

- `"oda"` – Official development assistance (ODA), disbursement-level
  data

- `"pta_disbursements"` – PTA disbursement-level data

- `"pta_agreement_totals"` – PTA agreement-level totals

The full list of valid dataset keys is defined in
`inst/extdata/official_colnames.csv`.
