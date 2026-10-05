# Rename columns to official (public) labels

Renames internal snake_case column names to official public labels using
a dataset-specific mapping.

## Usage

``` r
rename_to_official(x, dataset, lang = c("en", "no"))
```

## Arguments

- x:

  A data frame or tibble.

- dataset:

  Dataset key identifying which official naming scheme to use.

- lang:

  Language for official labels. One of `"en"` or `"no"`.

## Value

`x` with renamed columns.

## Details

Valid dataset keys currently include:

- `"oda"`

- `"pta_disbursements"`

- `"pta_agreement_totals"`

The full list of valid dataset keys is defined in
`inst/extdata/official_colnames.csv`.

Only columns with an official label for the selected dataset and
language are renamed. All other columns are left unchanged (e.g. derived
columns).
