# Prepare PTA disbursement-level dataset for aidresults

Builds the PTA disbursement-level dataset (current year and forward).
Applies anonymisation mappings and formats the dataset for publication.

## Usage

``` r
prep_aidresults_pta_disbursements(maps = NULL, lang = "en")
```

## Arguments

- maps:

  Optional. Output from \[build_anonymization_maps()\]. If not provided,
  maps will be built from required source datasets internally.

- lang:

  Language for official column names. Defaults to "en".

## Value

A tibble ready for publication.
