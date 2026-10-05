# Prepare PTA agreement totals dataset for aidresults

Builds the PTA agreement-level totals dataset (historical and current).
Applies anonymisation mappings and formats the dataset for publication.

## Usage

``` r
prep_aidresults_pta_agreement_totals(maps = NULL, lang = "en")
```

## Arguments

- maps:

  Optional. Output from \[build_anonymization_maps()\]. If not provided,
  maps will be built from required source datasets internally.

- lang:

  Language for official column names. Defaults to "en".

## Value

A tibble ready for publication.
