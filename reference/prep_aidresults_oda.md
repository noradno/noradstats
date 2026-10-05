# Prepare ODA dataset for aidresults

Builds the ODA (historical, disbursement-level) dataset used by
aidresults. Applies anonymisation mappings, selects the publication
schema, and renames columns to official English names.

## Usage

``` r
prep_aidresults_oda(maps = NULL, lang = "en")
```

## Arguments

- maps:

  Optional. Output from \[build_anonymization_maps()\]. If not provided,
  maps will be built from required source datasets internally.

- lang:

  Language for official column names. Defaults to "en".

## Value

A tibble ready for publication.
