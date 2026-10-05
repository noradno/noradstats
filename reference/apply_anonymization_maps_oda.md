# Apply anonymisation mappings to ODA disbursement-level data

Applies partner/impl mappings and masks title/description for flagged
agreements.

## Usage

``` r
apply_anonymization_maps_oda(df_oda_disbursements, maps)
```

## Arguments

- df_oda_disbursements:

  ODA disbursement-level data.

- maps:

  Output from \[build_anonymization_maps()\].

## Value

An anonymised ODA tibble.
