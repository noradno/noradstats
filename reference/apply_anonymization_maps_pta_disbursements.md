# Apply anonymisation mappings to PTA disbursement-level data

Applies partner mapping (ODA + PTA-new) and impl mapping (ODA-derived),
and masks title/description for flagged agreements.

## Usage

``` r
apply_anonymization_maps_pta_disbursements(df_pta_disbursements, maps)
```

## Arguments

- df_pta_disbursements:

  PTA disbursement-level data.

- maps:

  Output from \[build_anonymization_maps()\].

## Value

An anonymised PTA disbursement-level tibble.
