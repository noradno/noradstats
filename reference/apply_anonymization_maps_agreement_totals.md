# Apply anonymisation mappings to PTA agreement totals data

Agreement totals lacks recipient country and partner groups, so masking
is applied only by agreement_no using the derived mapping tables.

## Usage

``` r
apply_anonymization_maps_agreement_totals(df_pta_agreement_totals, maps)
```

## Arguments

- df_pta_agreement_totals:

  PTA agreement totals data.

- maps:

  Output from \[build_anonymization_maps()\].

## Value

An anonymised agreement totals tibble.
