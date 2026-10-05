# Build anonymisation mappings for aid results publication

Derives which agreements must be anonymised, and how, based on: - ODA
disbursements (full context: recipient country + partner/impl groups) -
PTA disbursements (adds decisions for new agreements not present in ODA)

## Usage

``` r
build_anonymization_maps(df_oda_disbursements, df_pta_disbursements, rules)
```

## Arguments

- df_oda_disbursements:

  ODA disbursement-level data.

- df_pta_disbursements:

  PTA disbursement-level data.

- rules:

  Output from \[anonymization_rules()\].

## Value

A named list containing:

- anonymized_text:

  Masking label.

- df_partner_map:

  Mapping agreement_no -\> partner_masked.

- df_impl_map:

  Mapping agreement_no -\> impl_masked (ODA-derived).

- vec_mask_title_desc:

  Agreement numbers requiring masked title/description.
