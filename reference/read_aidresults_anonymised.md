# Read anonymised datasets for aid results publication

Reads the relevant datasets from DuckDB via noradstats readers and
applies a consistent anonymisation approach across sources: - ODA
disbursement-level data (historical, 1960..last year) - PTA
disbursement-level data (current year and forward) - PTA agreement
totals (agreement-level, 1960..today)

## Usage

``` r
read_aidresults_anonymised()
```

## Value

A named list with three tibbles:

- df_oda_disbursements:

  Anonymised ODA disbursement-level data.

- df_pta_disbursements:

  Anonymised PTA disbursement-level data.

- df_pta_agreement_totals:

  Anonymised PTA agreement totals data.
