# Add public climate finance columns to an existing statsys data frame

This function takes an existing statsys data frame as input and adds
public climate finance columns using the UNFCCC methodology, including
both Official Development Finance (ODA and OOF). Earmarked climate
finance is calculated using the Rio Markers for Climate Change
adaptation and mitigation, applying a 40 percent coefficient for
activities with only a significant climate change objective(s). The
Riomarked capitalisation(s) of Norfund Climate Investment
Mandate/Climate Investment Fund is excluded to avoiding double counting,
as the CIF investments are already included (OOF). Imputed multilateral
climate finance is calculated using the OECD imputed multilateral
shares, along with Norad's temporary estimates for the most recent
year(s). All amounts are expressed in gross disbursements. Note: Climate
finance to all developing countries is included, including countries
that are not non-Annex 1 parties (such as Ukraine), and should therefore
be excluded when reporting to the UNFCCC. The function returns the
statsys data frame with the following additional columns for climate
finance:

- `climate_finance_nok`:

  Numeric variable of total public climate finance (earmarked and
  imputed multilateral).

- `climate_finance_tag`:

  Logical variable to identify climate finance activities, mening
  Rio-marked activites (regardless of amounts extended) and positive
  climate shares.

- `climate_finance_channel`:

  Categorical variable to separate earmarked from imputed multilateral
  climate finance.

- `climate_finance_channel2`:

  Categorical variable to separate earmarked (ex. Norfund/KIF),
  Norfund/KIF, and imputed multilateral climate finance.

- `climate_finance_type_of_support_3levels`:

  Categorical variable of type of support (UNFCCC levels): adaptation
  only, mitigation only, and cross-cutting. Only useful for
  `climate_finance_nok`, not for
  `climate_adaptation_finance_earmarked_nok` and
  `climate_mitigation_finance_earmarked_nok`.

- `climate_adaptation_finance_earmarked_nok`:

  Numeric variable of earmarked climate adaptation finance. Note that
  many of the activities may also be cross-cutting, also aimed at
  climate change mitigation.

- `climate_mitigation_finance_earmarked_nok`:

  Numeric variable of earmarked climate mitigation finance. Note that
  many of the activities may also be cross-cutting, also aimed at
  climate change adaptation.

## Usage

``` r
add_cols_climate_finance(df_statsys)
```

## Arguments

- df_statsys:

  A statsys data frame, which must already be loaded into the
  environment.

## Value

A statsys data frame with an additional \`climate_finance_nok\` column.

## Details

\## Important: Before running this function, you must have already
loaded the statsys data frame by using
[`noradstats::read_statsys()`](https://noradno.github.io/noradstats/reference/read_statsys.md).

## Examples

``` r
# Load the statsys data
df_statsys <- read_statsys()
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpIrjQUj/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Error in dbConnect(dbdir, read_only, bigint, config): IO Error: Cannot open file "/home/runner/work/noradstats/noradstats/docs/reference/C:/Users/home/runner/Norad/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/3. Databasefiler/statsys.duckdb": No such file or directory
#> ℹ Context: rapi_startup
#> ℹ Error type: IO

# Add the climate_finance_nok column to the df_statsys data
df_statsys_climate_finance <- add_cols_climate_finance(df_statsys)
#> Error: object 'df_statsys' not found

# Ensure that the df_statsys data frame is available before running this function,
# using the `read_statsys()` function
```
