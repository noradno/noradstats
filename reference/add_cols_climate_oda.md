# Add public climate oda columns to an existing statsys data frame

This function takes an existing statsys data frame as input and adds
public climate ODA columns. Earmarked climate ODA is calculated using
the Rio Markers for Climate Change adaptation and mitigation, applying a
40 percent coefficient for activities with only a significant climate
change objective(s). Imputed multilateral climate ODA is calculated
using the OECD imputed multilateral shares, along with Norad's temporary
estimates for the most recent year(s). Imputed ordinary Norfund climate
ODA (2 year averages) is calculated by Norad using the imputed
multilateral shares methodology. All amounts are expressed in net
disbursements. The function returns the statsys data frame with the
following additional columns for climate ODA:

- `climate_oda_nok`:

  Numeric variable of total public climate ODA (earmarked incl imputed
  ordinary Norfund and imputed multilateral).

- `climate_oda_tag`:

  Logical variable to identify climate ODA activities, mening Rio-marked
  activites (regardless of disbursements) and positive imputed
  multilateral or imputed ordinary Norfund climate shares.

- `climate_oda_channel`:

  Categorical variable to separate earmarked from imputed multilateral
  climate ODA

- `climate_oda_channel2`:

  Categorical variable to separate earmarked (ex. capitalisation of
  ordinary Norfund), imputed ordinary Norfund ODA, and imputed
  multilateral ODA

- `climate_oda_type_of_support_3levels`:

  Categorical variable of type of support (UNFCCC levels): adaptation
  only, mitigation only, and cross-cutting. Only useful for
  `climate_oda_nok`, not for `climate_adaptation_oda_earmarked_nok` and
  `climate_mitigation_oda_earmarked_nok`.

- `climate_oda_finance_earmarked_nok`:

  Numeric variable of earmarked ODA for climate adaptation. Note that
  many of the activities may also be cross-cutting, also aimed at
  climate change mitigation.

- `climate_oda_finance_earmarked_nok`:

  Numeric variable of earmarked ODA for climate mitigation, including
  imputed ordinary Norfund. Note that many of the activities may also be
  cross-cutting, also aimed at climate change adaptation.

## Usage

``` r
add_cols_climate_oda(df_oda)
```

## Arguments

- df_statsys:

  An ODA data frame, which must already be loaded into the environment.

## Value

An ODA data frame with an additional \`climate_oda_nok\` column.

## Details

\## Important: Before running this function, you must have already
loaded the ODA data frame by using
[`noradstats::read_oda()`](https://noradno.github.io/noradstats/reference/read_oda.md).

## Examples

``` r
# Load the ODA data
df_oda <- read_oda()
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

# Add the climate_oda_nok columns to the df_oda data frame
df_oda_climate_oda <- add_cols_climate_oda(df_oda)
#> Error: object 'df_oda' not found

# Ensure that the df_oda data frame is available before running this function,
# using the `read_oda()` function
```
