# Add humanitarian (hum) columns to an existing oda data frame

This function takes an existing oda data frame as input and adds hum
columns. The function returns the oda data frame with the following
additional columns for hum:

- `hum_nok`:

  Numeric variable of total disbursed hum oda (earmarked and imputed
  multilateral).

- `hum_tag`:

  Logical variable to identify hum activities, meaning earmarked ODA for
  DAC sectors and imputed multilateral ODA for hum (positive imputed
  multilateral hum shares)

- `hum_oda_channel`:

  Categorical variable to separate earmarked oda to hum from imputed
  multilateral oda to hum.

## Usage

``` r
add_cols_hum(df_oda)
```

## Arguments

- df_oda:

  A oda data frame, which must already be loaded into the environment.

## Value

A oda data frame with additional hum columns.

## Details

\## Important: Before running this function, you must have already
loaded the oda data frame by using
[`noradstats::read_oda()`](https://noradno.github.io/noradstats/reference/read_oda.md).

## Examples

``` r
# Load the oda data
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

# Add hum columns to the df_oda data
df_oda_hum <- add_cols_hum(df_oda)
#> Error: object 'df_oda' not found

# Ensure that the df_oda data frame is available before running this function,
# using the `read_oda()` function
```
