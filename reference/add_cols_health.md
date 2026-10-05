# Add health columns to an existing oda data frame

This function takes an existing oda data frame as input and adds health
columns. The function returns the oda data frame with the following
additional columns for health:

- `health_nok`:

  Numeric variable of total disbursed health oda (earmarked and imputed
  multilateral).

- `health_tag`:

  Logical variable to identify health activities, meaning earmarked ODA
  for DAC sectors of long term and emergency aid and imputed
  multilateral ODA for health (positive imputed multilateral health
  shares)

- `health_oda_channel`:

  Categorical variable to separate earmarked oda to health from imputed
  multilateral oda to health.

- `health_oda_channel2`:

  Categorical variable to separate earmarked humanitarian oda to health,
  long term oda to health, and imputed multilateral to health.

## Usage

``` r
add_cols_health(df_oda)
```

## Arguments

- df_oda:

  A oda data frame, which must already be loaded into the environment.

## Value

A oda data frame with additional health columns.

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

# Add health column to the df_oda data
df_oda_health <- add_cols_health(df_oda)
#> Error: object 'df_oda' not found

# Ensure that the df_oda data frame is available before running this function,
# using the `read_oda()` function
```
