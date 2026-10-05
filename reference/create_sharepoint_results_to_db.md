# Create SharePoint results datasets and write them to DuckDB

This function orchestrates the SharePoint results pipeline:

## Usage

``` r
create_sharepoint_results_to_db(
  db_path = get_duckdb_path(),
  overwrite = TRUE,
  quiet = FALSE
)
```

## Arguments

- db_path:

  Path to DuckDB database. Defaults to \[get_duckdb_path()\].

- overwrite:

  Logical. Overwrite existing tables in DuckDB.

- quiet:

  Logical. If TRUE, reduces messages.

## Value

(Invisibly) a tibble with one row per dataset written (name, table,
nrow).

## Details

- Reads the results registry defined in `results_datasets()`, which
  declares which datasets exist, their target table names, and their
  associated builder functions.

- Executes the corresponding `make_*()` builder functions (defined in
  `results_make.R`) to transform raw SharePoint Lists into
  analysis-ready tibbles.

- Writes each resulting dataset to DuckDB as a separate table.

The expected schema of each results table is documented in
`results_datasets.R`.

## Examples

``` r
if (FALSE) { # \dontrun{
create_sharepoint_results_to_db()
} # }
```
