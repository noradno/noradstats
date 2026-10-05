# Read SharePoint results datasets from DuckDB

Reads the SharePoint-based "results" tables from DuckDB and returns them
as a named list.

## Usage

``` r
read_sharepoint_results(db_path = get_duckdb_path(), datasets = NULL)
```

## Arguments

- db_path:

  Path to DuckDB database. Defaults to \[get_duckdb_path()\].

- datasets:

  Character vector of dataset names to read (e.g. "agr_assessments").
  Default NULL reads all datasets defined by internal
  \[results_datasets()\].

## Value

A named list of tibbles.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- read_sharepoint_results()
names(results)
head(results$agr_assessments)
} # }
```
