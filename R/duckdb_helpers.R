# Internal DuckDB helpers (not exported)
#
# These helpers provide a small, consistent interface for working with
# the shared DuckDB database file used throughout noradstats.
#
# Design principles:
# - Fail early with clear errors if the database path is invalid
# - Ensure database connections are always closed
# - Keep DuckDB interaction logic in one place

# Validate that the DuckDB database path exists and is usable
stop_if_missing_db <- function(db_path) {
  if (!is.character(db_path) || length(db_path) != 1 || !nzchar(db_path)) {
    stop("`db_path` must be a non-empty string.", call. = FALSE)
  }
  if (!file.exists(db_path)) {
    stop(
      "The DuckDB database file does not exist or is inaccessible: ",
      db_path,
      call. = FALSE
    )
  }
}

# Execute a function with an open DuckDB connection
#
# This helper:
# - opens a connection to the DuckDB file
# - passes the connection to `fn(con)`
# - guarantees that the connection is closed afterwards
#
# Used by both read_*() and create_*() functions to avoid repeating
# connection management logic.
with_duckdb <- function(db_path, fn) {
  stop_if_missing_db(db_path)
  
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  fn(con)
}

# Read a full DuckDB table into R as a tibble
#
# This is a thin convenience wrapper around DBI::dbReadTable(),
# used to standardise how tables are read across the package.
read_db_table_as_tibble <- function(db_path, table_name) {
  with_duckdb(db_path, function(con) {
    DBI::dbReadTable(con, table_name) |>
      tibble::as_tibble()
  })
}
