#' Create SharePoint results datasets and write them to DuckDB
#'
#' This function orchestrates the SharePoint results pipeline:
#'
#' \itemize{
#'   \item Reads the results registry defined in \code{results_datasets()},
#'         which declares which datasets exist, their target table names,
#'         and their associated builder functions.
#'   \item Executes the corresponding \code{make_*()} builder functions
#'         (defined in \code{results_make.R}) to transform raw SharePoint
#'         Lists into analysis-ready tibbles.
#'   \item Writes each resulting dataset to DuckDB as a separate table.
#' }
#'
#' The expected schema of each results table is documented in
#' \code{results_datasets.R}.
#'
#' @param db_path Path to DuckDB database. Defaults to [get_duckdb_path()].
#' @param overwrite Logical. Overwrite existing tables in DuckDB.
#' @param quiet Logical. If TRUE, reduces messages.
#'
#' @return (Invisibly) a tibble with one row per dataset written (name, table, nrow).
#' @export
#'
#' @examples
#' \dontrun{
#' create_sharepoint_results_to_db()
#' }
create_sharepoint_results_to_db <- function(
    db_path = get_duckdb_path(),
    overwrite = TRUE,
    quiet = FALSE
) {
  stop_if_missing_db(db_path)
  
  # Early dependency check (SharePoint access)
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop(
      "Package 'Microsoft365R' is required to build SharePoint results.\n",
      "Install it to run create_sharepoint_results_to_db().",
      call. = FALSE
    )
  }
  
  datasets <- results_datasets()
  
  # Basic validation of registry
  bad <- purrr::map_lgl(datasets, ~ is.null(.x$table) || is.null(.x$builder))
  if (any(bad)) {
    stop("Internal error: results_datasets() entries must contain `table` and `builder`.", call. = FALSE)
  }
  
  res <- with_duckdb(db_path, function(con) {
    
    # NOTE: value first (spec), then name (nm)
    write_one <- function(spec, nm) {
      if (!quiet) message("Building: ", nm)
      df <- spec$builder()
      
      if (!quiet) message("Writing table: ", spec$table)
      DBI::dbWriteTable(con, spec$table, df, overwrite = overwrite)
      
      tibble::tibble(
        name  = nm,
        table = spec$table,
        nrow  = nrow(df)
      )
    }
    
    purrr::imap(datasets, write_one) |>
      purrr::list_rbind()
  })
  
  if (!quiet) message("🎉 Success! SharePoint results written to DuckDB.")
  
  invisible(res)
}
