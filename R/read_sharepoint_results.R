#' Read SharePoint results datasets from DuckDB
#'
#' Reads the SharePoint-based "results" tables from DuckDB and returns them as a named list.
#'
#' @param db_path Path to DuckDB database. Defaults to [get_duckdb_path()].
#' @param datasets Character vector of dataset names to read (e.g. "agr_assessments").
#'   Default NULL reads all datasets defined by internal [results_datasets()].
#'
#' @return A named list of tibbles.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- read_sharepoint_results()
#' names(results)
#' head(results$agr_assessments)
#' }
read_sharepoint_results <- function(
    db_path = get_duckdb_path(),
    datasets = NULL
) {
  stop_if_missing_db(db_path)
  
  reg <- results_datasets()
  all_names <- names(reg)
  
  if (is.null(datasets)) {
    datasets <- all_names
  }
  
  datasets <- as.character(datasets)
  
  missing <- setdiff(datasets, all_names)
  if (length(missing) > 0) {
    stop(
      "Unknown dataset name(s): ",
      paste(missing, collapse = ", "),
      ". Valid names are: ",
      paste(all_names, collapse = ", "),
      call. = FALSE
    )
  }
  
  setNames(
    purrr::map(
      datasets,
      function(nm) {
        read_db_table_as_tibble(db_path, reg[[nm]]$table)
      }
    ),
    datasets
  )
}
