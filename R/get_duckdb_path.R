#' Get the local path to the DuckDB database file
#'
#' Returns the user-specific file path to the shared DuckDB database.
#' The database file is stored in a SharePoint-synced folder, but is accessed
#' purely as a local file on disk (no network or API access).
#'
#' This function is used by all read_*() and create_*() functions that interact
#' with the DuckDB database.
#'
#' @return A character string containing the full local path to the DuckDB database file.
#' @export
get_duckdb_path <- function() {
  
  # Root of user home directories on Windows
  db_path_start <- "C:/Users"
  
  # Extract Norad user ID from home directory (e.g. aaw123)
  db_user <- sub(".*(aaw[a-zA-Z0-9]{3}).*", "\\1", path.expand("~"))
  
  # Relative path to the DuckDB file inside the SharePoint-synced folder
  db_path_end <- "Norad/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/3. Databasefiler/statsys.duckdb"
  
  # Construct full local file path
  file.path(db_path_start, db_user, db_path_end)
}
