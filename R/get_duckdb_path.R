#' Get the user-specific path to the DuckDB database
#'
#' This function determines the user-specific file path to the DuckDB database 
#' based on the operating system and user home directory.
#'
#' @return A string containing the full path to the DuckDB database.
#' @export
get_duckdb_path <- function() {
  
  # The path start
  db_path_start <- "C:/Users"

  # The user specific path
  db_user <- sub(".*(aaw[a-zA-Z0-9]{3}).*", "\\1", path.expand("~"))

  # The path end
  db_path_end <- "Norad/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/3. Databasefiler/statsys.duckdb"
  
  # Combine the home directory with the user-specific path
  file.path(db_path_start, db_user, db_path_end)
}
