#' Get the user-specific path to the DuckDB database
#'
#' This function determines the user-specific file path to the DuckDB database 
#' based on the operating system and user home directory.
#'
#' @return A string containing the full path to the DuckDB database.
#' @export
get_duckdb_path <- function() {
  
  # Specify user-specific path to database file
  docs <- path.expand("~")  # Expands to home directory on both Windows and macOS
  
  # Detect the operating system and adjust the path accordingly
  db_path_start <- switch(Sys.info()["sysname"],
                 "Windows" = dirname(docs),  # Windows: home is the parent of "Documents"
                 "Darwin" = docs,            # macOS: home is the same as "docs"
                 docs)                       # Default for other systems

  # Specify the relative path to the database file
  db_path_end <- "Norad/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/3. Databasefiler/statsys.duckdb"
  
  # Combine the home directory with the user-specific path
  file.path(db_path_start, db_path_end)
}
