#' Access database of Norwegian ODA data from R
#'
#' This function connects to a specified DuckDB database table to retrieve Norwegian Official Development (ODA) data from 1960 to the recent year.
#' The database file is expected to be located in a synchronized SharePoint directory on the user's local computer.
#' Use DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.
#'
#' @return Returns a remote tibble data frame linked to the Statsys database table.
#' @export
#' @examples
#' ?access_oda()
#'

access_oda <- function() {
  
  # Specify user specific path to database file
  docs <- path.expand("~")
  
  if (Sys.info()["sysname"] == "Windows") {
    home <- dirname(docs)
  } else if (Sys.info()["sysname"] == "Darwin") {
    home <- docs
  }
  
  default_path_end <- "/Norad/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/3. Databasefiler/statsys.duckdb"
  default_path <- paste0(home, default_path_end)
  
  # Connect to the database table
  con <- DBI::dbConnect(duckdb::duckdb(), default_path)
  
  # Create a remote tibble linked to the crs database table. This allows for using dplyr syntax in database queries
  df_remote_oda <- dplyr::tbl(con, "oda")
  
  return(df_remote_oda)
}