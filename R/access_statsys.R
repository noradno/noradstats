#' Access database of Statsys data from R
#'
#' This function creates a proxy tibble connected to the Statsys data table in the DuckDB database.
#' This data includes Norwegian official development assistance (ODA), ODA frame agreement level data, Other official flows(OOF), export credits and private flows.
#' The data covers 1960 to recent year.
#' The DuckDB database file is located on Norads Microsoft Sharepoint site and is expected to be synced via Microsoft Teams to to the users local directory.
#' Use DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.
#'
#' @return Returns a proxy tibble connected to a Statsys table in the DuckDB database.
#' @export
#' @examples
#' ?access_statsys()
#'

access_statsys <- function() {
  
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
  
  # Create a proxy tibble connected to the Statsys table in the DuckDB database.
  df_remote_statsys <- dplyr::tbl(con, "statsys_official")
  
  return(df_remote_statsys)
}
