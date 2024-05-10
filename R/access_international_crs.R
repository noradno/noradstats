#' Access database of international CRS data from R
#'
#' This function connects to a specified DuckDB database table to retrieve CRS (Creditor Reporting System) activity level data of 
#' Official Development Finance (ODA/OOF) from official donors (DAC, non-DAC, multilaterals) and private philantropy, starting from 1960 to the recent year. 
#' It returns a remote tibble linked to the crs database file, and allows for using dplyr syntax in database queries.
#' The database file is expected to be located in a synchronized SharePoint directory on the user's local computer.
#' Use DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.
#' Data source: https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1
#'
#' @return Returns a remote tibble data frame linked to the international CRS database table.
#' @export
#' @examples
#' ?access_crs()
#'

access_international_crs <- function() {
  
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
  df_remote_crs <- dplyr::tbl(con, "crs")
  
  return(df_remote_crs)
}
