#' Access database of international CRS data from R
#'
#' This function creates a proxy tibble connected to the CRS table in the DuckDB database.
#' This data includes CRS (Creditor Reporting System) activity level data of Official Development Finance (ODA/OOF) from official donors (DAC, non-DAC, multilaterals) and private philantropy, starting from 1973 to the recent year.
#' The DuckDB database file is located on Norads Microsoft Sharepoint site and is expected to be synced via Microsoft Teams to to the users local directory.
#' Use DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.
#' Data source: https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1
#'
#' @return Returns a proxy tibble connected to the CRS table in the DuckDB database.
#' @export
#' @examples
#' ?access_crs()
#'

access_international_crs <- function() {
  
  # Specify user specific path to the DuckDB database file
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
  
  # Create a procy tibble linked to the crs database table.
  df_remote_crs <- dplyr::tbl(con, "crs")
  
  return(df_remote_crs)
}
