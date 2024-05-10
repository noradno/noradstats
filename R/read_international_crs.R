#' Read international CRS data into R
#'
#' This function connects to a specified DuckDB database table to retrieve CRS (Creditor Reporting System) activity level data of 
#' Official Development Finance (ODA/OOF) from official donors (DAC, non-DAC, multilaterals) and private philantropy, starting from 1960 to the recent year. 
#' It returns all the data in the database table as a tibble. The database file is expected to be located in a synchronized SharePoint directory on the user's local computer.
#' Data source: https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1
#'
#' @return A tibble containing international CRS data.
#' @export
#' @examples
#' ?read_oda()
#'

read_international_crs <- function() {
  
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
  
  # Return oda database table as tibble
  df_crs <- con |>
    DBI::dbReadTable("crs") |>
    tibble::as_tibble()
  
  # Disconnect database
  DBI::dbDisconnect(con, shutdown = TRUE)
  
  return(df_crs)
}
