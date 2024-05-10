#' Read Statsys data into R
#'
#' This function connects to a specified DuckDB database table to retrieve Statsys data. This includes official development assistance (ODA), frame agreement level data, Other official flows(OOF), export credits and private flows.
#' The data is available from 1960 to the recent year.
#' The database file is expected to be located in a synchronized SharePoint directory on the user's local computer.
#' The function returns all the data in the database table as a tibble data frame.
#'
#' @return A tibble containing Norwegian ODA data.
#' @export
#' @examples
#' ?read_statsys()
#'

read_statsys <- function() {
  
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
  df_statsys <- con |>
    DBI::dbReadTable("statsys") |>
    tibble::as_tibble()
  
  # Disconnect database
  DBI::dbDisconnect(con, shutdown = TRUE)
  
  return(df_statsys)
}
