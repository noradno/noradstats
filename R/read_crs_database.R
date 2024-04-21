#' Read international activity level CRS data (1973-). Create a remote tibble connected to the crs table in the DuckDB database file stored in Sharepoint
#'
#' @return Returns a remote table (tibble) linked to the 'crs' table in the DuckDB database. Use dplyr syntax to collect data from the remote tibble and to disconnect when done, using DBI::dbDisconnect(con, shutdown=TRUE)
#'
#' @export
#' @examples
#' ?read_crs_database()

read_crs_database <- function() {
  # Find user-specific path to database file --------------------------------
  
  # Find the user id to input in the path
  docs <- path.expand("~")
  
  # Check if system is Windows
  if (Sys.info()["sysname"] == "Windows") {
    home <- dirname(docs)
  } else if (Sys.info()["sysname"] == "Darwin") {
    home <- docs
  }
  
  # Path to duckdb database file
  default_path_end <- "/Norad/Norad-Avd-Kunnskap - Statistikk og analyse/13. Annen data/CRS bulk files/crs_database.duckdb"
  default_path <- paste0(home, default_path_end)
  
  # Connecting to database and create a remote tibble linked to crs --------
  
  # Establish a connection to a DuckDB database file
  con <- DBI::dbConnect(duckdb::duckdb(), default_path)
  
  # List all tables in the connected DuckDB database to verify existing tables
  DBI::dbListTables(con)
  
  # Create a remote tibble linked to the 'crs' table in the DuckDB database
  # This allows for using dplyr syntax on database queries
  df_crs_remote <- dplyr::tbl(con, "crs")
  
  # Return remote tibble
  return(df_crs_remote)
  
}
