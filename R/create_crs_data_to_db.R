#' Function to save and overwrite CRS data to DuckDB Database
#' 
#' Before running this function, load CRS data into a data frame in the R Environment:
#' 1) Download CRS-parquet.parquet file from the OECD database available on CRS: Creditor Reporting System (flows) [cloud replica].
#' 2) Read the CRS-parquet.parquet file into R Environment using nanoparquet::read_parquet()
#' 
#' @param df_crs Name of the crs data frame in the R environment to be saved to the DuckDB database.
#'
#' @details 
#' The function checks if both the crs input data frame exists and then saves the data frame to
#' the DuckDB database table crs, overwriting the existing crs table.
#' 
#' @examples
#' \dontrun{
#' # Call the function to save and overwrite the Statsys data
#' create_crs_data_to_db(df_crs)
#' }
#'
#' @importFrom nanoparquet read_parquet
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom duckdb duckdb
#' @export
create_crs_data_to_db <- function(df_crs) {
  
  # Check if the crs data frame exists and is a data frame
  stopifnot(inherits(df_crs, "tbl"))

  # Get the user-specific DuckDB path
  db_path <- get_duckdb_path()
  
  # Check if the DuckDB database file is accessible
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }

  # Connect to the database
  con <- dbConnect(duckdb(), db_path)

  # Write the data to the database table crs
  dbWriteTable(con, "crs", df_crs, overwrite = TRUE)
  
  # Close the connection
  dbDisconnect(con, shutdown = TRUE)
  
  # Success message with emoji 🎉
  message("🎉 Success! The crs table in the DuckDB database is overwritten by the new data.")
}
