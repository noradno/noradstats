#' Function to import, save and overwrite PTA Agreement totals data to DuckDB Database
#'
#' Reads PTA Agreement totals from a CSV file, processes it, cleans the column names, and then saves the data to a DuckDB database.
#' The existing data in the "pta_agreement_totals" table in the database will be overwritten.
#' 
#' Before running this function, it is recommended that users first inspect the
#' data by running the `read_pta_agreement_totals_from_csv()` function to ensure that the data
#' has the expected structure and column types.
#' 
#' @param input_csv A string. Path to the input CSV file containing the Statsys data.
#'
#' @details 
#' The function checks if both the CSV file and the DuckDB database file exist. If either
#' file is not accessible, the function will stop and return an error message. It uses 
#' internal `noradstats` functions to read and process the data. 
#' The `janitor` package is used to clean column names. The resulting 
#' data is then written to the DuckDB database, overwriting the existing "pta_agreement_totals" table.
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' # Path to the CSV file containing the Statsys data
#' input_csv <- "path/to/your_pta_report.csv"
#'
#' # Call the function to save and overwrite the Statsys data
#' create_pta_agreement_totals_to_db(input_csv)
#' }

#'
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom duckdb duckdb
#' @export
create_pta_agreement_totals_to_db <- function(input_csv) {
  
  # Check if the input file exists
  if (!file.exists(input_csv)) {
    stop("The input CSV file does not exist.")
  }
  
  # Get the user-specific DuckDB path
  db_path <- get_duckdb_path()
  
  # Check if the DuckDB database file is accessible
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }

  # Read the CSV file and process the data
  df_pta_agreement_totals <- read_pta_agreement_totals_from_csv(input_csv)
  
  # Connect to the database
  con <- dbConnect(duckdb(), db_path)
 
  # Write the data to the database
  dbWriteTable(con, "pta_agreement_totals", df_pta_agreement_totals, overwrite = TRUE)
  
  # Close the connection
  dbDisconnect(con, shutdown = TRUE)
  
  # Success message with emoji 🎉
  message("🎉 Success! The 'pta_agreement_totals' table in the DuckDB database is overwritten by the new data.")
}
