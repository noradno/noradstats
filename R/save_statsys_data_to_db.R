#' Save and overwrite Statsys Data to DuckDB Database
#'
#' Reads Statsys data from a CSV file, processes it by adding basic and country 
#' columns, cleans the column names, and then saves the data to a DuckDB database.
#' The existing data in the "statsys" table in the database will be overwritten.
#' 
#' Before running this function, it is recommended that users first inspect the
#' data by running the `read_statsys_from_csv()` function to ensure that the data
#' has the expected structure and column types.
#' 
#' @param input_csv A string. Path to the input CSV file containing the Statsys data.
#'
#' @details 
#' The function checks if both the CSV file and the DuckDB database file exist. If either
#' file is not accessible, the function will stop and return an error message. It uses 
#' internal `noradstats` functions to read and process the data by adding basic and 
#' country-related columns. The `janitor` package is used to clean column names. The resulting 
#' data is then written to the DuckDB database, overwriting the existing "statsys" table.
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' # Path to the CSV file containing the Statsys data
#' input_csv <- "path/to/your_statsys_data.csv"
#'
#' # Call the function to save and overwrite the Statsys data
#' save_statsys_data_to_db(input_csv)
#' }

#'
#' @importFrom janitor clean_names
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom duckdb duckdb
#' @export
save_statsys_data_to_db <- function(input_csv) {
  
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
  df_statsys <- read_statsys_from_csv(input_csv) |> 
    add_cols_basic() |> 
    add_cols_countrycode() |> 
    clean_names()
  
  # Connect to the database
  con <- dbConnect(duckdb(), db_path)
  
  # Write the data to the database
  dbWriteTable(con, "statsys", df_statsys, overwrite = TRUE)
  
  # Close the connection
  dbDisconnect(con, shutdown = TRUE)
  
  # Success message with emoji 🎉
  message("🎉 Success! The 'statsys' table in the DuckDB database is overwritten by the new data.")
}
