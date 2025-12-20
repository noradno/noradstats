#' Read PTA Disbursement level data table from DuckDB into R
#'
#' This function imports from the DuckDB database a data frame of PTA Disbursement level from 2000 and beyond
#'
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom duckdb duckdb
#' @importFrom tibble as_tibble
#' @return Returns a tibble with many columns.
#' @examples
#' \dontrun{
#' # Read the pta_disbursement_level table from the DuckDB database:
#' df_pta_disbursement_level <- read_pta_disbursement_level()
#' 
#' # Display the first few rows of the data:
#' head(df_pta_disbursement_level)
#' }
#' @export
#'
read_pta_disbursement_level <- function() {
  
  # Get the user-specific DuckDB path
  db_path <- get_duckdb_path()
  
  # Check if the DuckDB database file is accessible
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }
  
  # Connect to the DuckDB database
  con <- dbConnect(duckdb(), db_path)
  
  # Read all data from the 'pta_disbursement_level' table
  df_pta_disbursement_level <- dbReadTable(con, "pta_disbursement_level") |> 
    as_tibble()
  
  # Disconnect from the database
  dbDisconnect(con, shutdown = TRUE)
  
  return(df_pta_disbursement_level)
}
