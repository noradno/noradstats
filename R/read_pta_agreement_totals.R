#' Read PTA Agreement Totals data table from DuckDB into R
#'
#' This function imports from the DuckDB database a data frame of PTA Agreement Totals from 1960 to today.
#'
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom duckdb duckdb
#' @importFrom tibble as_tibble
#' @return Returns a tibble with 7 columns: `agreement_no`, `agreement_title`, `agreement_partner`, `agreement_period`, `agreement_period_from`, `agreement_period_to` and `expected_agreement_total`.
#' @examples
#' \dontrun{
#' # Read the pta_agreement_totals table from the DuckDB database:
#' df_pta_agreement_totals <- read_pta_agreement_totals()
#' 
#' # Display the first few rows of the data:
#' head(df_pta_agreement_totals)
#' }
#' @export
#'
read_pta_agreement_totals <- function() {
  
  # Get the user-specific DuckDB path
  db_path <- get_duckdb_path()
  
  # Check if the DuckDB database file is accessible
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }
  
  # Connect to the DuckDB database
  con <- dbConnect(duckdb(), db_path)
  
  # Read all data from the 'pta_agreement_totals' table
  df_pta_agreement_totals <- dbReadTable(con, "pta_agreement_totals") |> 
    as_tibble()
  
  # Disconnect from the database
  dbDisconnect(con, shutdown = TRUE)
  
  return(df_pta_agreement_totals)
}
