#' Read Imputed Multilateral Shares data into R
#'
#' This function imports from the DuckDB database a data frame of the annual imputed multilateral shares
#' calculated by the OECD-secretariate (except for Norad estimates for the last year). A column named Marker
#' identifies the Marker share, for instance: climate.
#'
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom duckdb duckdb
#' @importFrom tibble as_tibble
#' @return Returns a tibble with two columns: `aid_type`, `agreement_partner`, `marker`, `year` and `share`, from the 'imputed_multi_shares' table in the DuckDB database.
#' @examples
#' \dontrun{
#' # Read the imputed_multi_shares table from the DuckDB database:
#' df_imputed_multi_shares <- read_imputed_multi_shares()
#' 
#' # Display the first few rows of the data:
#' head(df_imputed_multi_shares)
#' }
#' @export
#'
read_imputed_multi_shares <- function() {
  
  # Get the user-specific DuckDB path
  db_path <- get_duckdb_path()
  
  # Check if the DuckDB database file is accessible
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }
  
  # Connect to the DuckDB database
  con <- dbConnect(duckdb(), db_path)
  
  # Read all data from the 'imputed_multi_shares' table
  df_imputed_multi_shares <- dbReadTable(con, "imputed_multi_shares") |> 
    as_tibble()
  
  # Disconnect from the database
  dbDisconnect(con, shutdown = TRUE)
  
  return(df_imputed_multi_shares)
}
