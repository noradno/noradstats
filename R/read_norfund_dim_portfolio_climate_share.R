#' Read Norfund DIM Portfolio Climate Share data into R
#'
#' This function imports a data frame of the annual climate mitigation share (2-year averages) of the Norfund DIM 
#' (Development Investment Mandate) portfolio from the DuckDB database.
#'
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom duckdb duckdb
#' @importFrom tibble as_tibble
#' @return Returns a tibble with two columns: `year` and `climate_share`, from the 'norfund_dim_portfolio_climate_share' table in the DuckDB database.
#' @examples
#' \dontrun{
#' # Read the Norfund DIM Portfolio Climate Share from the DuckDB database:
#' df_climate_share <- read_norfund_dim_portfolio_climate_share()
#' 
#' # Display the first few rows of the data:
#' head(df_climate_share)
#' }
#' @export
#'
read_norfund_dim_portfolio_climate_share <- function() {
  
  # Get the user-specific DuckDB path
  db_path <- get_duckdb_path()
  
  # Check if the DuckDB database file is accessible
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }
  
  # Connect to the DuckDB database
  con <- dbConnect(duckdb(), db_path)
  
  # Read all data from the 'norfund_dim_climate_share' table
  df_norfund_dim_portfolio_climate_share <- dbReadTable(con, "norfund_dim_portfolio_climate_share") |> 
    as_tibble()
  
  # Disconnect from the database
  dbDisconnect(con, shutdown = TRUE)
  
  return(df_norfund_dim_portfolio_climate_share)
}
