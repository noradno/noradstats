# ========== Exported Function ==========

#' Create and save a data frame of the imputed climate share of capitalisation of ordinary Norfund DIM and save the table in DuckDB database
#'
#' This is the exported main function that calls internal helper functions to create the data frame of the imputed climate share of
#' capitalisation of ordinary Norfund DIM and save the table in DuckDB database.
#'
#' Steps:
#' 1. Imports capitalisation agreements from an Excel spreadsheet ("agreement_number_norfund_dim_capitalisation").
#' 2. Imports the annual climate mitigation share (2-year average) for the Norfund DIM portfolio from the DuckDB database.
#' 3. Builds a data frame of the imputed climate share of the Norfund capitalisation agreements by year.
#' 4. Saves the data frame into the DuckDB database under the table name 'imputed_norfund_climate_share'.
#'
#' @param filepath A string. The path to the Excel file containing the capitalisation agreements. The file must contain a column named 'agreement_number'.
#' @return A data frame containing 'agreement_number', 'year', and 'climate_share_2yr_avg' for the imputed climate share of Norfund capitalisation agreements.
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate select distinct left_join
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom duckdb duckdb
#' @export
create_imputed_norfund_climate_share_to_db <- function(filepath = "agreement_number_norfund_dim_capitalisation.xlsx") {
  
  # Step 1: Import capitalisation data from Excel
  df_capitalisation <- import_norfund_capitalisation(filepath)
  
  # Step 2: Import Norfund climate share data from DuckDB
  df_norfund_dim_portfolio_climate_share <- import_norfund_dim_portfolio_climate_share()
  
  # Step 3: Build imputed Norfund climate data frame
  df_imputed_norfund_climate_share <- build_imputed_norfund_climate_share(df_capitalisation, df_norfund_dim_portfolio_climate_share)
  
  # Step 4: Save the resulting data frame to the DuckDB database
  save_imputed_norfund_climate_share_to_db(df_imputed_norfund_climate_share)

  return(df_imputed_norfund_climate_share)
}

# ========== Internal Helper Functions ==========

# Internal function to read capitalisation data from Excel
# This imports a data frame containing capitalisation agreements of Norfund DIM
# The column in the data frame should be 'agreement_number', with rows being the unique agreement numbers.
import_norfund_capitalisation <- function(filepath = "agreement_number_norfund_dim_capitalisation.xlsx") {
  if (!file.exists(filepath)) {
    stop("The capitalisation Excel file does not exist: ", filepath)
  }
  
  df_capitalisation <- read_xlsx(filepath)  # Assigning the read data to a variable
  
  # Check if the 'agreement_number' column exists
  if (!"agreement_number" %in% colnames(df_capitalisation)) {
    stop("The Excel file must contain a column named 'agreement_number'.")
  }
  
  return(df_capitalisation)
}

# Internal function to read Norfund climate share from DuckDB database
# This imports a data frame of the annual climate mitigation share (2-year averages) for the ordinary Norfund DIM portfolio.
# It uses the function read_norfund_dim_portfolio_climate_share() to import the data, and includes the connection check.
import_norfund_dim_portfolio_climate_share <- function() {
  
  # Get the user-specific DuckDB path
  db_path <- get_duckdb_path()
  
  # Check if the DuckDB database file is accessible
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }
  
  # Read Norfund climate share from DuckDB
  noradstats::read_norfund_dim_portfolio_climate_share() |> 
    select(year, climate_share_2yr_avg)
}

# Internal function to create a data frame of imputed climate share of the capitalisation agreements per year.
# It uses the function read_oda() to find the relevant years for each agreement, filters out data after 2014,
# as 2015 is the first year with a climate_share value, and merges the climate share with the year column.
build_imputed_norfund_climate_share <- function(df_capitalisation, df_norfund_dim_portfolio_climate_share) {
  read_oda() |> 
    filter(
      agreement_number %in% df_capitalisation$agreement_number,
      year > 2014
    ) |> 
    distinct(agreement_number, year) |> 
    left_join(df_norfund_dim_portfolio_climate_share, by = "year")
}

# Internal function to save the resulting data frame into DuckDB
# This saves the final data frame containing 'agreement_number', 'year', and 'climate_share_2yr_avg' 
# into the DuckDB database, in a table named 'imputed_norfund_climate_share'.
save_imputed_norfund_climate_share_to_db <- function(df_imputed_norfund_climate_share) {
  db_path <- get_duckdb_path()
  con <- dbConnect(duckdb(), db_path)
  dbWriteTable(con, "imputed_norfund_climate_share", df_imputed_norfund_climate_share, overwrite = TRUE)
  dbDisconnect(con, shutdown = TRUE)
  message("🎉 Success! The 'imputed_norfund_climate_share' table has been successfully updated in the DuckDB database.")
}
