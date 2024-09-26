# ========== Exported Function ==========

#' Import and save imputed multilateral shares data into DuckDB database
#'
#' This is the exported main function that calls internal helper functions to import and save 
#' the imputed multilateral shares data into the DuckDB database.
#'
#' Steps:
#' 1. Imports the imputed multilateral shares data from an Excel spreadsheet. 
#'    The file must contain the columns: `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
#'    The imported spreadsheet should not include any observations with NA share, only real values.
#'    The share for each agreement partner is calculated by the OECD secretariate and available online.
#' 2. Saves the imported data into the DuckDB database under the table name 'imputed_multi_shares'.
#'
#' @param filepath A string. The path to the Excel file containing the imputed multilateral shares data. 
#' The Excel file must contain the following columns:
#' - `aid_type`: Character string representing the type of aid.
#' - `agreement_partner`: Character string representing the partner involved in the agreement.
#' - `marker`: Character string representing the multilateral marker, meaning climate, environment etc.
#' - `year`: Integer representing the year.
#' - `share`: Numeric value representing the share of the shares.
#' 
#' @details 
#' This function imports an Excel spreadsheet containing imputed multilateral shares data, 
#' which must have the following columns: `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
#' 
#' Once imported, the data is saved into a table named `imputed_multi_shares` in the DuckDB database. 
#' This table will overwrite any existing data with the same name in the database.
#'
#' @return A data frame containing the imported multilateral shares data with the following columns:
#' `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
#' 
#' @examples
#' \dontrun{
#' # Example: Import and save multilateral shares data from a file
#' create_imputed_multi_shares_to_db("path/to/imputed_multi_shares.xlsx")
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate
#' @export
create_imputed_multi_shares_to_db <- function(filepath = "C:/Users/aaw262/Documents/R/norfund_climate/imputed_multi_shares.xlsx") {
  
  # Step 1: Import the imputed multilateral shares data from Excel
  df_imputed_multi_shares <- import_imputed_multi_shares(filepath)
  
  # Step 2: Save the imported data to the DuckDB database
  save_imputed_multi_shares_to_db(df_imputed_multi_shares)
  
  return(df_imputed_multi_shares)
}

# ========== Internal Helper Functions ==========

# Internal function to import imputed multilateral shares data from an Excel spreadsheet
# The Excel file must contain the columns `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
import_imputed_multi_shares <- function(filepath = "C:/Users/aaw262/Documents/R/norfund_climate/imputed_multi_shares.xlsx") {
  if (!file.exists(filepath)) {
    stop("The imputed_multi_shares.xlsx file does not exist: ", filepath)
  }
  
  df_imputed_multi_shares <- read_xlsx(filepath) |> 
    mutate(year = as.integer(year))
  
  return(df_imputed_multi_shares)
}

# Internal function to save the imputed multilateral shares data into the DuckDB database
# This saves the data frame into the table named 'imputed_multi_shares' in DuckDB.
save_imputed_multi_shares_to_db <- function(df_imputed_multi_shares) {
  db_path <- get_duckdb_path()
  con <- dbConnect(duckdb(), db_path)
  dbWriteTable(con, "imputed_multi_shares", df_imputed_multi_shares, overwrite = TRUE)
  dbDisconnect(con, shutdown = TRUE)
  message("🎉 Success! The 'imputed_multi_shares' table has been successfully updated in the DuckDB database.")
}
