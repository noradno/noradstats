# ========== Exported Function ==========

#' Import and save imputed multilateral marker data into DuckDB database
#'
#' This is the exported main function that calls internal helper functions to import and save 
#' the imputed multilateral marker data into the DuckDB database.
#'
#' Steps:
#' 1. Imports the imputed multilateral marker data from an Excel spreadsheet. 
#'    The file must contain the columns: `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
#'    The imported spreadsheet should not include any observations with NA share, only real values.
#'    The share for each agreement partner is calculated by the OECD secretariate and available online.
#' 2. Saves the imported data into the DuckDB database under the table name 'imputed_multi_marker_ratio'.
#'
#' @param filepath A string. The path to the Excel file containing the imputed multilateral marker data. 
#' The Excel file must contain the following columns:
#' - `aid_type`: Character string representing the type of aid.
#' - `agreement_partner`: Character string representing the partner involved in the agreement.
#' - `marker`: Character string representing the multilateral marker, meaning climate, environment etc.
#' - `year`: Integer representing the year.
#' - `share`: Numeric value representing the share of the marker.
#' 
#' @details 
#' This function imports an Excel spreadsheet containing imputed multilateral marker data, 
#' which must have the following columns: `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
#' 
#' Once imported, the data is saved into a table named `imputed_multi_marker_ratio` in the DuckDB database. 
#' This table will overwrite any existing data with the same name in the database.
#'
#' @return A data frame containing the imported multilateral marker data with the following columns:
#' `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
#' 
#' @examples
#' \dontrun{
#' # Example: Import and save multilateral marker data from a file
#' create_imputed_multi_marker_to_db("path/to/imputed_multi_marker.xlsx")
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate
#' @export
create_imputed_multi_marker_to_db <- function(filepath = "C:/Users/aaw262/Documents/R/norfund_climate/imputed_multi_marker.xlsx") {
  
  # Step 1: Import the imputed multilateral marker data from Excel
  df_imputed_multi_marker_ratio <- import_imputed_multi_marker_ratio(filepath)
  
  # Step 2: Save the imported data to the DuckDB database
  save_imputed_multi_marker_ratio_to_db(df_imputed_multi_marker_ratio)
  
  return(df_imputed_multi_marker_ratio)
}

# ========== Internal Helper Functions ==========

# Internal function to import imputed multilateral marker data from an Excel spreadsheet
# The Excel file must contain the columns `aid_type`, `agreement_partner`, `marker`, `year`, and `share`.
import_imputed_multi_marker_ratio <- function(filepath = "C:/Users/aaw262/Documents/R/norfund_climate/imputed_multi_marker.xlsx") {
  if (!file.exists(filepath)) {
    stop("The imputed_multi_marker.xlsx file does not exist: ", filepath)
  }
  
  df_imputed_multi_marker_ratio <- read_xlsx(filepath) |> 
    mutate(year = as.integer(year))
  
  return(df_imputed_multi_marker_ratio)
}

# Internal function to save the imputed multilateral marker data into the DuckDB database
# This saves the data frame into the table named 'imputed_multi_marker_ratio' in DuckDB.
save_imputed_multi_marker_ratio_to_db <- function(df_imputed_multi_marker_ratio) {
  db_path <- get_duckdb_path()
  con <- dbConnect(duckdb(), db_path)
  dbWriteTable(con, "imputed_multi_marker_ratio", df_imputed_multi_marker_ratio, overwrite = TRUE)
  dbDisconnect(con, shutdown = TRUE)
  message("🎉 Success! The 'imputed_multi_marker_ratio' table has been successfully updated in the DuckDB database.")
}
