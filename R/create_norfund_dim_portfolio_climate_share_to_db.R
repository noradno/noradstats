# ========== Exported Function ==========

#' Create a table of the Norfund DIM Portfolio Climate Share and store it in the DuckDB database
#'
#' This function calculates the annual climate mitigation share (2 year averages) of the Norfund DIM (development mandate) portfolio and saves the year-share data frame in the DuckDB database.
#' The user must provide an Excel file containing the Norfund CIM (Climate Investment Mandate) agreements to be excluded.
#' 
#' @details 
#' After running this function, the 'norfund_dim_climate_share' table will be created or overwritten in 
#' the DuckDB database, and a success message will be displayed.
#' The `create_norfund_dim_portfolio_climate_share_to_db()` function performs the following steps using internal helper functions:
#' \itemize{
#'   \item `import_cim_data()`: Imports the CIM agreements data from an Excel file.
#'   \item `prepare_norfund_dim_data()`: Prepares the Norfund DIM data by importing statsys data, filtering out CIM agreements from 2022 onwards, 
#'   and performing necessary data processing.
#'   \item `calculate_climate_share()`: Calculates the two-year climate mitigation share.
#'   \item `save_climate_share_to_db()`: Saves the calculated climate share data to the DuckDB database.
#' }
#'
#' @param cim_filepath A string. The path to the Excel file containing the CIM agreements to exclude. Default is "avtalenr_cim.xlsx". The file must contain the two columns *agreement_number* and *cim_dim*.
#' @return This function does not return a value. It saves the climate share table to the DuckDB database 
#' and displays a success message upon completion.
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate rename summarize group_by ungroup arrange anti_join if_else
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom stringr str_trim
#' @examples
#' \dontrun{
#' # Use default CIM file path
#' create_norfund_dim_portfolio_climate_share_to_db()
#'
#' # Specify a different CIM file path
#' create_norfund_dim_portfolio_climate_share_to_db("path/to/cim_file.xlsx")
#' }
#' @export
create_norfund_dim_portfolio_climate_share_to_db <- function(cim_filepath = "agreement_number_norfund_cim_portfolio.xlsx") {
  # Imports CIM agreements data from an Excel file.
  df_cim <- import_cim_data(cim_filepath)
  print(df_cim)

  # Prepares the Norfund DIM data by filtering out CIM etc, and processing.
  df_norfund_dim <- prepare_norfund_dim_data(df_cim)
  print(df_norfund_dim)
  
  # Calculates the two-year climate mitigation shares
  df_norfund_dim_climate_share <- calculate_climate_share(df_norfund_dim)
  
  # Saves the climate share data to the DuckDB database.
  save_climate_share_to_db(df_norfund_dim_climate_share)

  return(df_norfund_dim_climate_share)
}

# ========== Internal Helper Functions ==========

#' Import CIM Data from Excel (internal function)
#'
#' This is an **internal helper function** and is not exported for direct use. It reads a table of 
#' Climate Investment Mandate (CIM) data from an Excel file.
#'
#' @param cim_filepath A string. The path to the Excel file.
#' @return A data frame containing the CIM agreements.
import_cim_data <- function(cim_filepath) {
  if (!file.exists(cim_filepath)) {
    stop("The agreement_number_norfund_cim_portfolio.xlsx file does not exist: ", cim_filepath)
  }
  
  df_cim <- read_xlsx(cim_filepath)

  # Check if the 'agreement_number' column exists
  if (!"agreement_number" %in% colnames(df_cim)) {
     stop("The Excel file must contain a column named 'agreement_number'.")
   }
  
  df_cim <- df_cim |> 
    mutate(agreement_number = str_trim(agreement_number))
  
  return(df_cim)
}

#' Prepare Norfund DIM Data (internal function)
#'
#' This is an **internal helper function** and is not exported for direct use. It imports statsys data and
#' process the data to include only relevant Norfund DIM data (Development Investment Mandate (DIM).
#' Excludes CIF agreements from 2022 and onwards (keeping old agreements before CIM was created - as there are old agreements also transfered from DIM to CIM in 2022)
#' @return A data frame of the Norfund DIM data.
prepare_norfund_dim_data <- function(df_cim) {
  df_norfund_dim <- read_statsys() |> 
    filter(extending_agency == "Norfund") |> 
    filter(type_of_assistance != "Administration") |>  
    filter(type_of_flow == "OOF") |>
    filter(year >= 2014) |> 
    mutate(agreement_number = str_trim(agreement_number)) |> 
    filter(!(year >= 2022 & agreement_number %in% df_cim$agreement_number)) |> 
    mutate(
      total_finance_nok = amounts_extended_1000_nok * 1e3,
      mitigation_finance_nok = case_when(
        pm_climate_change_mitigation == "Main objective" ~ amounts_extended_1000_nok * 1e3,
        pm_climate_change_mitigation == "Significant objective" ~ (amounts_extended_1000_nok * 1e3) * 0.4,
        .default = as.numeric(0))
    )
  
  return(df_norfund_dim)
}

#' Calculate Climate Mitigation Share for Norfund DIM (internal function)
#'
#' This is an **internal helper function** and is not exported for direct use. It calculates
#' the two-year climate mitigation share for the Norfund DIM data.
#' Removes rows with NA climate_share (2014, the first year)
#' @return A data frame containing the climate mitigation shares.
calculate_climate_share <- function(df_norfund_dim) {
  df_norfund_dim_climate_share <- df_norfund_dim |> 
    group_by(year) |> 
    summarize(
      mitigation_finance_nok = sum(mitigation_finance_nok, na.rm = TRUE),
      total_finance_nok = sum(total_finance_nok, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    arrange(year) |> 
    mutate(
      climate_share = (lag(mitigation_finance_nok) + mitigation_finance_nok) / 
                              (lag(total_finance_nok) + total_finance_nok)
    ) |> 
    filter(!is.na(climate_share))
  
  return(df_norfund_dim_climate_share)
}

#' Save Climate Mitigation Share Data to DuckDB (internal function)
#'
#' This is an **internal helper function** and is not exported for direct use. It saves
#' the calculated Norfund DIM climate share data to the DuckDB database and diconnects.
save_climate_share_to_db <- function(df_norfund_dim_climate_share) {
  db_path <- get_duckdb_path()
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }
  con <- dbConnect(duckdb(), db_path)
  dbWriteTable(con, "norfund_dim_portfolio_climate_share", df_norfund_dim_climate_share, overwrite = TRUE)
  dbDisconnect(con, shutdown = TRUE)
  message("🎉 Success! The 'norfund_dim_portfolio_climate_share' table has been successfully updated in the DuckDB database.")
}
