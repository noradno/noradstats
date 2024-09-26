# ========== Exported Function ==========

#' Create a table of the Norfund DIM Portfolio Climate Ratio and store it in the DuckDB database
#'
#' This function calculates the annual climate mitigation ratio (2 year averages) of the Norfund DIM (development mandate) portfolio and saves the year-ratio data frame in the DuckDB database.
#' The user must provide an Excel file containing the Norfund CIM (Climate Investment Mandate) agreements to be excluded.
#' 
#' @details 
#' After running this function, the 'norfund_dim_climate_ratio' table will be created or overwritten in 
#' the DuckDB database, and a success message will be displayed.
#' The `create_norfund_dim_portfolio_climate_ratio_to_db()` function performs the following steps using internal helper functions:
#' \itemize{
#'   \item `import_cim_data()`: Imports the CIM agreements data from an Excel file.
#'   \item `prepare_norfund_dim_data()`: Prepares the Norfund DIM data by importing statsys data, filtering out CIM agreements from 2022 onwards, 
#'   and performing necessary data processing.
#'   \item `calculate_climate_ratio()`: Calculates the two-year climate mitigation ratio.
#'   \item `save_climate_ratio_to_db()`: Saves the calculated climate ratio data to the DuckDB database.
#' }
#'
#' @param cim_filepath A string. The path to the Excel file containing the CIM agreements to exclude. Default is "avtalenr_cim.xlsx". The file must contain the two columns *agreement_number* and *cim_dim*.
#' @return This function does not return a value. It saves the climate ratio table to the DuckDB database 
#' and displays a success message upon completion.
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate rename summarize group_by ungroup arrange anti_join if_else
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom stringr str_trim
#' @examples
#' \dontrun{
#' # Use default CIM file path
#' create_norfund_dim_portfolio_climate_ratio_to_db()
#'
#' # Specify a different CIM file path
#' create_norfund_dim_portfolio_climate_ratio_to_db("path/to/cim_file.xlsx")
#' }
#' @export
create_norfund_dim_portfolio_climate_ratio_to_db <- function(cim_filepath = "agreement_number_norfund_cim.xlsx") {
  # Imports CIM agreements data from an Excel file.
  df_cim <- import_cim_data(cim_filepath)
  print(df_cim)

  # Prepares the Norfund DIM data by filtering out CIM etc, and processing.
  df_norfund_dim <- prepare_norfund_dim_data(df_cim)
  print(df_norfund_dim)
  
  # Calculates the two-year climate mitigation ratios
  df_norfund_dim_climate_ratio <- calculate_climate_ratio(df_norfund_dim)
  print(df_norfund_dim_climate_ratio)
  
  # Saves the climate ratio data to the DuckDB database.
  save_climate_ratio_to_db(df_norfund_dim_climate_ratio)

  return(df_norfund_dim_climate_ratio)
}

# ========== Internal Helper Functions ==========

#' Import CIM Data from Excel (internal function)
#'
#' This is an **internal helper function** and is not exported for direct use. It reads a table of
#' Climate Investment Mandate (CIM) data from an Excel file.
#'
#' The Excel file must contain two columns: `agreement_number` and `cim_dim`.
#' 
#' @return A data frame containing the CIM agreements.
import_cim_data <- function(filepath = "agreement_number_norfund_cim.xlsx") {
  if (!file.exists(filepath)) {
    stop("The CIM Excel file does not exist: ", filepath)
  }
  
  df_cim <- read_xlsx(filepath)

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
    add_cols_climate_clean() |> 
    filter(extending_agency == "Norfund") |> 
    filter(type_of_assistance != "Administration") |>  
    filter(type_of_flow == "OOF") |>
    filter(year >= 2014) |> 
    mutate(agreement_number = str_trim(agreement_number)) |> 
    filter(!(year >= 2022 & agreement_number %in% df_cim$agreement_number)) |> 
    mutate(
      total_finance_nok = if_else(amounts_extended_1000_nok < 0, 0, amounts_extended_1000_nok * 1e3),
      mitigation_finance_nok = climate_mitigation_nok_mill_gross_fix * 1e6
    )
  
  return(df_norfund_dim)
}

#' Calculate Climate Mitigation Ratio for Norfund DIM (internal function)
#'
#' This is an **internal helper function** and is not exported for direct use. It calculates
#' the two-year climate mitigation ratio for the Norfund DIM data.
#' @return A data frame containing the climate mitigation ratios.
calculate_climate_ratio <- function(df_norfund_dim) {
  df_norfund_dim_climate_ratio <- df_norfund_dim |> 
    group_by(year) |> 
    summarize(
      mitigation_finance_nok = sum(mitigation_finance_nok, na.rm = TRUE),
      total_finance_nok = sum(total_finance_nok, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    arrange(year) |> 
    mutate(
      climate_ratio_2yr_avg = (lag(mitigation_finance_nok) + mitigation_finance_nok) / 
                              (lag(total_finance_nok) + total_finance_nok)
    )
  
  return(df_norfund_dim_climate_ratio)
}

#' Save Climate Mitigation Ratio Data to DuckDB (internal function)
#'
#' This is an **internal helper function** and is not exported for direct use. It saves
#' the calculated Norfund DIM climate ratio data to the DuckDB database and diconnects.
save_climate_ratio_to_db <- function(df_norfund_dim_climate_ratio) {
  db_path <- get_duckdb_path()
  if (!file.exists(db_path)) {
    stop("The DuckDB database file does not exist or is inaccessible: ", db_path)
  }
  con <- dbConnect(duckdb(), db_path)
  dbWriteTable(con, "norfund_dim_portfolio_climate_ratio", df_norfund_dim_climate_ratio, overwrite = TRUE)
  dbDisconnect(con, shutdown = TRUE)
  message("🎉 Success! The 'norfund_dim_portfolio_climate_ratio' table has been successfully updated in the DuckDB database.")
}
