#' Read Statsys data into R
#'
#' This function imports all data from the statsys table in the DuckDB database.
#' This data includes Norwegian official development assistance (ODA), ODA frame agreement level data, Other official flows(OOF), export credits and private flows.
#' The data covers 1960 to recent year.
#' The DuckDB database file is located on Norads Microsoft Sharepoint site and is expected to be synced via Microsoft Teams to to the users local directory.
#'
#' @param version A character string specifying which table to read from. If "statsys_official", the function reads from the "statsys_official" table. If "statsys_active", the function reads from the "statsys_active" table. Defaults to "statsys_official".
#' @return Returns a tibble of ODA, OOF and PF data from the statsys table in the DuckDB database.
#' @importFrom noradstats get_duckdb_path
#' @export
#' @examples
#' ?read_statsys()
#'

read_statsys <- function(version = "statsys_official") {
  
  # User specific file path
  default_path <- get_duckdb_path()
  
  # Connect to the statsys table in the DuckDB database
  con <- DBI::dbConnect(duckdb::duckdb(), default_path)

  # Determine the table to read based on the version argument
  if (version == "statsys_official") {
    table_name <- "statsys_official"
  } else if (version == "statsys_active") {
    table_name <- "statsys_active"
  } else {
    stop("Invalid statsys version argument")
  }
  
  # Read all data from the statsys table
  df_statsys <- con |>
    DBI::dbReadTable(table_name) |>
    tibble::as_tibble()
  
  # Disconnect database
  DBI::dbDisconnect(con, shutdown = TRUE)
  
  return(df_statsys)
}
