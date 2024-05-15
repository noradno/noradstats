#' Read Norwegian ODA data into R
#'
#' This function imports all Norwegian Official Development (ODA) from the Statsys table in the DuckDB database.
#' This include ODA data from 1960 to the recent year. Frame agreement level data is excluded.
#' The DuckDB database file is located on Norads Microsoft Sharepoint site and is expected to be synced via Microsoft Teams to to the users local directory.
#'
#' @return Returns a tibble of Norwegian ODA data from 1960 to the recent year. Frame agreement level data is excluded.
#' @export
#' @examples
#' ?read_oda()
#'

read_oda <- function() {
  
  # Read all data from the statsys table in DuckDB database
  df_statsys <- read_statsys()
  
  # Filter the data to include only ODA data and exclude frame agreement level data
  df_oda <- df_statsys |> 
    dplyr::filter(
      type_of_flow == "ODA",
      type_of_agreement != "Rammeavtale"
      )

  return(df_oda)
}
