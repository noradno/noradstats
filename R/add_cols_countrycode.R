#' Add ISO3 country code columns to dataframe
#'
#' @param data Input data frame of Norwegian development assistance, with column \emph{Recipient country} containing country names.
#'
#' @return Returns data frame with additional column:
#' \itemize{
#'   \item iso3: iso3 character code, identified using package \emph{countrycode} based on column \emph{Recipient country}. Non-matches are given NA values and are returned in a warning message. 
#' }
#'
#' @export
#' @examples
#' ?add_cols_countrycode()

add_cols_countrycode <- function(data) {
  
  # Data frame of unique countries, by removing global, regional, adm. Not using Income category to filtering as the variable was introduced in 1999.
  df_unique_countries <- data |> 
    dplyr::filter(!stringr::str_detect(`Recipient country`, "Regional|regional|Multilateral|Global|Administration")) |> 
    dplyr::select(`Recipient country`) |> 
    dplyr::arrange(`Recipient country`) |> 
    unique()
  
  # Add column iso3 character code to the data frame of unique countries. Non-matches are given NA values and are returned in a warning message. 
  df_unique_countries <- df_unique_countries |> 
    dplyr::mutate(iso3 = countrycode::countrycode(`Recipient country`, origin = "country.name", destination = "iso3c",
                                                  custom_match = c("Kosovo" = "XKX",
                                                                   "Yemen Dem. Rep." = "YEM")))
  
  # Include column ISO3 in the original data frame
  data <- dplyr::left_join(data, df_unique_countries, by = "Recipient country")
}