#' Add country code columns to dataframe
#'
#' @param data Input dataframe of Norwegian development assistance
#'
#' @return Returns dataframe with additional columns:
#' \itemize{
#'   \item ISO3: ISO-3 character code, identified using package \emph{countrycode} based on column \emph{Recipient country}. Non-matches are given NA values and are returned in a warning message. 
#' }
#'
#' @export
#' @examples
#' ?add_cols_countrycode()

add_cols_countrycode <- function(data) {
  
  # Data frame of unique countries, by removing observations with unspecified income category
  df_unique_countries <- data |> 
    dplyr::filter(`Income category` != "Unspecified") |> 
    dplyr::select(`Recipient country`) |> 
    dplyr::arrange(`Recipient country`) |> 
    unique()
  
  # Add column ISO3 character code to the data frame of unique countries. Non-matches are given NA values and are returned in a warning message. 
  df_unique_countries <- df_unique_countries |> 
    dplyr::mutate(ISO3 = countrycode::countrycode(`Recipient country`, origin = "country.name", destination = "iso3c",
                                                  custom_match = c("Kosovo" = "XKX")))
  
  # Include column ISO3 in the original data frame
  data <- dplyr::left_join(data, df_unique_countries, by = "Recipient country")
}