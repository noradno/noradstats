#' Add humanitarian (hum) columns to an existing oda data frame
#'
#' This function takes an existing oda data frame as input and adds hum columns.
#' The function returns the oda data frame with the following additional columns for hum:
#' \describe{
#'   \item{\code{hum_nok}}{Numeric variable of total disbursed hum oda (earmarked and imputed multilateral).}
#'   \item{\code{hum_tag}}{Logical variable to identify hum activities, meaning earmarked ODA for DAC sectors and imputed multilateral ODA for hum (positive imputed multilateral hum shares)}
#'   \item{\code{hum_oda_channel}}{Categorical variable to separate earmarked oda to hum from imputed multilateral oda to hum.}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the oda data frame by using
#' \code{noradstats::read_oda()}.
#' 
#' @import dplyr
#' @importFrom noradstats read_imputed_multi_sector_shares
#' @param df_oda A oda data frame, which must already be loaded into the environment.
#' @return A oda data frame with additional hum columns.
#' @export
#'
#' @examples
#' # Load the oda data
#' df_oda <- read_oda()
#'
#' # Add hum columns to the df_oda data
#' df_oda_hum <- add_cols_hum(df_oda)
#'
#' # Ensure that the df_oda data frame is available before running this function,
#' # using the `read_oda()` function
add_cols_hum <- function(df_oda) {

  # Check that the input data contain ODA data only and give warning if other types flows are also included.
  if (any(df_oda$type_of_flow != "ODA")) {
    warning("Attention: The data frame should contain observations where 'type_of_flow' is 'ODA' only.
    Please ensure you have used `read_oda()` and not `read_statsys()`.")
  }

  # Import imputed multi sector shares from DuckDB and filter for hum shares
  df_multi_hum <- read_imputed_multi_sector_shares() |> 
    filter(sector == "Humanitarian") |> 
    rename(imputed_multi_hum_share = share) |> 
    select(agreement_partner, year, imputed_multi_hum_share)
  
  # Join the imputed_multi_hum_share column to oda data
  df_oda <- df_oda |> 
    left_join(df_multi_hum, join_by(agreement_partner == agreement_partner, year == year))
  
# Logical variable to identify hum activities
df_oda <- df_oda |>
  mutate(
    hum_tag = case_when(
      # DAC main sector 720, 730, 740
      dac_main_sector_code %in% c(720, 730, 740) ~ TRUE,
   
      # Hum-specific imputed core contributions to multilateral partners (This should be improved)
      type_of_assistance == "Core contributions to multilat" & 
        agreement_partner %in% df_multi_hum$agreement_partner &
        year %in% df_multi_hum$year ~ TRUE,
      
      # Other activities (FALSE)
      .default = FALSE
    )
  )

# Numeric variable of hum_nok. Return zeros instead of NAs for core contributions with missing hum share.
  df_oda <- df_oda |> 
    mutate(
      # Earmarked oda to hum
      hum_nok = case_when(
        hum_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ disbursed_nok,
      # Imputed multilateral to hum
        hum_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ disbursed_nok * coalesce(imputed_multi_hum_share, 0),
        .default = 0
      )
    )

# Categorical variables
df_oda <- df_oda |>
  mutate(
    # Categorical variable for channel of hum ODA: earmarked and imputed multilateral
    hum_channel = case_when(
      hum_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ "Earmarked ODA to hum",
      hum_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ "Imputed multilateral ODA to hum",
      .default = NA
    )
  )

# Relocate columns
df_oda <- df_oda |> 
  relocate(imputed_multi_hum_share, .after = last_col())

  # Return the final dataframe with the additional hum columns
  return(df_oda)
}
