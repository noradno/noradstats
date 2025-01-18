#' Add health columns to an existing oda data frame
#'
#' This function takes an existing oda data frame as input and adds health columns.
#' The function returns the oda data frame with the following additional columns for health:
#' \describe{
#'   \item{\code{health_nok}}{Numeric variable of total disbursed health oda (earmarked and imputed multilateral).}
#'   \item{\code{health_tag}}{Logical variable to identify health activities, meaning earmarked ODA for DAC sectors of long term and emergency aid and imputed multilateral ODA for health (positive imputed multilateral health shares)}
#'   \item{\code{health_oda_channel}}{Categorical variable to separate earmarked oda to health from imputed multilateral oda to health.}
#'   \item{\code{health_oda_channel2}}{Categorical variable to separate earmarked humanitarian oda to health, long term oda to health, and imputed multilateral to health.}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the statsys data frame by using
#' \code{noradstats::read_oda()}.
#' 
#' @import dplyr
#' @importFrom noradstats read_imputed_multi_sector_shares
#' @param df_oda A statsys data frame, which must already be loaded into the environment.
#' @return A statsys data frame with additional health columns.
#' @export
#'
#' @examples
#' # Load the statsys data
#' df_oda <- read_oda()
#'
#' # Add health column to the df_oda data
#' df_oda_health <- add_cols_health(df_oda)
#'
#' # Ensure that the df_oda data frame is available before running this function,
#' # using the `read_oda()` function
add_cols_health <- function(df_oda) {

  # Check if the data contains any "OOF" (Other Official Flows) values, and not only "ODA" values,
  # to ensure the correct dataset is being used.
  if (!any(df_oda$type_of_flow == "ODA")) {
    stop("Error: The data frame must contain observations where 'type_of_flow' is 'ODA' only.
    Please ensure you have used `read_oda()` and not `read_statsys()`.")
  }

  # Import imputed multi sector shares from DuckDB and filter for health shares
  df_multi_health <- read_imputed_multi_sector_shares() |> 
    filter(sector == "Health") |> 
    rename(imputed_multi_health_share = share) |> 
    select(agreement_partner, year, imputed_multi_health_share)
  
  # Join the imputed_multi_health_share column to oda data
  df_oda <- df_oda |> 
    left_join(df_multi_health, join_by(agreement_partner == agreement_partner, year == year))
  
# Logical variable to identify health activities
df_oda <- df_oda |>
  mutate(
    health_tag = case_when(
      # DAC main sector 121, 122, 123, 130
      dac_main_sector_code %in% c(121, 122, 123, 130) ~ TRUE,

      # DAC sub sector 720.11
      dac_main_sector_code == 720 & dac_sub_sector_code == 11 ~ TRUE,
      
      # Health-specific imputed core contributions to multilateral partners (This should be improved)
      type_of_assistance == "Core contributions to multilat" & 
        agreement_partner %in% df_multi_health$agreement_partner &
        year %in% df_multi_health$year ~ TRUE,
      
      # Other activities (FALSE)
      .default = FALSE
    )
  )

# Numeric variable of health_nok. Return zeros instead of NAs for core contributions with missing health share.
  df_oda <- df_oda |> 
    mutate(
      # Earmarked oda to health
      health_nok = case_when(
        health_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ disbursed_nok,
      # Imputed multilateral to health
        health_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ disbursed_nok * coalesce(imputed_multi_health_share, 0),
        .default = 0
      )
    )

# Categorical variables
df_oda <- df_oda |>
  mutate(
    # Categorical variable for channel of health ODA: earmarked and imputed multilateral
    health_channel = case_when(
      health_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ "Earmarked ODA to health",
      health_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ "Imputed multilateral ODA to health",
      .default = NA
    ),

    # Categorical variable of channel of health oda: earmarked long term and humanitarian, and imputed multilateral
    health_channel2 = case_when(
      health_tag == TRUE & type_of_assistance != "Core contributions to multilat" &
        dac_main_sector_code %in% c(121, 122, 123, 130) ~ "Earmarked long-term ODA to health",
      health_tag == TRUE & type_of_assistance != "Core contributions to multilat" &
        dac_main_sector_code == 720 & dac_sub_sector_code == 11 ~ "Earmarked humanitarian ODA to health",
      health_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ "Imputed multilateral ODA to health",
      .default = NA
    )
  )

# Relocate columns
df_oda <- df_oda |> 
  relocate(imputed_multi_health_share, .after = last_col())

  # Return the final dataframe with the additional health columns
  return(df_oda)
}
