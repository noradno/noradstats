#' Add education columns to an existing oda data frame
#'
#' This function takes an existing oda data frame as input and adds education columns.
#' The function returns the oda data frame with the following additional columns for education:
#' \describe{
#'   \item{\code{education_nok}}{Numeric variable of total disbursed education oda (earmarked and imputed multilateral).}
#'   \item{\code{education_tag}}{Logical variable to identify education activities, meaning earmarked ODA for DAC sectors of long term and emergency aid and imputed multilateral ODA for education (positive imputed multilateral education shares)}
#'   \item{\code{education_oda_channel}}{Categorical variable to separate earmarked oda to education from imputed multilateral oda to education.}
#'   \item{\code{education_oda_channel2}}{Categorical variable to separate earmarked humanitarian oda to education, long term oda to education, and imputed multilateral to education.}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the statsys data frame by using
#' \code{noradstats::read_oda()}.
#' 
#' @import dplyr
#' @importFrom noradstats read_imputed_multi_sector_shares
#' @param df_oda A statsys data frame, which must already be loaded into the environment.
#' @return A statsys data frame with additional education columns.
#' @export
#'
#' @examples
#' # Load the statsys data
#' df_oda <- read_oda()
#'
#' # Add education column to the df_oda data
#' df_oda_education <- add_cols_education(df_oda)
#'
#' # Ensure that the df_oda data frame is available before running this function,
#' # using the `read_oda()` function
add_cols_education <- function(df_oda) {

  # Check if the data contains any "OOF" (Other Official Flows) values, and not only "ODA" values,
  # to ensure the correct dataset is being used.
  if (!any(df_oda$type_of_flow == "ODA")) {
    stop("Error: The data frame must contain observations where 'type_of_flow' is 'ODA' only.
    Please ensure you have used `read_oda()` and not `read_statsys()`.")
  }

  # Import imputed multi sector shares from DuckDB and filter for education shares
  df_multi_education <- read_imputed_multi_sector_shares() |> 
    filter(sector == "Education") |> 
    rename(imputed_multi_education_share = share) |> 
    select(agreement_partner, year, imputed_multi_education_share)
  
  # Join the imputed_multi_education_share column to oda data
  df_oda <- df_oda |> 
    left_join(df_multi_education, join_by(agreement_partner == agreement_partner, year == year))
  
# Logical variable to identify education activities
df_oda <- df_oda |>
  mutate(
    education_tag = case_when(
      # DAC main sector 111-114
      dac_main_sector_code %in% c(111, 112, 113, 114) ~ TRUE,

      # DAC sub sector 720.12
      dac_main_sector_code == 720 & dac_sub_sector_code == 12 ~ TRUE,
      
      # Education-specific imputed core contributions to multilateral partners (This should be improved)
      type_of_assistance == "Core contributions to multilat" & 
        agreement_partner %in% df_multi_education$agreement_partner &
        year %in% df_multi_education$year ~ TRUE,
      
      # Other activities (FALSE)
      .default = FALSE
    )
  )

# Numeric variable of education_nok. Return zeros instead of NAs for core contributions with missing education share.
  df_oda <- df_oda |> 
    mutate(
      # Earmarked oda to education
      education_nok = case_when(
        education_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ disbursed_nok,
      # Imputed multilateral to education
        education_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ disbursed_nok * coalesce(imputed_multi_education_share, 0),
        .default = 0
      )
    )

# Categorical variables
df_oda <- df_oda |>
  mutate(
    # Categorical variable for channel of education ODA: earmarked and imputed multilateral
    education_channel = case_when(
      education_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ "Earmarked ODA to education",
      education_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ "Imputed multilateral ODA to education",
      .default = NA
    ),

    # Categorical variable of channel of education oda: earmarked long term and humanitarian, and imputed multilateral
    education_channel2 = case_when(
      education_tag == TRUE & type_of_assistance != "Core contributions to multilat" &
        dac_main_sector_code %in% c(111, 112, 113, 114) ~ "Earmarked long-term ODA to education",
      education_tag == TRUE & type_of_assistance != "Core contributions to multilat" &
        dac_main_sector_code == 720 & dac_sub_sector_code == 12 ~ "Earmarked humanitarian ODA to education",
      education_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ "Imputed multilateral ODA to education",
      .default = NA
    )
  )

# Relocate columns
df_oda <- df_oda |> 
  relocate(imputed_multi_education_share, .after = last_col())

  # Return the final dataframe with the additional education columns
  return(df_oda)
}
