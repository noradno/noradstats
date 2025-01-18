#' Add food columns to an existing oda data frame
#'
#' This function takes an existing oda data frame as input and adds food columns.
#' The function returns the oda data frame with the following additional columns for food:
#' \describe{
#'   \item{\code{food_nok}}{Numeric variable of total disbursed food oda (earmarked and multilateral).}
#'   \item{\code{food_tag}}{Logical variable to identify food activities, meaning earmarked and multilateral (long-term and emergency)}
#'   \item{\code{food_oda_channel_highlevel}}{Categorical variable to separate earmarked oda to food from  multilateral oda to food.}
#'   \item{\code{food_oda_channel_lowlevel}}{Categorical variable to separate earmarked and multilateral and long-term and emergency.}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the oda data frame by using
#' \code{noradstats::read_oda()}.
#' 
#' @import dplyr
#' @param df_oda A oda data frame, which must already be loaded into the environment.
#' @return A oda data frame with additional food columns.
#' @export
#'
#' @examples
#' # Load the oda data
#' df_oda <- read_oda()
#'
#' # Add food column to the df_oda data
#' df_oda_food <- add_cols_food(df_oda)
#'
#' # Ensure that the df_oda data frame is available before running this function,
#' # using the `read_oda()` function
add_cols_food <- function(df_oda) {

  # Check if the data contains any "OOF" (Other Official Flows) values, and not only "ODA" values,
  # to ensure the correct dataset is being used.
  if (!any(df_oda$type_of_flow == "ODA")) {
    stop("Error: The data frame must contain observations where 'type_of_flow' is 'ODA' only.
    Please ensure you have used `read_oda()` and not `read_statsys()`.")
  }

# Categorical variables
df_oda <- df_oda |>
  mutate(
    # Channel low-level
    food_channel_lowlevel = case_when(
      # Long-term earmarked ODA
      dac_main_sector_code %in% c(311, 313) ~ "Long-term earmarked food ODA",
      dac_main_sector_code == 430 & dac_sub_sector_code %in% c(71, 72, 73) ~ "Long-term earmarked food ODA",
      dac_main_sector_code == 520 & dac_sub_sector_code == 10 ~ "Long-term earmarked food ODA",
      agreement_partner == "Global Crop Diversity Trust" ~ "Long-term earmarked food ODA",
      
      # Long-term multilateral ODA
      type_of_assistance == "Core contributions to multilat" &
        agreement_partner %in% c(
          "CGIAR - Consultative Group on International Agricultural Research",
          "FAO - Food and Agricultural Organization of the United Nations",
          "IFAD - International Fund for Agricultural Development"
        ) ~ "Long-term multilateral food ODA",
      
      # Emergency earmarked food ODA
      dac_main_sector_code == 720 & dac_sub_sector_code == 40 ~ "Emergency earmarked food ODA",
      
      # Emergency multilateral food ODA
      type_of_assistance == "Core contributions to multilat" &
        agreement_partner == "WFP - World Food Programme" ~ "Emergency multilateral food ODA",
      
      # Other activities (FALSE)
      .default = NA
    ),

    # Channel high-level
    food_channel_highlevel = case_when(
      food_channel_lowlevel %in% c("Long-term earmarked food ODA", "Emergency earmarked food ODA") ~ "Earmarked food ODA",
      food_channel_lowlevel %in% c("Long-term multilateral food ODA", "Emergency multilateral food ODA") ~ "Multilateral food ODA",
      .default = NA
    ),

    # Tag to identify food ODA (logical)
    food_tag = !is.na(food_channel_highlevel)
  )

# Numeric variable of food_nok. Same as disbursed_nok, but return zeros instead of NAs.
  df_oda <- df_oda |> 
    mutate(food_nok = if_else(food_tag == TRUE, disbursed_nok, 0))

  # Return the final dataframe with the additional food columns
  return(df_oda)
}
