#' Add public climate finance columns to an existing statsys data frame
#'
#' This function takes an existing statsys data frame as input and adds public climate finance columns using the UNFCCC methodology.
#' Earmarked climate finance is calculated using the Rio Markers for Climate Change adaptation and mitigation, applying a 40 percent coefficient for activities with only a significant climate change objective(s).
#' Imputed multilateral climate finance is calculated using the OECD imputed multilateral shares, along with Norad's temporary estimates for the most recent year(s).
#' All amounts are expressed in gross disbursements, and any negative gross disbursements in the statsys database are set to 0.
#' Note: Climate finance to all developing countries is included, including countries that are not non-Annex 1 parties (such as Ukraine), and should therefore be excluded when reporting to the UNFCCC.
#' The function returns the statsys data frame with the following additional columns for climate finance:
#' \describe{
#'   \item{\code{climate_finance_nok}}{Numeric variable of total public climate finance (earmarked and imputed multilateral).}
#'   \item{\code{climate_finance_tag}}{Logical variable to identify climate finance activities, mening Rio-marked activites (regardless of amounts extended) and positive climate shares.}
#'   \item{\code{climate_finance_channel}}{Categorical variable to separate earmarked from imputed multilateral climate finance.}
#'   \item{\code{climate_finance_channel2}}{Categorical variable to separate earmarked (ex. Norfund/KIF), Norfund/KIF, and imputed multilateral climate finance.}
#'   \item{\code{climate_finance_type_of_support_3levels}}{Categorical variable of type of support (UNFCCC levels): adaptation only, mitigation only, and cross-cutting. Only useful for \code{climate_finance_nok}, not for \code{climate_adaptation_finance_earmarked_nok} and \code{climate_mitigation_finance_earmarked_nok}.}
#'   \item{\code{climate_adaptation_finance_earmarked_nok}}{Numeric variable of earmarked climate adaptation finance. Note that many of the activities may also be cross-cutting, also aimed at climate change mitigation.}
#'   \item{\code{climate_mitigation_finance_earmarked_nok}}{Numeric variable of earmarked climate mitigation finance. Note that many of the activities may also be cross-cutting, also aimed at climate change adaptation.}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the statsys data frame by using
#' \code{noradstats::read_statsys()}.
#' 
#' @import dplyr
#' @importFrom noradstats read_imputed_multi_shares
#' @param df_statsys A statsys data frame, which must already be loaded into the environment.
#' @return A statsys data frame with an additional `climate_finance_nok` column.
#' @export
#'
#' @examples
#' # Load the statsys data
#' df_statsys <- read_statsys()
#'
#' # Add the climate_finance_nok column to the df_statsys data
#' df_statsys_climate_finance <- add_cols_climate_finance(df_statsys)
#'
#' # Ensure that the df_statsys data frame is available before running this function,
#' # using the `read_statsys()` function
add_cols_climate_finance <- function(df_statsys) {

  # Check if the data contains any "OOF" (Other Official Flows) values, and not only "ODA" values,
  # to ensure the correct dataset is being used.
  if (!any(df_statsys$type_of_flow == "OOF")) {
    stop("Error: The data frame must contain observations where 'type_of_flow' is 'OOF', not only 'ODA'.
    Please ensure you have used `read_statsys()` and not `read_oda()`.")
  }

  # Adjust amounts_extended in the provided statsys data frame
  df_statsys <- df_statsys |> 
    mutate(
      amounts_extended = if_else(amounts_extended_1000_nok < 0, 0, amounts_extended_1000_nok * 1e3)
    )
  
  # Import imputed multi shares from DuckDB and filter for climate shares
  df_multi_climate <- read_imputed_multi_shares() |> 
    filter(marker == "Climate") |> 
    rename(climate_share = share) |> 
    select(agreement_partner, year, climate_share)
  
  # Join the climate_share column to statsys data
  df_statsys <- df_statsys |> 
    left_join(df_multi_climate, join_by(agreement_partner == agreement_partner, year == year))
  
  # Calculate climate finance using methodologies for climate-specific earmarked contributions (Rio Markers)
  # and climate-specific multilateral core contributions to multilateral organisations.
  # Exclude Norfund to avoid double counting of the Rio-marked capitalisation of Norfund's Climate Investment Fund.
  # Return zeros instead of NAs for core contributions with missing climate share.
  df_statsys <- df_statsys |> 
    mutate(
      # Climate finance (both earmarked and imputed multilateral)
      climate_finance_nok = case_when(
        (pm_climate_change_adaptation == "Main objective" | pm_climate_change_mitigation == "Main objective") & agreement_partner != "Norfund" ~ amounts_extended,
        (pm_climate_change_adaptation == "Significant objective" | pm_climate_change_mitigation == "Significant objective") & agreement_partner != "Norfund" ~ amounts_extended * 0.4,
        type_of_assistance == "Core contributions to multilat" & agreement_partner != "Norfund" ~ amounts_extended * coalesce(climate_share, 0),
        .default = 0
      ),
      # Earmarked adaptation finance
      climate_adaptation_finance_earmarked_nok = case_when(
        pm_climate_change_adaptation == "Main objective" & agreement_partner != "Norfund" ~ amounts_extended,
        pm_climate_change_adaptation == "Significant objective" & agreement_partner != "Norfund" ~ amounts_extended * 0.4,
        .default = 0
      ),
      # Earmarked mitigation finance
      climate_mitigation_finance_earmarked_nok = case_when(
        pm_climate_change_mitigation == "Main objective" & agreement_partner != "Norfund" ~ amounts_extended,
        pm_climate_change_mitigation == "Significant objective" & agreement_partner != "Norfund" ~ amounts_extended * 0.4,
        .default = 0
      )
    )

  # Categorical variables
  # Logical variable to identify valid years of climate finance activities
  df_statsys <- df_statsys |> 
    mutate(
    # Logical variable to identify climate relevant activities qualitatively, mening Rio-marked activites (regardless of amounts extended) and positive climate shares.
    climate_finance_tag = (pm_climate_change_adaptation != "None" | pm_climate_change_mitigation != "None" | (!is.na(climate_share) & climate_share != 0)),

    # Categorical variable for channel of climate finance: earmarked and imputed multilateral
    climate_finance_channel = case_when(
      type_of_assistance == "Core contributions to multilat" & climate_finance_tag ~ "Imputed multilateral climate finance",
      type_of_assistance != "Core contributions to multilat" & climate_finance_tag ~ "Earmarked climate finance",
      .default = NA
    ),

    # Categorical variable of channel of climate finance: earmarked (ex. Norfund), Norfund and imputed multilateral
    climate_finance_channel2 = case_when(
      type_of_assistance == "Core contributions to multilat" & climate_finance_tag ~ "Imputed multilateral climate finance",
      extending_agency == "Norfund" & climate_finance_tag ~ "Earmarked climate finance from Norfund/KIF",
      type_of_assistance != "Core contributions to multilat" & extending_agency != "Norfund" & climate_finance_tag ~ "Earmarked climate finance excluding Norfund/KIF",
      .default = NA
    ),

    # Categorical variable of type of support (UNFCCC-levels)
    climate_finance_type_of_support_3levels = case_when(
      pm_climate_change_adaptation != "None" & pm_climate_change_mitigation == "None" ~ "Adaptation only",
      pm_climate_change_adaptation == "None" & pm_climate_change_mitigation != "None" ~ "Mitigation only",
      pm_climate_change_adaptation != "None" & pm_climate_change_mitigation != "None" ~ "Cross-cutting",
      .default = NA
    )
  )
  
  # Remove the columns amounts_extended and climate_share
  df_statsys <- df_statsys |> 
    select(-c(amounts_extended, climate_share))

  # Return the final dataframe with the additional climate finance column
  return(df_statsys)
}