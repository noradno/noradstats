#' Add public climate finance columns to an existing statsys data frame
#'
#' This function takes an existing statsys data frame as input and adds public climate finance columns using the UNFCCC methodology, including both Official Development Finance (ODA and OOF).
#' Earmarked climate finance is calculated using the Rio Markers for Climate Change adaptation and mitigation, applying a 40 percent coefficient for activities with only a significant climate change objective(s).
#' The Riomarked capitalisation(s) of Norfund Climate Investment Mandate/Climate Investment Fund is excluded to avoiding double counting, as the CIF investments are already included (OOF).
#' Imputed multilateral climate finance is calculated using the OECD imputed multilateral shares, along with Norad's temporary estimates for the most recent year(s).
#' All amounts are expressed in gross disbursements.
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

  # Adjust amounts_extended from 1000 NOK to NOK
  df_statsys <- df_statsys |> 
    mutate(amounts_extended = amounts_extended_1000_nok * 1e3)
  
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
  # Return zeros instead of NAs for core contributions with missing climate share.

  # Identifying (withouth hard-coding) the agreement number(s) of the riomarked capitalisation(s) of Norfund Climate Investment Mandate - to be excluded, to avoid double counting of riomarked capitalisation and outflows
  vec_cim_capitalisation_agr <- df_statsys |>
    filter(
      extending_agency == "UD - Oslo" & 
      agreement_partner == "Norfund" & 
      as.numeric(substr(agreement_signed, 1, 4)) >= 2022 & 
      type_of_flow == "ODA" & 
      (pm_climate_change_adaptation != "None" | pm_climate_change_mitigation != "None")
    ) |>
    distinct(agreement_number)
  
  df_statsys <- df_statsys |> 
    mutate(
      # Climate finance (ODA and OOF from both earmarked and imputed multilateral channels)
      climate_finance_nok = case_when(
        # Earmarked support using Rio Markers. Exluding the Riomarked capitalisation of Norfund CIM.
        (pm_climate_change_adaptation == "Main objective" | pm_climate_change_mitigation == "Main objective") & !agreement_number %in% vec_cim_capitalisation_agr ~ amounts_extended,
        (pm_climate_change_adaptation == "Significant objective" | pm_climate_change_mitigation == "Significant objective") & !agreement_number %in% vec_cim_capitalisation_agr ~ amounts_extended * 0.4,
        # Imputed climate multilateral
        type_of_assistance == "Core contributions to multilat" & agreement_partner %in% df_multi_climate$agreement_partner ~ amounts_extended * coalesce(climate_share, 0),
        .default = 0
      ),
      # Earmarked adaptation finance. Exluding the Riomarked capitalisation of Norfund CIM.
      climate_adaptation_finance_earmarked_nok = case_when(
        pm_climate_change_adaptation == "Main objective" & !agreement_number %in% vec_cim_capitalisation_agr ~ amounts_extended,
        pm_climate_change_adaptation == "Significant objective" & !agreement_number %in% vec_cim_capitalisation_agr ~ amounts_extended * 0.4,
        .default = 0
      ),
      # Earmarked mitigation finance. Exluding the Riomarked capitalisation of Norfund CIM.
      climate_mitigation_finance_earmarked_nok = case_when(
        pm_climate_change_mitigation == "Main objective" & !agreement_number %in% vec_cim_capitalisation_agr ~ amounts_extended,
        pm_climate_change_mitigation == "Significant objective" & !agreement_number %in% vec_cim_capitalisation_agr ~ amounts_extended * 0.4,
        .default = 0
      )
    )

  # Categorical variables
  # Logical variable to identify valid years of climate finance activities
  df_statsys <- df_statsys |> 
    mutate(
    # Logical variable to identify climate-relevant activities qualitatively, meaning Rio-marked activities and positive multilaeral climate shares. Exluding the Riomarked capitalisation of Norfund CIM.
    climate_finance_tag = (
      (pm_climate_change_adaptation != "None" & !agreement_number %in% vec_cim_capitalisation_agr) |
      (pm_climate_change_mitigation != "None" & !agreement_number %in% vec_cim_capitalisation_agr) |
      (type_of_assistance == "Core contributions to multilat" & agreement_partner %in% df_multi_climate$agreement_partner)
    ),

    # Categorical variable for channel of climate finance: earmarked and imputed multilateral
    climate_finance_channel = case_when(
      type_of_assistance == "Core contributions to multilat" & climate_finance_tag ~ "Imputed multilateral climate finance",
      type_of_assistance != "Core contributions to multilat" & climate_finance_tag ~ "Earmarked climate finance",
      .default = NA
    ),

    # Categorical variable of channel of climate finance: earmarked (ex. Norfund/KIF OOF), Norfund/KIF OOF, and imputed multilateral
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