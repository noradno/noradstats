#' Add public climate oda columns to an existing statsys data frame
#'
#' This function takes an existing statsys data frame as input and adds public climate ODA columns.
#' Earmarked climate ODA is calculated using the Rio Markers for Climate Change adaptation and mitigation, applying a 40 percent coefficient for activities with only a significant climate change objective(s).
#' Imputed multilateral climate ODA is calculated using the OECD imputed multilateral shares, along with Norad's temporary estimates for the most recent year(s).
#' Imputed ordinary Norfund climate ODA (2 year averages) is calculated by Norad using the imputed multilateral shares methodology.
#' All amounts are expressed in net disbursements.
#' The function returns the statsys data frame with the following additional columns for climate ODA:
#' \describe{
#'   \item{\code{climate_oda_nok}}{Numeric variable of total public climate ODA (earmarked incl imputed ordinary Norfund and imputed multilateral).}
#'   \item{\code{climate_oda_tag}}{Logical variable to identify climate ODA activities, mening Rio-marked activites (regardless of disbursements) and positive imputed multilateral or imputed ordinary Norfund climate shares.}
#'   \item{\code{climate_oda_channel}}{Categorical variable to separate earmarked from imputed multilateral climate ODA}
#'   \item{\code{climate_oda_channel2}}{Categorical variable to separate earmarked (ex. capitalisation of ordinary Norfund), imputed ordinary Norfund ODA, and imputed multilateral ODA}
#'   \item{\code{climate_oda_type_of_support_3levels}}{Categorical variable of type of support (UNFCCC levels): adaptation only, mitigation only, and cross-cutting. Only useful for \code{climate_oda_nok}, not for \code{climate_adaptation_oda_earmarked_nok} and \code{climate_mitigation_oda_earmarked_nok}.}
#'   \item{\code{climate_oda_finance_earmarked_nok}}{Numeric variable of earmarked ODA for climate adaptation. Note that many of the activities may also be cross-cutting, also aimed at climate change mitigation.}
#'   \item{\code{climate_oda_finance_earmarked_nok}}{Numeric variable of earmarked ODA for climate mitigation, including imputed ordinary Norfund. Note that many of the activities may also be cross-cutting, also aimed at climate change adaptation.}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the ODA data frame by using
#' \code{noradstats::read_oda()}.
#' 
#' @import dplyr
#' @importFrom noradstats read_imputed_multi_shares
#' @param df_statsys An ODA data frame, which must already be loaded into the environment.
#' @return An ODA data frame with an additional `climate_oda_nok` column.
#' @export
#'
#' @examples
#' # Load the ODA data
#' df_oda <- read_oda()
#'
#' # Add the climate_oda_nok columns to the df_oda data frame
#' df_oda_climate_oda <- add_cols_climate_oda(df_oda)
#'
#' # Ensure that the df_oda data frame is available before running this function,
#' # using the `read_oda()` function
add_cols_climate_oda <- function(df_oda) {

  # Check if the data contains any "OOF" (Other Official Flows) values, as only "ODA" data should be present
  if (any(df_oda$type_of_flow == "OOF")) {
    stop("Error: The data frame should only contain ODA data, not 'OOF'.
    Please ensure you have used `read_oda()` and not `read_statsys()`.")
  }
 
  # Import imputed multi shares from DuckDB and filter for climate shares
  df_multi_climate <- read_imputed_multi_shares() |> 
    filter(marker == "Climate") |> 
    rename(multi_climate_share = share) |> 
    select(agreement_partner, year, multi_climate_share)
 
  # Join the imputed multi climate_share column to df_oda data
  df_oda <- df_oda |> 
    left_join(df_multi_climate, join_by(agreement_partner == agreement_partner, year == year))

  # Import imputed norfund climate shares
  df_imputed_norfund_climate_shares <- read_imputed_norfund_climate_shares()

  # Join the imputed norfund climate_shares column to df_oda
  df_oda <- df_oda |> 
    left_join(df_imputed_norfund_climate_shares, join_by(agreement_number == agreement_number, year == year))
  
  # Calculate climate ODA using methodologies for climate-specific earmarked contributions (Rio Markers),
  # climate-specific imputed norfund climate share and imputed multilateral climate share.
  # Note: Capitalisation of Norfunds climate investment mandate (CIM) is tagged with CMM and therefore correctly included as earmarked support.
  # Return zeros instead of NAs for core contributions with missing climate share.
  df_oda <- df_oda |> 
    mutate(
      # Climate ODA (using Riomarkers, imputed ordinary Norfund and imputed multilateral)
      climate_oda_nok = case_when(
        # Earmarked support using Rio Markers
        (pm_climate_change_adaptation == "Main objective" | pm_climate_change_mitigation == "Main objective") ~ disbursed_nok,
        (pm_climate_change_adaptation == "Significant objective" | pm_climate_change_mitigation == "Significant objective") ~ disbursed_nok * 0.4,
        # Imputed climate ordinary Norfund
        agreement_number %in% df_imputed_norfund_climate_shares$agreement_number ~ disbursed_nok * coalesce(norfund_climate_share, 0),
        # Imputed climate multilateral
        type_of_assistance == "Core contributions to multilat" & agreement_partner %in% df_multi_climate$agreement_partner ~ disbursed_nok * coalesce(multi_climate_share, 0),
        .default = 0
      ),
      # Earmarked adaptation ODA
      climate_adaptation_oda_earmarked_nok = case_when(
        pm_climate_change_adaptation == "Main objective" ~ disbursed_nok,
        pm_climate_change_adaptation == "Significant objective" ~ disbursed_nok * 0.4,
        .default = 0
      ),
      # Earmarked mitigation ODA (incl. imputed ordinary Norfund as a whole)
      climate_mitigation_oda_earmarked_nok = case_when(
        pm_climate_change_mitigation == "Main objective" & is.na(norfund_climate_share) ~ disbursed_nok,
        pm_climate_change_mitigation == "Significant objective" & is.na(norfund_climate_share) ~ disbursed_nok * 0.4,
        agreement_number %in% df_imputed_norfund_climate_shares$agreement_number ~ disbursed_nok * coalesce(norfund_climate_share, 0),
        .default = 0
      )
    )

  # Categorical variables
  # Logical variable to identify valid years of climate ODA activities
  df_oda <- df_oda |> 
    mutate(
    # Logical variable to identify climate relevant activities qualitatively, meaning Rio-marked activites (regardless of amounts extended) and positive multilateral or norfund climate shares
    climate_oda_tag = (
      pm_climate_change_adaptation != "None" |
      pm_climate_change_mitigation != "None" |
      (type_of_assistance == "Core contributions to multilat" & agreement_partner %in% df_multi_climate$agreement_partner) |
      agreement_number %in% df_imputed_norfund_climate_shares$agreement_number),

    # Categorical variable for channel of climate finance: earmarked and imputed multilateral
    climate_oda_channel = case_when(
      type_of_assistance == "Core contributions to multilat" & climate_oda_tag ~ "Imputed multilateral climate ODA",
      type_of_assistance != "Core contributions to multilat" & climate_oda_tag ~ "Earmarked climate ODA",
      .default = NA
    ),

    # Categorical variable of channel of climate finance: earmarked (ex. Norfund), Norfund and imputed multilateral
    climate_oda_channel2 = case_when(
      type_of_assistance == "Core contributions to multilat" & climate_oda_tag ~ "Imputed multilateral climate ODA",
      agreement_number %in% df_imputed_norfund_climate_shares$agreement_number & climate_oda_tag ~ "Imputed ordinary Norfund climate ODA",
      type_of_assistance != "Core contributions to multilat" & !agreement_number %in% df_imputed_norfund_climate_shares$agreement_number & climate_oda_tag ~ "Earmarked climate ODA excluding ordinary Norfund",
      .default = NA
    ),

    # Categorical variable of type of support (UNFCCC-levels)
    climate_oda_type_of_support_3levels = case_when(
      pm_climate_change_adaptation != "None" & pm_climate_change_mitigation == "None" ~ "Adaptation only",
      pm_climate_change_adaptation == "None" & pm_climate_change_mitigation != "None" ~ "Mitigation only",
      pm_climate_change_adaptation != "None" & pm_climate_change_mitigation != "None" ~ "Cross-cutting",
      .default = NA
    )
  )
  
  # Remove the columns climate_shares and norfund_climate_share
  df_oda <- df_oda |> 
    select(-c(multi_climate_share, norfund_climate_share))

  # Return the final dataframe with the additional climate finance column
  return(df_oda)
}
