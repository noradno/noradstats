#' Add srhr columns to an existing oda data frame
#'
#' This function takes an existing oda data frame as input and adds srhr columns.
#' The function returns the oda data frame with the following additional columns for srhr:
#' \describe{
#'   \item{\code{srhr_nok}}{Numeric variable of total disbursed srhr oda. Using hard-coded percentages for individual partners/sectors}
#'   \item{\code{srhr_tag}}{Logical variable to identify srhr activities, by identifying sectors, partners and agreements)}
#'   \item{\code{srhr_oda_channel_lowlevel}}{Categorical variable to separate the sectors and partners}
#'   \item{\code{srhr_oda_channel_highlevel}}{Categorical variable to separate earmarked ODA to SRHR and multilateral ODA to SRHR (estimate)}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the oda data frame by using
#' \code{noradstats::read_oda()}.
#' 
#' @import dplyr
#' @param df_oda A oda data frame, which must already be loaded into the environment.
#' @return A oda data frame with additional srhr columns.
#' @export
#'
#' @examples
#' # Load the oda data
#' df_oda <- read_oda()
#'
#' # Add srhr column to the df_oda data
#' df_oda_health <- add_cols_srhr(df_oda)
#'
#' # Ensure that the df_oda data frame is available before running this function,
#' # using the `read_oda()` function
add_cols_srhr <- function(df_oda) {

  # Check that the input data contain ODA data only and give warning if other types flows are also included.
  if (any(df_oda$type_of_flow != "ODA")) {
    warning("Attention: The data frame should contain observations where 'type_of_flow' is 'ODA' only.
        Please ensure you have used `read_oda()` and not `read_statsys()`.")
  }

  df_oda <- df_oda |>
    
  # Categorical variable for srhr lowlevel channels (meaning sectors/partners/agreements)
  mutate(
    srhr_channel_lowlevel = case_when(
      dac_main_sector_code_name == "130 - Population policies/programmes and reproductive health" ~ "130 - Population policies/programmes and reproductive health",
      type_of_assistance == "Core contributions to multilat" &
        agreement_partner == "UNAIDS - UN Programme on HIV/AIDS" ~ "Core support to UNAIDS (100 pct)",
      type_of_assistance == "Core contributions to multilat" &
        agreement_partner == "UNFPA - UN Population Fund" ~ "Core support to UNFPA (100 pct)",
      type_of_assistance == "Core contributions to multilat" &
        agreement_partner == "GFATM - Global Fund to Fight AIDS, Tuberculosis and Malaria" ~ "Core support to GFATM (50 pct)",
      agreement_number == "QZA-20/0303-1" ~ "Support to selected agreements with GEF (26 pct)",
      .default = NA
    ),

    # Tag to identify srhr ODA (logical)
    srhr_tag = !is.na(srhr_channel_lowlevel),

    # Categorical variable for srhr highlevel channels: earmarked and multilateral support
    srhr_channel_highlevel = case_when(
      srhr_tag == TRUE 
      & type_of_assistance == "Core contributions to multilat" ~ "Multilateral core ODA to SRHR (estimate)",
      srhr_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ "Earmarked ODA to SRHR",
      .default = NA
    ),

    # Numeric variable of srhr_nok. Using percentages on different partners, and return zeros instead of NAs.
    srhr_nok = case_when(
      srhr_channel_lowlevel %in% c(
        "130 - Population policies/programmes and reproductive health",
        "Core support to UNAIDS (100 pct)",
        "Core support to UNFPA (100 pct)"
      ) ~ disbursed_nok,
      srhr_channel_lowlevel == "Core support to GFATM (50 pct)" ~ disbursed_nok * 0.5,
      srhr_channel_lowlevel == "Support to selected agreements with GEF (26 pct)" ~ disbursed_nok * 0.26,
      .default = as.double(0)
    )
  )

  # Return the final dataframe with the additional srhr columns
  return(df_oda)
}




