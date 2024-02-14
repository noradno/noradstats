#' Add climate columns to dataframe
#'
#' @param data Input dataframe of Norwegian development assistance
#'
#' @return Returns dataframe with additional climate-relevant columns:
#' \itemize{
#'   \item climate_aid_type: Type of climate aid using UNFCCC classification: adaptation, climate change mitigation, cross-cutting, none.
#'   \item climate_aid_nok_mill: Estimated earmarked climate aid in million NOK (40 percent of significant objective).
#'   \item climate_adaptation_nok_mill: Estimated earmarked aid targeting adaptation (40 percent of significant objective).
#'   \item climate_mitigation_nok_mill: Estimated earmarked aid targeting mitigation (40 percent of significant objective).
#'   \item climate_mitigation_nok_mill_gross_fix: Handy for calculating the climate share of Norfund's investements. Estimated earmarked amounts extended targeting mitigation (negative extended amounts are excluded).
#'   \item climate_aid_nok_gross_fix: For reporting of climate finance the UNFCCC. Gross disbursements (negative extended amounts are excluded).
#' }
#' @export
#
#' @importFrom rlang .data
#'
#' @examples
#' ?add_cols_climate_clean()
#'
add_cols_climate_clean <- function(data) {

  data %>%

    # column climate_aid_type

    dplyr::mutate(climate_aid_type = dplyr::case_when(
      .data$pm_climate_change_adaptation!='None' & .data$pm_climate_change_mitigation=='None' ~ 'Adaptation',
      .data$pm_climate_change_mitigation!='None' & .data$pm_climate_change_adaptation=='None' ~ 'Mitigation',
      .data$pm_climate_change_adaptation!='None' & .data$pm_climate_change_mitigation!='None' ~ 'Cross-cutting',
      TRUE ~ 'None')) %>%

    dplyr::mutate(climate_aid_type = forcats::fct_relevel(.data$climate_aid_type,"Adaptation", "Mitigation", "Cross-cutting", "None")) %>%

    # column climate_aid_nok_mill

    dplyr::mutate(climate_aid_nok_mill = dplyr::case_when(
      .data$pm_climate_change_adaptation == "Main objective" | .data$pm_climate_change_mitigation == "Main objective" ~ .data$disbursed_mill_nok,
      .data$pm_climate_change_adaptation == "Significant objective" | .data$pm_climate_change_mitigation == 'Significant objective' ~ .data$disbursed_mill_nok * 0.4,
      TRUE ~ as.numeric(0))) %>%

    # column climate_adaptation_aid_nok_mill

    dplyr::mutate(climate_adaptation_nok_mill = dplyr::case_when(
      .data$pm_climate_change_adaptation == "Main objective" ~ .data$disbursed_mill_nok,
      .data$pm_climate_change_adaptation == "Significant objective" ~ .data$disbursed_mill_nok * 0.4,
      TRUE ~ as.numeric(0))) %>%

    # column climate_mitigation_nok_mill

    dplyr::mutate(climate_mitigation_nok_mill = dplyr::case_when(
      .data$pm_climate_change_mitigation == "Main objective" ~ .data$disbursed_mill_nok,
      .data$pm_climate_change_mitigation == "Significant objective" ~ .data$disbursed_mill_nok * 0.4,
      TRUE ~ as.numeric(0))) %>%

    # column climate_mitigation_nok_mill_gross_fix

    dplyr::mutate(climate_mitigation_nok_mill_gross_fix = dplyr::case_when(
      .data$pm_climate_change_mitigation == "Main objective" ~
        dplyr::if_else(.data$amounts_extended_1000_nok < 0, 0, .data$amounts_extended_1000_nok / 1000),
      .data$pm_climate_change_mitigation == "Significant objective" ~
        dplyr::if_else(.data$amounts_extended_1000_nok < 0, 0, .data$amounts_extended_1000_nok / 1000) * 0.4,
      TRUE ~ as.numeric(0))) %>%
    
    # column climate_adaptation_nok_mill_gross_fix
    
    dplyr::mutate(climate_adaptation_nok_mill_gross_fix = dplyr::case_when(
      .data$pm_climate_change_adaptation == "Main objective" ~
        dplyr::if_else(.data$amounts_extended_1000_nok < 0, 0, .data$amounts_extended_1000_nok / 1000),
      .data$pm_climate_change_adaptation == "Significant objective" ~
        dplyr::if_else(.data$amounts_extended_1000_nok < 0, 0, .data$amounts_extended_1000_nok / 1000) * 0.4,
      TRUE ~ as.numeric(0))) %>%
    
    # column climate_finance_nok_gross_fix
    
    dplyr::mutate(climate_finance_nok_gross_fix = dplyr::case_when(
      .data$pm_climate_change_adaptation == "Main objective" | .data$pm_climate_change_mitigation == "Main objective" ~
        dplyr::if_else(.data$amounts_extended_1000_nok < 0, 0, .data$amounts_extended_1000_nok * 1000),
      .data$pm_climate_change_adaptation == "Significant objective" | .data$pm_climate_change_mitigation == 'Significant objective' ~
        dplyr::if_else(.data$amounts_extended_1000_nok < 0, 0, .data$amounts_extended_1000_nok * 1000) * 0.4,
      TRUE ~ as.numeric(0)))
}
