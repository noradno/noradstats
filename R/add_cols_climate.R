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
#' }
#' @export
#
#' @importFrom rlang .data
#'
#' @examples
#' ?add_cols_climate()
#'
add_cols_climate <- function(data) {

  data <- data %>%

    # column climate_aid_type

    dplyr::mutate("climate_aid_type" = dplyr::case_when(
      `PM - Climate change adaptation`!='None' & `PM - Climate change mitigation`=='None' ~ 'Adaptation',
      `PM - Climate change mitigation`!='None' & `PM - Climate change adaptation`=='None' ~ 'Mitigation',
      `PM - Climate change adaptation`!='None' & `PM - Climate change mitigation`!='None' ~ 'Cross-cutting',
      TRUE ~ 'None')) %>%

    dplyr::mutate("climate_aid_type" = forcats::fct_relevel(.data$climate_aid_type,"Adaptation", "Mitigation", "Cross-cutting", "None")) %>%

    # column climate_aid_nok_mill

    dplyr::mutate("climate_aid_nok_mill" = dplyr::case_when(
      `PM - Climate change adaptation` == "Main objective" | `PM - Climate change mitigation` == "Main objective" ~ `Disbursed (mill NOK)`,
      `PM - Climate change adaptation` == "Significant objective" | `PM - Climate change mitigation` == 'Significant objective' ~ `Disbursed (mill NOK)` * 0.4,
      TRUE ~ as.numeric(0))) %>%

    # column climate_adaptation_aid_nok_mill

    dplyr::mutate("climate_adaptation_nok_mill" = dplyr::case_when(
      `PM - Climate change adaptation` == "Main objective" ~ `Disbursed (mill NOK)`,
      `PM - Climate change adaptation` == "Significant objective" ~ `Disbursed (mill NOK)` * 0.4,
      TRUE ~ as.numeric(0))) %>%

    # column climate_mitigation_nok_mill

    dplyr::mutate("climate_mitigation_nok_mill" = dplyr::case_when(
      `PM - Climate change mitigation` == "Main objective" ~ `Disbursed (mill NOK)`,
      `PM - Climate change mitigation` == "Significant objective" ~ `Disbursed (mill NOK)` * 0.4,
      TRUE ~ as.numeric(0))) %>%

    # column climate_mitigation_nok_mill_gross_fix

    dplyr::mutate("climate_mitigation_nok_mill_gross_fix" = dplyr::case_when(
      `PM - Climate change mitigation` == "Main objective" ~
        dplyr::if_else(`Amounts extended (1000 NOK)` < 0, 0, `Amounts extended (1000 NOK)` / 1000),
      `PM - Climate change mitigation` == "Significant objective" ~
        dplyr::if_else(`Amounts extended (1000 NOK)` < 0, 0, `Amounts extended (1000 NOK)` / 1000) * 0.4,
      TRUE ~ as.numeric(0)))
}
