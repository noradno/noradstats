#' Anonymization rules for aid results publication
#'
#' Returns stable parameters used to derive anonymisation decisions and to apply
#' consistent masking across ODA and PTA datasets.
#'
#' @return A named list with anonymisation parameters.
#' @export
anonymization_rules <- function() {
  list(
    anonymized_text = "Temporarily anonymised",
    recipient_countries = c("Afghanistan"),
    partner_groups = c("NGO Local", "Private sector in developing countries", "Consultants"),
    oda_min_year = 2000
  )
}
