#' Read PTA Agreement totals report into R from user-specified CSV file
#'
#' Reads and processes the PTA Agreement totals report to produce a cleaned
#' dataset of agreement-level metadata (title, partner, period, and expected totals).
#'
#' @param path Path to the PTA Agreement totals CSV file
#' @return A cleaned data frame with one row per agreement
read_pta_agreement_totals_from_csv <- function(path) {
  if (!file.exists(path)) {
    stop("File Agreement totals.csv does not exist")
  }
  
  df <- readr::read_csv2(
    file = path,
    skip = 13,
    name_repair = janitor::make_clean_names,
    locale = readr::locale(decimal_mark = ",", grouping_mark = " ", encoding = "UTF-8")
  ) |>
    dplyr::select(-dplyr::starts_with("x")) |>  # Drop empty columns
    head(-2)
  
  df <- df |>
    dplyr::select(
      agreement_no,
      agreement_title,
      agreement_partner,
      agr_period,
      expected_agreement_total
    ) |>
    dplyr::mutate(
      # Convert thousands to NOK
      expected_agreement_total = expected_agreement_total * 1000,
      # Extract from/to years from agr_period column
      agreement_period_from = stringr::str_sub(agr_period, end = 4),
      agreement_period_to = stringr::str_sub(agr_period, start = -4)
    ) |>
    dplyr::rename(agreement_period = agr_period) |>
    dplyr::relocate(expected_agreement_total, .after = dplyr::last_col())
  
  return(df)
}
