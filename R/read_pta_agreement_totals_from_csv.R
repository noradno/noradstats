#' Read PTA Agreement totals report into R from user-specified CSV file
#'
#' Reads and processes the PTA Agreement totals report to produce a cleaned
#' dataset of agreement-level data.
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
    locale = readr::locale(decimal_mark = ",", grouping_mark = " ", encoding = "UTF-8")
  ) |>
    dplyr::select(-tidyselect::starts_with("...")) |>  # Drop empty columns
    head(-2)
  
  df <- df |>
    # Snake case names
    janitor::clean_names() |> 
    # Selecting columns
    dplyr::select(
      agreement_no,
      agreement_title,
      agreement_partner,
      agr_period,
      estimated_amount,
      agreed_original_amount,
      agreed_additional_grant,
      total_agreed,
      expected_agreement_total,
      disbursed,
      total_prognosis,
      not_planned
    ) |>
    dplyr::mutate(
      # Convert thousands to NOK
      dplyr::across(tidyselect::where(is.numeric), \(x) x * 1e3),
      # Cleaning the agreement period variable (extract the years, allow NA years from to, and create additional from and to variable)
      agr_period = {
        x <- stringr::str_squish(as.character(agr_period))
        x <- stringr::str_replace_all(x, "\\s*-\\s*", "-")
        
        from <- stringr::str_extract(x, "^\\d{4}")
        to   <- stringr::str_extract(x, "(?<=-)\\d{4}$")
        
        dplyr::if_else(
          !is.na(from) | !is.na(to),
          paste0(dplyr::coalesce(from, "NA"), "-", dplyr::coalesce(to, "NA")),
          NA_character_
        )
      },
      # Creating extra variables
      agr_period_from = as.integer(stringr::str_extract(agr_period, "^\\d{4}")),
      agr_period_to   = as.integer(stringr::str_extract(agr_period, "(?<=-)\\d{4}$"))
    )
  
  return(df)
}
