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
      # Cleaning the agreement period variable (extract the years, allow NA years from to, and create additional from and to variable)
      agreement_period = {
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
      agreement_period_from = as.integer(stringr::str_extract(agreement_period, "^\\d{4}")),
      agreement_period_to   = as.integer(stringr::str_extract(agreement_period, "(?<=-)\\d{4}$"))
    ) |>
    dplyr::select(-agr_period) |> 
    dplyr::relocate(expected_agreement_total, .after = dplyr::last_col())
  
  return(df)
}
