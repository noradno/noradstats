#' Read PTA Disbursement level report into R from user-specified CSV file
#'
#' Reads and processes the PTA Disbursement level report to produce a cleaned
#' dataset of disbursement level data
#'
#' @param path Path to the PTA Disbursement level CSV file
#' @return A cleaned data frame with one row per agreement
read_pta_disbursement_level_from_csv <- function(path) {
  if (!file.exists(path)) {
    stop("File Disbursement level.csv does not exist")
  }
  
  readr::read_csv2(
    file = path,
    skip = 7,
    col_types = readr::cols(
      `Case no` = readr::col_character(),
      `Agreement period (from)` = readr::col_integer(),
      `Agreement period (to)` = readr::col_integer(),
      Year = readr::col_integer(),
      Date = readr::col_date(format = "%d.%m.%Y"),
      Sector = readr::col_integer(),
      `Sub sector` = readr::col_integer(),
      `Main sector` = readr::col_integer(),
      `Cost center` = readr::col_integer(),
      `Programme area` = readr::col_integer()
    ),
    locale = readr::locale(decimal_mark = ",", grouping_mark = " ", encoding = "UTF-8")
  ) |>
    head(-2) |> # Remove footer rows
    
    janitor::clean_names() |> # Clean column names
    dplyr::filter(stringr::str_detect(cost_center_name, "^[A-ZÆØÅ]{2}")) |>  # Filter out invalid rows
    dplyr::mutate(
      # Replace "-" values with "None" in all character variables
      dplyr::across(
        dplyr::where(is.character),
        ~ dplyr::if_else(.x == "-", "None", .x)
      )
    ) |>
    dplyr::filter(
      agr_phase != "E"
    )
}
