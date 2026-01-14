#' Official column name mapping (internal -> official)
#'
#' Returns the mapping between internal (snake_case) column names and official
#' (public) labels for a given dataset and language.
#'
#' Valid dataset keys currently include:
#' \itemize{
#'   \item \code{"oda"} – Official development assistance (ODA), disbursement-level data
#'   \item \code{"pta_disbursements"} – PTA disbursement-level data
#'   \item \code{"pta_agreement_totals"} – PTA agreement-level totals
#' }
#'
#' The full list of valid dataset keys is defined in
#' \code{inst/extdata/official_colnames.csv}.
#'
#' @param dataset Dataset key (character) identifying which official naming
#'   scheme to use. Must match a value in the \code{dataset} column of
#'   \code{inst/extdata/official_colnames.csv}.
#' @param lang Language for official labels. One of \code{"en"} or \code{"no"}.
#'
#' @return A tibble with columns \code{dataset}, \code{internal}, and \code{official}.
#' @export
official_colname_map <- function(dataset, lang = c("en", "no")) {
  
  lang <- match.arg(lang)
  
  # Read mapping and restrict to requested dataset
  df <- read_official_colname_map() |>
    dplyr::filter(.data$dataset %in% .env$dataset)
  
  col_lang <- paste0("official_", lang)
  
  # Keep only the columns needed downstream
  df |>
    dplyr::transmute(
      dataset  = .data$dataset,
      internal = .data$internal,
      official = .data[[col_lang]]
    )
}

#' Rename columns to official (public) labels
#'
#' Renames internal snake_case column names to official public labels using a
#' dataset-specific mapping.
#'
#' Valid dataset keys currently include:
#' \itemize{
#'   \item \code{"oda"}
#'   \item \code{"pta_disbursements"}
#'   \item \code{"pta_agreement_totals"}
#' }
#'
#' The full list of valid dataset keys is defined in
#' \code{inst/extdata/official_colnames.csv}.
#'
#' Only columns with an official label for the selected dataset and language
#' are renamed. All other columns are left unchanged (e.g. derived columns).
#'
#' @param x A data frame or tibble.
#' @param dataset Dataset key identifying which official naming scheme to use.
#' @param lang Language for official labels. One of \code{"en"} or \code{"no"}.
#'
#' @return \code{x} with renamed columns.
#' @export
rename_to_official <- function(x, dataset, lang = c("en", "no")) {
  
  lang <- match.arg(lang)
  
  # Get official names for this dataset and language
  df_colnames <- official_colname_map(dataset = dataset, lang = lang) |>
    dplyr::filter(!is.na(.data$official), .data$official != "")
  
  # Create named vector: internal -> official
  vec <- stats::setNames(df_colnames$official, df_colnames$internal)
  
  # Rename only columns that have an official label
  cols_to_rename <- intersect(names(x), names(vec))
  
  x |>
    dplyr::rename_with(~ unname(vec[.x]), .cols = dplyr::all_of(cols_to_rename))
}

#' List available dataset keys for official column names
#'
#' Returns the dataset keys that can be used with \code{official_colname_map()}
#' and \code{rename_to_official()}.
#'
#' @return A character vector of dataset keys.
#' @export
available_official_datasets <- function() {
  
  read_official_colname_map() |>
    dplyr::distinct(.data$dataset) |>
    dplyr::pull(.data$dataset)
}

# ---- internal ---------------------------------------------------------------

#' Read official column name mapping from package extdata
#'
#' @keywords internal
read_official_colname_map <- function() {
  
  path <- system.file("extdata", "official_colnames.csv", package = "noradstats")
  if (identical(path, "")) {
    stop("Missing extdata/official_colnames.csv in package.", call. = FALSE)
  }
  
  readr::read_delim(path, delim = ";", show_col_types = FALSE)
}
