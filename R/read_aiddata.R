#' Read Norwegian Aid data (Statsys csv) to a tibble
#'
#' @param file csv file of Statsys data. Default is \emph{statsys_ten.csv}
#'
#' @return Returns a tibble using the readr::read_delim function. Using delim = ";", specifying type of specific columns and decimal mark.
#' @export
#'
#' @examples
#' ?read_aiddata()
#'

read_aiddata <- function(file = "statsys_ten.csv") {
  
  data <- readr::read_delim(
    file,
    delim = ";",
    col_types = readr::cols(`SDG description` = readr::col_character()),
    locale = readr::locale(decimal_mark = ",")
  )
  
}