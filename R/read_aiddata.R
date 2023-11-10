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

read_aiddata <- function(file = path) {
  
  # Find project directory
  wd <- getwd()
  
  # Extract the user id: letters starting from "/u"
  vec_user <- substr(wd, regexpr("/u", wd) + 1, regexpr("/u", wd) + 6)
  
  # Path to PTA agreement totals report (csv). Notice that the usernumber is replaced by the general "tUserCode" text
  path <- "C:/Users/placeholder_user/UD Office 365 AD/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/4. CSV/statsys_ten.csv"
  
  # Replace Insertuser in path with vec_user
  path <- gsub("placeholder_user", vec_user, path)
  
  # Read
  data <- readr::read_delim(
    path,
    delim = ";",
    col_types = readr::cols(`SDG description` = readr::col_character()),
    locale = readr::locale(decimal_mark = ",")
  )
  
}