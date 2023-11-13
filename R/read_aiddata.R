#' Read Norwegian Aid data (Statsys csv) into R
#'
#' @param file csv file of Statsys data. Default is \emph{statsys_ten.csv}
#'
#' @return Returns a tibble using the readr::read_delim function. Using delim = ";", specifying type of specific columns and decimal mark.
#' @export
#'
#' @examples
#' ?read_aiddata()
#'

read_aiddata <- function(file = NULL) {
  
  # I argument file is not null
  if (!is.null(file)) {
    # Read
    data <- readr::read_delim(
      file,
      delim = ";",
      col_types = readr::cols(`SDG description` = readr::col_character()),
      locale = readr::locale(decimal_mark = ",")
    )
  } else {
    # I argument file is null - use default path
    
    # Find the user id to input in the path
    # Find project directory
    wd <- getwd()
    
    # Check if the second character of the Userid is a digit (new style) or not (old style)
    # Use regexpr to find the position of "Users/"
    pos <- regexpr("Users/", wd)
    
    # Find the second character after "Users/"
    second_char <- substr(wd, pos + 7, pos + 7)
    
    # Check if the second character is a digit (new user id format) or not (old user id format)
    is_digit <- grepl("\\d", second_char)
    
    # Extract the user id from the wd dependent on it is a new or old user id format
    if (is_digit) {
      vec_user <- substr(wd, pos + 6, pos + 11)
    } else {
      vec_user <- substr(wd, pos + 6, pos + 9)
    }
    
    # Path to PTA agreement totals report (csv). Notice that the user id is replaced by the general "placeholder_user" text
    default_path_raw <-
      "C:/Users/placeholder_user/UD Office 365 AD/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/4. CSV/statsys_total.csv"
    
    # Replace placeholder_user in path with vec_user
    default_path <- gsub("placeholder_user", vec_user, default_path_raw)
    
    # Read
    data <- readr::read_delim(
      default_path,
      delim = ";",
      col_types = readr::cols(`SDG description` = readr::col_character()),
      locale = readr::locale(decimal_mark = ",")
    )
  }
}