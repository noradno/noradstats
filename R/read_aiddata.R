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
    docs <- path.expand("~")
    home <- dirname(docs)
    
    # Path to PTA agreement totals report (csv).
    default_path_end <- "/Norad/Norad-Avd-Kunnskap - Statistikk og analyse/06. Statistikkdatabaser/4. CSV/statsys_total.csv"
    default_path <- paste0(home, default_path_end)
    
    # Read
    data <- readr::read_delim(
      default_path,
      delim = ";",
      col_types = readr::cols(`SDG description` = readr::col_character()),
      locale = readr::locale(decimal_mark = ",")
    )
  }
}