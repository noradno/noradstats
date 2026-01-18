#' Read Statsys data into R from user-specified CSV file
#'
#' This function reads a Statsys CSV file, which should be delimited by semicolons (;)
#' and have commas (,) as decimal marks. It expects the CSV file to be at the specified path
#' and returns a tibble containing the data. Make sure the path is correctly specified to avoid errors.
#' The function checks for valid path input before attempting to read the file.
#'
#' @param path Required path to CSV file of Statsys data.
#' @return Returns a tibble of Statsys data.
#' @export
#' @examples
#' df_statsys <- read_statsys_from_csv("path/to/your/statsys_file.csv")
#'

read_statsys_from_csv <- function(path) {

  if (!file.exists(path)) {
    stop("File does not exist at the specified path: ", path)
  }
  
  data <- readr::read_delim(
    path,
    delim = ";",
    col_types = readr::cols(
      `Agreement signed` = readr::col_date(format = "%Y%m%d"),
      `Agr compl date` = readr::col_date(format = "%Y%m%d"),
      `Group of Agreement Partner code` = readr::col_integer(),
      `Channel Code ID` = readr::col_integer(),
      `DAC Main sector (code)` = readr::col_integer(),
      `DAC Sub sector (code)` = readr::col_integer(),
      `Recipient country CRS` = readr::col_integer(),
      `Type of assistance (code)` = readr::col_integer(),
      `SDG description` = readr::col_character(),
      Year = readr::col_integer()
      
      ),
    locale = readr::locale(decimal_mark = ",")
  )
  
  return(data)
}
