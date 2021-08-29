#' Read delimited Norwegian Development Aid dataset (Statsys format and column names) into a tibble
#'
#' @param file Path to local csv-file of Norwegian Development Aid (Statsys format and column names). Default: \emph{statsys_10yr.csv}
#' @param subdir If TRUE, look for file in sub folder \emph{data}. If FALSE, look in current directory as specified in \emph{file} argument.
#'
#' @return Returns a tibble using the vroom package, using delim = ";" and num_threads = 1 (to avoid splitting strings into new rows)
#' @export
#'
#' @examples
#' ?read_aiddata()
#'
read_aiddata <- function(file = "oda_ten.csv", subdir = FALSE) {
  
  # Read file from current directory
  if (subdir == FALSE) {
    data <- readr::read_delim(file,
                         delim = ";", # Semi-column separated
                         col_types = readr::cols(`SDG description` = readr::col_character()), # Specifying SDG description as type character
                         locale = readr::locale(decimal_mark = ",")
                         ) 
    
    # Read file from sub folder "data"
  } else {
    data <- readr::read_delim(paste0("data/", file),
                         delim = ";", # Semi-column separated
                         col_types = readr::cols(`SDG description` = readr::col_character()), # Specifying SDG description as type character
                         locale = readr::locale(decimal_mark = ",")
                         )
  }
}