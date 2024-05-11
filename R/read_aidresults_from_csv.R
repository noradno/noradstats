#' Read aidresults.no data into R from user-specified CSV file
#'
#' This function reads a CSV file of ODA statistics downloaded from aidresults.no: https://resultater.norad.no/microdata
#' The function excepcts the CSV to be delimited by semicolons (;) commas (,) as decimal marks and UTF-16LE encoding.
#' It expects the CSV file to be at the specified path and returns a tibble containing the data. Make sure the path is correctly specified to avoid errors.
#' The function checks for valid path input before attempting to read the file.
#' 
#' 
#' @param path Required path to CSV file of Offical Norwegian ODA data downloaded from aidresults.no: https://resultater.norad.no/microdata
#' @return Returns a tibble of Norwegian ODA data
#' @export
#' @examples
#' ?read_aidresults_from_csv

read_aidresults_from_csv <- function(path) {
  
  if (!file.exists(path)) {
    stop("File does not exist at the specified path: ", path)
  }

      # Read the csv file into the R environment using encoding UTF-16LE
      data <- readr::read_delim(
        path,
        delim = ",",
        locale = readr::locale(encoding = "UTF-16LE")
      )
      
      return(data)
}
