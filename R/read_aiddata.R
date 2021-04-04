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
read_aiddata <- function(file = "statsys_10yr.csv", subdir = FALSE) {
  
  # Read file from current directory
  if (subdir == FALSE) {
    data <- vroom::vroom(file,
                         delim = ";", # Semi-column separated
                         num_threads = 1) # Avoid special characters make new rows
    
    # Read file from sub folder "data"
  } else {
    data <- vroom::vroom(paste0("data/", file),
                         delim = ";",
                         num_threads = 1)
  }
}