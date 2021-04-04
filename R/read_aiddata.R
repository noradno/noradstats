#' Read delimited Norwegian Development Aid dataset (Statsys format and column names) into a tibble
#'
#' @param file Path to local csv-file of Norwegian Development Aid (Statsys format and column names). Default: \emph{statsys_10yr.csv}
#'
#' @return Returns a tibble using the vroom package, using delim = ";" and num_threads = 1 (to avoid splitting strings into new rows)
#' @export
#'
#' @examples
#' ?read_aiddata()
#'
read_aiddata <- function(file = "statsys_10yr.csv") {
  data <- vroom::vroom(file,
                       delim = ";", # Semi-column separated
                       num_threads = 1) # Avoid that special characters make new rows
}