#' Read delimited Norwegian Development Aid dataset (Statsys format and column names) into a tibble
#'
#' @param file Path to local csv-file of Norwegian Development Aid (Statsys format and column names). Default: \emph{statsys_10yr.csv}
#'
#' @return Returns a tibble using the vroom package, using delim = ";", comma as decimal, and num_threads = 1.
#' @export
#'
#' @examples
#' ?read_aiddata()
#'
read_aiddata <- function(file = "statsys_10yr.csv") {
  locale <- readr::locale(decimal_mark = ",") # Set comma as decimal mark

  data <- vroom::vroom(file,
                       delim = ";", # Semi-column separated
                       locale = locale,
                       num_threads = 1) # Avoid that special characters make new rows
}