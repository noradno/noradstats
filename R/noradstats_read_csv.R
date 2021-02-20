#' Read a delimited development aid dataset (csv) into a tibble
#'
#' @param file Path to a local csv file
#'
#' @return Returns a tibble using the vroom package, using delim = ";", comma as decimal, and num_threads = 1.
#' @export
#'
#' @examples
#' ?noradstats_read_csv()
#'
noradstats_read_csv <- function(file) {
  locale <- readr::locale(decimal_mark = ",") # Set comma as decimal mark

  data <- vroom::vroom(file,
                       delim = ";", # Semi-column separated
                       locale = locale,
                       num_threads = 1) # Avoid that special characters make new rows
}
