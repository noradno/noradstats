#' Find files on noradstats google drive
#'
#' @param gmail_account Google drive account. Default is \emph{noradstats@gmail.com}
#'
#' @return Returns a tibble of files on noradstats google drive
#' @export
#'
#' @examples
#' ?find_aiddata()
#'

find_aiddata <- function(gmail_account = "noradstats@gmail.com") {
  
  # Authenticating Google drive connection
  googledrive::drive_auth(email = gmail_account)
  
  # Find available files
  googledrive::drive_find()

}