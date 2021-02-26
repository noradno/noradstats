#' Find available files on (noradstats) Google Drive
#'
#' @param gmail_account Google drive account. Default: \emph{noradstats@gmail.com}
#'
#' @return Returns a tibble of available files on Noradstats Google Drive
#' @export
#'
#' @examples
#' ?find_aiddata()
#'
find_aiddata <- function(gmail_account = "noradstats@gmail.com") {
  googledrive::drive_auth(email = gmail_account)
  googledrive::drive_find()

}