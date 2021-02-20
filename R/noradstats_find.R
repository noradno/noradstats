#' Find files on Google drive (to find development aid datasets)
#'
#' @param gmail_account Google drive account. Default: noradstats@gmail.com
#'
#' @return Returns a tibble of available files on google drive account. Look for ODA datasets.
#' @export
#'
#' @examples
#' ?noradstats_find()
#'
noradstats_find <- function(gmail_account = "noradstats@gmail.com") {
  googledrive::drive_auth(email = gmail_account)
  googledrive::drive_find()

}
