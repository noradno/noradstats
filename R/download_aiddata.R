#' Download file from (noradstats) Google Drive
#'
#' @param gmail_account Google Drive account. Default: noradstats@gmail.com
#' @param file Character string of desired file to download. Default: \emph{statsys_10yr.csv}. Use noradstats::find_file() to find available files.
#'
#' @return Downloads and saves file in working directory.
#' @export
#'
#' @examples
#' ?download_aiddata()
#'
download_aiddata <- function(file = "statsys_10yr.csv", gmail_account = "noradstats@gmail.com") {
  googledrive::drive_auth(email = gmail_account)
  googledrive::drive_download(file = file,
                              path = file,
                              overwrite = TRUE)
}
