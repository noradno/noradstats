#' Download file from Google drive (to download development aid dataset)
#'
#' @param gmail_account Google drive account. Default: noradstats@gmail.com
#' @param file Filename to download. Use noradstats::find to find available files.
#'
#' @return Downloads the file and saves the file in working directory.
#' @export
#'
#' @examples
#' ?noradstats_download()
#'
noradstats_download <- function(file = NULL, gmail_account = "noradstats@gmail.com") {
  googledrive::drive_auth(email = gmail_account)
  googledrive::drive_download(file = file,
                              path = file,
                              overwrite = TRUE)
}
