#' Download data from noradstats google drive
#'
#' @param file Character string of file to download. Default file is \emph{statsys_ten.csv}. Use noradstats::find_aiddata() to find available files.
#' @param path Character string of path to save file. Default path is file name in current directory.
#' @param gmail_account Google Drive account. Default is \emph{noradstats@gmail.com}
#'
#' @return Downloads file to path.
#' @export
#'
#' @examples
#' ?get_aiddata()
#'

get_aiddata <- function(file = "statsys_ten.csv", path = NULL, gmail_account = "noradstats@gmail.com") {
  
  # Default path is current directory
  if (is.null(path)) {
  path <- file
  }
  
  # Authenticating Google drive connection
  googledrive::drive_auth(email = gmail_account)
  
  # Download file to path
  googledrive::drive_download(file = file, path = path,
                                overwrite = TRUE)
  
}
