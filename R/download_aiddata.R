#' Download file from (noradstats) Google Drive
#'
#' @param file Character string of desired file to download. Default: \emph{statsys_10yr.csv}. Use noradstats::find_file() to find available files.
#' @param gmail_account Google Drive account. Default: noradstats@gmail.com
#' @param subdir If TRUE, the file is stored in a sub folder \emph{data}. If FALSE, the file is stored in the current directory.
#'
#' @return Downloads and saves file in working directory.
#' @export
#'
#' @examples
#' ?download_aiddata()
#'
download_aiddata <- function(file = "oda_ten.csv",
                             gmail_account = "noradstats@gmail.com",
                             subdir = FALSE) {
  
  # Authenticating Google drive connection
  googledrive::drive_auth(email = gmail_account)
  
  # If storing file in current directory
  if (subdir == FALSE) {
    googledrive::drive_download(file = file,
                                path = file,
                                overwrite = TRUE)
    
    # If storing in existing sub directory ("data")
  } else if (subdir == TRUE & file.exists("./data") == TRUE) {
    googledrive::drive_download(
      file = file,
      path = paste0("data/", file),
      overwrite = TRUE
    )
    
    # If storing in new sub directory ("data")
  } else {
    dir.create(file.path("./data"))
    googledrive::drive_download(
      file = file,
      path = paste0("data/", file),
      overwrite = TRUE
    )
  }
}