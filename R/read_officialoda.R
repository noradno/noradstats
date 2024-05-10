#' Download and read the Official ODA statistics from aidresults.no into R
#' 
#' The function opens the URL of the API call for the default or specified data in the default web browser and downloads the csv file to the default downloads folder in the browser.
#' The function requires the default download folder in the default web browser to be able to read the csv file into R.
#' The function will look for the new csv file in the default download folder and read the csv file into the R environment. It will stop after 60 attempts.
#'
#' @param download_folder Path to the default download folder in the default web browser (path/to/downloadfolder), typically C:/Users/xxx/Downloads.".
#' 
#' @param url The URL of the API call for the selected data. Find the URL for the selected data at https://resultater.norad.no/microdata. The default call is all data for the last ten years.
#'
#' @return Returns a tibble data frame of official ODA statistics from aidresults.no
#' @export
#'
#' @examples
#' ?read_officialoda(download_folder = "path/to/downloadfolder", url = tenyears)
#'

read_aidresults <- function(download_folder, url = tenyears) {
  if (is.null(download_folder)) {
    stop("Path to downloads folder in directory is required. Please specify a valid path.")
  }
  
  # Available csv files in download folder before downloading the new csv file
  pre_n <- length(list.files(download_folder, pattern = "*.csv"))
  
  # The URL with the specified data call to the API
  tenyears <- "https://resultater.norad.no/api/microdata?from_year=2012&to_year=2021&main_region_code=&country_iso_code=&agreement_partner_group_sid=&agreement_partner_sid=&target_area_code=&dac_main_sector_code=&dac_sub_sector_code=&chapter_code=&format=csv&language=no"
  
  # Open URL in default browser to generate and download the csv file to the download folder
  utils::browseURL(url)
  
  # Counter for loop iterations
  iteration_count <- 0
  
  # Repeating operation to detect and read the new csv file into the R environment
  # The reason to repeat the detection of the new csv file is because it takes some time to download the file
  repeat {
    # Time interval for the repeat iteration: 1 second
    startTime <- Sys.time()
    sleepTime <- startTime + 1 - Sys.time()
    
    if (sleepTime > 0)
      Sys.sleep(sleepTime)
    
    # Showing process for each interation
    print("Looking for new .csv file")
    
    # Detecting new csv files in download folder
    new_n <- length(list.files(download_folder, pattern = "*.csv"))
    diff_n <- new_n - pre_n
    
    # Procedure when new file is detected, and stop condition
    if (diff_n > 0) {
      print("New .csv file detected")
      print("Importing .csv file to R environment")
      
      # Identifying the new csv file in the download folder
      available_csvs <-
        file.info(list.files(
          path = download_folder,
          pattern = "*.csv",
          full.names = TRUE
        ))
      
      sort_available_csvs <-
        available_csvs |> dplyr::arrange(dplyr::desc(as.POSIXct(.data$mtime)))
      
      newest_csv <- rownames(sort_available_csvs)[1]
      
      # Read the csv file into the R environment
      # Specify the encoding as UTF-16LE
      data <- readr::read_delim(
        newest_csv,
        delim = ",",
        locale = readr::locale(encoding = "UTF-16LE")
      )
      
      return(data)
      
      break
    }
    
    # Stop after checking for new csv file after 60 attempts
    iteration_count <- iteration_count + 1
    
    if (iteration_count >= 60) {
      print("Maximum iteration limit reached, no new .csv file detected.")
      break
    }
  }
}
