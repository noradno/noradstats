#' Read Official ODA statistics from aidresults.no to a tibble
#'
#' @param download_folder The path to the default download folder in the default web browser. No default path provided.
#' 
#' @param url The URL of the API call for the selected data. Find the URL for the selected data at https://resultater.norad.no/microdata. The default call is all data for the last ten years.
#'
#' @return The default web browser opens and generates a csv file containg the selected data and saves the file in the default download folder. The file is read as a tibble into the R environment.
#' @export
#'
#' @examples
#' ?read_officialoda(download_folder = "path/to/downloadfolder", url = tenyears)
#'

read_officialoda <- function(download_folder = NULL, url = tenyears) {
  
  tenyears <- "https://resultater.norad.no/api/microdata?from_year=2012&to_year=2021&main_region_code=&country_iso_code=&agreement_partner_group_sid=&agreement_partner_sid=&target_area_code=&dac_main_sector_code=&dac_sub_sector_code=&chapter_code=&format=csv&language=no"
  utils::browseURL(url)
  available_csvs <- file.info(list.files(path = download_folder, pattern = "*.csv", full.names = TRUE))
  sort_available_csvs <- available_csvs |> dplyr::arrange(dplyr::desc(as.POSIXct(mtime)))
  newest_csv <- rownames(sort_available_csvs)[1]
  readr::read_csv(newest_csv)
  
}