#' Read Norwegian imputed multilateral ODA to countries and regions data into R
#' 
#' This function connects to the OECD SDMX API and extracts Norwegian imputed multilateral ODA to countries and regions data by year, including metadata.
#' The amounts are USD million and caclulates NOK million using the exchange rate data from the OECD API.
#'
#' @param startyear Specify a numeric value of the first year in time period. Default value is \emph{2011}.
#' @param endyear Specity a numeric value of the last year in time period. Default value is \emph{2020}.
#'
#' @return Returns a dataframe (tibble) of Norwegian imputed multilateral ODA to countries and regions
#' @export
#'
#' @examples
#' ?df_imputed_countries <- read_imputed_countries()
#'

read_imputed_countries <- function(startyear = 2011, endyear = 2020) {
  # Using OECD table TABLE2A including metadata (dsd=TRUE)
  # The key arugment spesifies selected values for the available table dimentions in order, separating the dimensions by dots:
  # RECIPIENT: No value (before a dot) includes all recipient countries and regions.
  # DONOR: Norway
  # PART: 1 (developing countries)
  # AIDTYPE: 106 (Imputed multilateral ODA)
  # DATATYPE A (Current prices)
  # TIME (specified in arugment startyear and endyear)
  
  # Include metadata by specifying dsd = TRUE
  sdmx_imputed_countries <- rsdmx::readSDMX(
    providerId = "OECD",
    resource = "data",
    flowRef = "TABLE2A",
    key = ".8.1.106.A",
    key.mode = "SDMX",
    start = startyear,
    end = endyear,
    dsd = TRUE
  )
  
  # Transforming sdmx xml data to dataframe. Inklude metadata columns by using the argument labels= TRUE
  df_imputed_countries <- as.data.frame(sdmx_imputed_countries, labels = TRUE) |>
    tibble::as_tibble()
  
  # Select relevant columns and renaming value column
  df_imputed_countries <- df_imputed_countries |>
    dplyr::select(.data$AIDTYPE_label.en, .data$DONOR, .data$DONOR_label.en, .data$RECIPIENT, .data$RECIPIENT_label.en, .data$obsTime, .data$obsValue, .data$POWERCODE_label.en, .data$DATATYPE_label.en) |>
    dplyr::rename(usd_mill = .data$obsValue)
  
  
  # Dataframe of exchange rate NOR - USD from OECD API ----------------------------------------
  
  # Use OECD table SNA_TABLE4 including metadata
  # URL from oecd.stat under  National accounts -> Annual National Accounts -> Main aggregates -> 4.PPPs and exchange rates.
  # The key argument filters the relevant dimentions in the table: NOR.EXC.CD.
  
  sdmx_exchangerate <- rsdmx::readSDMX(
    providerId = "OECD",
    resource = "data",
    flowRef = "SNA_TABLE4",
    key = "NOR.EXC.CD",
    key.mode = "SDMX",
    start = startyear,
    end = endyear,
    dsd = TRUE
  )
  
  # Transforming sdmx xml data to dataframe. Inklude metadata columns by using the argument labels= TRUE
  df_exchangerate <- as.data.frame(sdmx_exchangerate) |>
    tibble::as_tibble()
  
  # Select relevant columns and renaming value column
  df_exchangerate <- df_exchangerate |>
    dplyr::select(.data$obsTime, .data$obsValue) |>
    dplyr::rename(exchangerate = .data$obsValue)
  
  # Include exchange rate column in df_imputed_countries dataset------------------
  
  # New exchange rate column by year
  df_imputed_countries <- dplyr::left_join(df_imputed_countries, df_exchangerate, by = "obsTime")
  
  # New column nok_mill based on exchange rate and cleaning variable names
  df_imputed_countries <- df_imputed_countries |>
    dplyr::mutate(nok_mill = .data$usd_mill * .data$exchangerate) |>
    janitor::clean_names()
  
}
