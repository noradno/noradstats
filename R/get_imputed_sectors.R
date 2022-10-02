#' Get data on Norwegian imputed multilateral ODA to sectors by year, using the OECD SDMX API. Amounts in USD million and NOK million.
#'
#' @param startyear Specify a numeric value of the first year in time period. Default value is \emph{2011}.
#' @param endyear Specity a numeric value of the last year in time period. Default value is \emph{2020}.
#'
#' @return Returns a dataframe (tibble) of Norwegian imputed multilateral ODA to sectors
#' @export
#'
#' @examples
#' ?df_imputed_sectors <- get_imputed_sectors()
#'

get_imputed_sectors <- function(startyear = 2020, endyear = 2020) {
  # Using OECD table TABLE2A including metadata (dsd=TRUE)
  # The key arugment spesifies selected values for the available table dimentions in order, separating the dimensions by dots:
  # DONOR: Norway
  # SECTOR: 110+139+230+310 (sectors)
  # AIDTYPE: 599 (imputed)
  # DATATYPE A (Current prices)
  # TIME (specified in arugment startyear and endyear)
  
  # Include metadata by specifying dsd = TRUE
  sdmx_imputed_sectors <- rsdmx::readSDMX(
    providerId = "OECD",
    resource = "data",
    flowRef = "TABLE5",
    key = "8.110+139+230+310.599.A",
    key.mode = "SDMX",
    start = startyear,
    end = endyear,
    dsd = TRUE
  )
  
  # Transforming sdmx xml data to dataframe. Inklude metadata columns by using the argument labels= TRUE
  df_imputed_sectors <- as.data.frame(sdmx_imputed_sectors, labels = TRUE) |>
    tibble::as_tibble()
  
  # Select relevant columns and renaming value column
  df_imputed_sectors <- df_imputed_sectors |>
    dplyr::select(.data$AIDTYPE_label.en, .data$DONOR, .data$DONOR_label.en, .data$SECTOR, .data$SECTOR_label.en, .data$obsTime, .data$obsValue, .data$POWERCODE_label.en, .data$AMOUNT_label.en) |>
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
  
  # Include exchange rate column in df_imputed_sectors dataset------------------
  
  # New exchange rate column by year
  df_imputed_sectors <- dplyr::left_join(df_imputed_sectors, df_exchangerate, by = "obsTime")
  
  # New column nok_mill based on exchange rate and cleaning variable names
  df_imputed_sectors <- df_imputed_sectors |>
    dplyr::mutate(nok_mill = .data$usd_mill * .data$exchangerate) |>
    janitor::clean_names()
  
}
