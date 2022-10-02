#' Get data on OECD DAC donors ODA to countries and regions (DAC2a), using the OECD SDMX API. Amounts in USD million
#'
#' @param startyear Specify a numeric value of the first year in time period. Default value is \emph{2020}.
#' @param endyear Specity a numeric value of the last year in time period. Default value is \emph{2020}.
#'
#' @return Returns a dataframe (tibble) on OECD DAC donors ODA to countries and regions (DAC2a)
#' @export
#'
#' @examples
#' ?df_donors <- get_donors()
#'

get_donors <- function(startyear = 2020, endyear = 2020) {
  # Using OECD table TABLE2A including metadata (dsd=TRUE)
  # The key arugment spesifies selected values for the available table dimentions in order, separating the dimensions by dots:
  # RECIPIENT: No value (before a dot) includes all recipient countries and regions.
  # DONOR: The 29 DAC-countries (separated by plus)
  # PART: 1 (developing countries)
  # AIDTYPE: 206 (Total net ODA)
  # DATATYPE A (Current prices)
  # TIME (specified in arugment startyear and endyear)
  
  sdmx_dac <- rsdmx::readSDMX(
    providerId = "OECD",
    resource = "data",
    flowRef = "TABLE2A",
    key = ".801+1+2+301+68+3+18+4+5+40+75+20+21+6+701+742+22+7+820+8+76+9+69+61+50+10+11+12+302.1.206.A",
    key.mode = "SDMX",
    start = startyear,
    end = endyear,
    dsd = TRUE
  )
  
  # Transforming sdmx xml data to dataframe. Inklude metadata columns by using the argument labels= TRUE
  df_dac <- as.data.frame(sdmx_dac, labels = TRUE) |>
    tibble::as_tibble()
  
  # Select relevant columns and renaming value column
  df_dac <- df_dac |>
    dplyr::select(
      .data$AIDTYPE_label.en,
      .data$DONOR,
      .data$DONOR_label.en,
      .data$RECIPIENT,
      .data$RECIPIENT_label.en,
      .data$obsTime,
      .data$obsValue,
      .data$POWERCODE_label.en,
      .data$DATATYPE_label.en
    ) |>
    dplyr::rename(usd_mill = .data$obsValue)
  
  # Clean names
  df_dac <- janitor::clean_names(df_dac)
}
