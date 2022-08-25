#' Get data on OECD DAC donors ODA to countries and regions (DAC2a), using the OECD SDMX API. Amounts in USD million
#'
#' @param startyear Specify a numeric value of the first year in time period. Default value is \emph{2020}.
#' @param endyear Specity a numeric value of the last year in time period. Default value is \emph{2020}.
#'
#' @return Returns a dataframe (tibble) on OECD DAC donors ODA to countries and regions (DAC2a)
#' @export
#'
#' @examples
#' ?df_imputed <- get_imputed()
#'

get_donors <- function(startyear = 2020, endyear = 2020) {
  # For å spesifisere key-argumentet, så identifiser hvilke keys-dimensjoner som finnes i datasettet og kan spesifiseres. Punktum-tegnet skiller dimensjonene.
  
  # Keys-argumentet:
  # Punktum er skilletegnet mellom de fem keys.
  # RECIPIENT: Oppgir ingen verdi (alle mottakerland)
  # DONOR: De 29 DAC-landene (separtert med pluss)
  # PART: 1 (utviklingsland)
  # AIDTYPE: 206 (Total net ODA)
  # DATATYPE A (Current prices)
  # TIME (tid spesifiseres separat i start og end.)
  
  # For å få med metadata (DSD) fyll inn dsd = TRUE
  
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
  
  # Strukturerer dsd til dataframe
  df_dac <- as.data.frame(sdmx_dac, labels = TRUE) |>
    tibble::as_tibble()
  
  # Velger relevante kolonner og endrer navn på verdikolonnen
  df_dac <- df_dac |>
    dplyr::select(
      AIDTYPE_label.en,
      DONOR,
      DONOR_label.en,
      RECIPIENT,
      RECIPIENT_label.en,
      obsTime,
      obsValue,
      POWERCODE_label.en,
      DATATYPE_label.en
    ) |>
    dplyr::rename(usd_mill = obsValue)
  
  # Rydder navn
  df_dac <- janitor::clean_names(df_dac)
}
