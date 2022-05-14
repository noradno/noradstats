#' Get data on Norwegian imputed multilateral ODA to countries and regions, using the OECD SDMX API. Amounts in USD million and NOK million.
#'
#' @param startyear Specify a numeric value of the first year in time period. Default value is \emph{2011}.
#' @param endyear Specity a numeric value of the last year in time period. Default value is \emph{2020}.
#'
#' @return Returns a dataframe (tibble) of Norwegian imputed multilateral ODA to countries and regions
#' @export
#'
#' @examples
#' ?df_imputed <- get_imputed()
#'

get_imputed <- function(startyear = 2011, endyear = 2020) {
  # Scriptet henter fra OECDs databaser Norges imputed multilateral ODA til mottakerland i tidsperioden X til Y
  # Her brukes OECD-datasettet TABLE2A inkludert metadata
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
  sdmx_imputed <- rsdmx::readSDMX(
    providerId = "OECD",
    resource = "data",
    flowRef = "TABLE2A",
    key = ".8.1.106.A",
    key.mode = "SDMX",
    start = startyear,
    end = endyear,
    dsd = TRUE
    )
  
  # Inkluder argument labels= TRUE for å få med metadata-kolonner
  df_imputed <- as.data.frame(sdmx_imputed, labels = TRUE) |>
    tibble::as_tibble()

  # Velger relevante kolonner og gir nytt navn til verdikolonne
  df_imputed <- df_imputed |>
    dplyr::select(AIDTYPE_label.en, DONOR, DONOR_label.en, RECIPIENT, RECIPIENT_label.en, obsTime, obsValue, POWERCODE_label.en, DATATYPE_label.en) |>
    dplyr::rename(usd_mill = obsValue)


  # Vekslingskurs NOR - USD fra OECD ----------------------------------------

  # Her brukes OECD-datasettet SNA_TABLE4, uten metadata
  # URLen er hentet fra nettsiden oecd.stat under National accounts -> Annual National Accounts -> Main aggregates -> 4.PPPs and exchange rates.
  # Har spesifisert URL-en til å filtrere på dimensjonene NOR.EXC.CD. Se i Annex for å idendtifisere dimensjonene, deres rekkefølge og verdier for filtrering.
  
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

  # Inkluder argument labels= TRUE for å få med metadata-kolonner
  df_exchangerate <- as.data.frame(sdmx_exchangerate) |>
    tibble::as_tibble()

  # Velger relevante kolonner og gir nytt navn til verdikolonne
  df_exchangerate <- df_exchangerate |>
    dplyr::select(obsTime, obsValue) |>
    dplyr::rename(exchangerate = obsValue)

  # Inkluderer vekslingskurs-kolonne i df_imputed datasett------------------

  # Lager vekslingskur-kolonne for riktig år
  df_imputed <- dplyr::left_join(df_imputed, df_exchangerate, by = "obsTime")

  # Ny kolonne nok_mill basert på vekslingskurs og rydder variabelnavn
  df_imputed <- df_imputed |>
    dplyr::mutate(nok_mill = usd_mill * exchangerate) |>
    janitor::clean_names()

}
