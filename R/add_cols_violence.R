#' Add UCDP organised violence variables (country-year observations) to ODA data frame (agreement-level observations) from 1989 and later. Source: UCDP Georeferenced Event Dataset (GED) and UCDP Candidates dataset (for 2023)
#'
#' @param data Input dataframe of Norwegian development assistance
#'
#' @return Returns filtered data frame (Year >= 1989) with additional conflict columns:
#' \itemize{
#'   \item violence_fatality_max: Numerical variable of number of fatalities from the larges conflict-id in country-year. Should not be used for summarisation in the agreement-level ODA data frame as the number is at country-level.
#'   \item violence_fatality_sum: Numerical variable of number of fatalities from all conflict-ids in country-year. Should not be used for summarisation in the agreement-level ODA data frame as the number is at country-level.
#'   \item violence_intensity: Categorical variable of intensity in fatalities of the largest conflict-id in country-year. "war" >= 1000, "major" > 150 & <= 999, "minor" <= 150. Non-conflicts are NA. The 150 fatality treshold is based on the World bank methodology, but also includes non-state violence and the 150 treshold is applied to the largest (not the sum) of conflict-ids in a country-year, and the WB methodology is not clear on that issue. https://thedocs.worldbank.org/en/doc/fb0f93e8e3375803bce211ab1218ef2a-0090082023/original/Classification-of-Fragility-and-Conflict-Situations-FY24.pdf
#'   \item violence_150: Logical variable of war/major conflict (maximum conflict-id over 150 fatalities in country-year). Based on the violence_intensity variable.
#'   \item violence_25: Logical variable of UCDP conflict (maximum conflict-id over 25 fatalities in country-year)
#' }
#'
#' @export
#' @examples
#' ?add_cols_violence()

add_cols_violence <- function(data) {
  
  # Check if the ODA data frame has the iso3 country code -------------------
  if(!"iso3" %in% colnames(data)) {
    stop("Error: The data frame must have the column iso3 to run add_cols_violence. Use noradstats::add_cols_countrycode to add the iso3 column.")
  }
  
  # Import GED conflict data from local rds file -----------------------------
  
  # Find the user id to input in the file path
  wd <- getwd()
  
  # Check if the second character of the Userid is a digit (new style) or not (old style)
  # Use regexpr to find the position of "Users/"
  pos <- regexpr("Users/", wd)
  
  # Find the second character after "Users/"
  second_char <- substr(wd, pos + 7, pos + 7)
  
  # Check if the second character is a digit (new user id format) or not (old user id format)
  is_digit <- grepl("\\d", second_char)
  
  # Extract the user id from the wd dependent on it is a new or old user id format
  if (is_digit) {
    vec_user <- substr(wd, pos + 6, pos + 11)
  } else {
    vec_user <- substr(wd, pos + 6, pos + 9)
  }
  
  # Path to GED conflict data (rds). The user id is replaced by the general "placeholder_user" text
  default_path_raw <-
    "C:/Users/placeholder_user/UD Office 365 AD/Norad-Avd-Kunnskap - Statistikk og analyse/11. Analyseprosjekter/Faste arrangementer/Tall som teller/2024/ucdp/GEDEvent_v23_1_incl_candidates.rds."
  
  # Replace placeholder_user in path with vec_user
  default_path <- gsub("placeholder_user", vec_user, default_path_raw)
  
  # Load UCDP Georeferenced Event Dataset (GED) of idividual events of organised violence (statebased, nonstate and onesided violence)
  ged_raw <- readRDS(default_path)
  
  
  # Create an aggregated country-year dataset of violence variables --------
  df_country_violence <- ged_raw |> 
    
    # First, a temporary numerical variable to summarise the number of fatalities in each conflict-id in each country-year
    dplyr::group_by(.data$country_id, .data$conflict_new_id, .data$year) |> 
    dplyr::summarise(country_conflictid_year_sum = sum(.data$best)) |> 
    dplyr::ungroup() |> 
    
    # Create two fatality variables for each country-year
    # Numerical variable of the number of fatalities in the largest conflict-id in each country-year
    dplyr::group_by(.data$country_id, .data$year) |> 
    dplyr::summarise(
      violence_fatality_max = max(.data$country_conflictid_year_sum),
      violence_fatality_sum = sum(.data$country_conflictid_year_sum)) |> 
    dplyr::ungroup() |>
    
    # Categorical variable of the intensity of the largest conflict-id in each country-year
    dplyr::mutate(violence_intensity = dplyr::case_when(
      .data$violence_fatality_max >= 1000 ~ "war",
      .data$violence_fatality_max > 150 & .data$violence_fatality_max <= 999 ~ "major",
      TRUE ~ "minor"
      )
    ) |> 
    
    # Logical variable of war/major conflict (maximum conflict-id over 150 fatalities in country-year)
    dplyr::mutate(violence_150 = .data$violence_intensity %in% c("war", "major")) |> 
    
    # Logical variable of UCDP conflict (maximum conflict-id over 25 fatalities in country-year)
    dplyr::mutate(violence_25 = TRUE)
  
  # Include iso3 country code and name in the conflict dataset. Custom codes for Yugoslavia (Serbia) and Yemen --------
  df_country_violence <- df_country_violence |>
    dplyr::mutate(
      iso3 = countrycode::countrycode(
        .data$country_id,
        origin = "gwn",
        destination = "iso3c",
        custom_match = c("678" = "YEM",
                         "345" = "SRB")
      )
    )
  
  # Relocate columns and remove country_id
  df_country_violence <- df_country_violence |>
    dplyr::select(-.data$country_id) |>
    dplyr::relocate(.data$iso3, .before = 1)
  
  
  # Include country-year level conflict columns to the the agreement level ODA data frame -----------
  
  # Filter ODA dataframe from year 1989 onwards, in line with the conflict data
  data <- data |> 
    dplyr::filter(.data$Year >= 1989)
  
  # Merge the selected conflict columns into the ODA data frame
  data <- dplyr::left_join(data, df_country_violence, by = c("Year" = "year", "iso3" = "iso3"))
  
  # Change NA values in logical violence-variables to FALSE and the categorical violence_intensity variable to "none"
  data <- data |>
    dplyr::mutate(dplyr::across(c(.data$violence_25, .data$violence_150), ~ dplyr::if_else(is.na(.x), FALSE, .x))) |> 
    dplyr::mutate(violence_intensity = dplyr::if_else(is.na(.data$violence_intensity), "none", .data$violence_intensity))
  
  
  # # Change NA values in these conflict intensity columns to "None" for country-specific observations. Other NAs are still NA.
  # data <- data |>
  #   dplyr::mutate(dplyr::across(dplyr::all_of(vec_conflict_intesity_variables), ~dplyr::if_else(
  #     is.na(.x) & !stringr::str_detect(.data$`Recipient country`, "Regional|regional|Multilateral|Global|Administration"),
  #     "none", .x)))
  
  
  # Return the ODA data frame including the selected conflict variables. The data frame is filtered by year > 1989 
  return(data)
}
