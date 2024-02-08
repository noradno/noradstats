#' Add UCDP conflict variables (country-year level) to ODA data frame from 1989 and later. From UCDP Georeferenced Event Dataset (GED)
#'
#' @param data Input dataframe of Norwegian development assistance
#'
#' @return Returns filtered data frame (Year >= 1989) with additional conflict columns:
#' \itemize{
#'   \item violence: Logical Variable where all observations are TRUE to identify the country-years with at least 1 conflict ID of statebased/nonstate/onesided violence.
#'   \item violence_fatality_count_max: Numerical variable of number of fatalities from the larges conflict in country-year. Should not be used for summarisation in the agreement-level ODA data frame as the number is at country-level.
#'   \item violence_fatality_count_sum: Numerical variable of number of fatalities from all conflicts in country-year. Should not be used for summarisation in the agreement-level ODA data frame as the number is at country-level.
#'   \item violence_intensity_max: Categorical variable of intensity in fatalities of the largest conflict in country-year. "war" >= 1000, "major" > 150 < 1000, "minor" < 150. Non-conflicts are NA. The 150 fatality treshold is based on the World bank methodology: https://thedocs.worldbank.org/en/doc/fb0f93e8e3375803bce211ab1218ef2a-0090082023/original/Classification-of-Fragility-and-Conflict-Situations-FY24.pdf
#'   \item violence_intensity_sum: Categorical variable of intensity in fatalities from all conflicts in country-year. "war" >= 1000, "major" > 150 < 1000, "minor" < 150. Non-conflicts are NA. The 150 fatality treshold is based on the World bank methodology: https://thedocs.worldbank.org/en/doc/fb0f93e8e3375803bce211ab1218ef2a-0090082023/original/Classification-of-Fragility-and-Conflict-Situations-FY24.pdf
#' }
#'
#' @export
#' @examples
#' ?add_cols_violence()

add_cols_violence <- function(data) {
  
  # Check if the ODA data frame has the iso3 country code -------------------
  if(!"iso3" %in% colnames(data)) {
    stop("Error: The data frame must have the column iso3. Use noradstats::add_cols_countrycode to add countrycode column.")
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
    "C:/Users/placeholder_user/UD Office 365 AD/Norad-Avd-Kunnskap - Statistikk og analyse/11. Analyseprosjekter/Faste arrangementer/Tall som teller/2024/ucdp/GEDEvent_v23_1.rds."
  
  # Replace placeholder_user in path with vec_user
  default_path <- gsub("placeholder_user", vec_user, default_path_raw)
  
  # Load UCDP Georeferenced Event Dataset (GED) of idividual events of organised violence (statebased, nonstate and onesided violence)
  ged_raw <- readRDS(default_path)
  
  
  # Create an aggregated dataframe conflict dataset of country-years observations and violence variables ----------
  
  df_country_violence <- ged_raw |>
    dplyr::group_by(.data$country_id, .data$year) |>
    dplyr::summarise(
      
      # Logical Variable where all observations are TRUE to identify the country-years with at least 1 conflict (statebased/nonstate/onesided)
      violence = TRUE,
      
      # Numerical variable of number of fatalities from the larges conflict in country-year
      violence_fatality_count_max = max(.data$best),
      
      # Numerical variable of number of fatalities from all conflicts in country-year
      violence_fatality_count_sum = sum(.data$best),
      
      # Numerical variable of number of conflicts in country-year
      violence_count_sum = dplyr::n_distinct(.data$conflict_new_id),
      
      # Categorical variable of intensity of the largest conflict in country-year
      violence_intensity_max = dplyr::case_when(
        max(.data$best) >= 1000 ~ "war",
        max(.data$best) > 150 & max(.data$best) <= 999 ~ "major",
        .default = "minor"),
      
      # Categorical variable of intensity of the sum of conflicts in country-year
      violence_intensity_sum = dplyr::case_when(
        sum(.data$best) >= 1000 ~ "war",
        sum(.data$best) > 150 & sum(.data$best) <= 999 ~ "major",
        .default = "minor")
    ) |>
    dplyr::ungroup()
  
  
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
    dplyr::select(-country_id) |>
    dplyr::relocate(iso3, .before = 1)
  
  
  # Include country-year level conflict columns to the the agreement level ODA data frame -----------
  
  # Filter ODA dataframe from year 1989 onwards, in line with the conflict data
  data <- data |> 
    dplyr::filter(.data$Year >= 1989)
  
  # Merge the selected conflict columns into the ODA data frame
  data <- dplyr::left_join(data, df_country_violence, by = c("Year" = "year", "iso3" = "iso3"))
  
  # Change NA values the logical violence variable to FALSE and NA values in the categorical character variables to "none"
  data <- data |>
    dplyr::mutate(violence = dplyr::if_else(is.na(violence), FALSE, violence)) |> 
    dplyr::mutate(dplyr::across(c(violence_intensity_max, violence_intensity_sum), ~ dplyr::if_else(is.na(.x), "none", .x)))
  
  
  # # Change NA values in these conflict intensity columns to "None" for country-specific observations. Other NAs are still NA.
  # data <- data |>
  #   dplyr::mutate(dplyr::across(dplyr::all_of(vec_conflict_intesity_variables), ~dplyr::if_else(
  #     is.na(.x) & !stringr::str_detect(.data$`Recipient country`, "Regional|regional|Multilateral|Global|Administration"),
  #     "none", .x)))
  
  
  # Return the ODA data frame including the selected conflict variables. The data frame is filtered by year > 1989 
  return(data)
}