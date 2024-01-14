#' Add UCDP conflict variables (country-year level) to ODA data frame from 1989 and later. From UCDP Georeferenced Event Dataset (GED)
#'
#' @param data Input dataframe of Norwegian development assistance
#' @param type Type of violence: "statebased", "onesided", "nonstate", "all". Default is "statebased" conflict variables.
#'
#' @return Returns filtered data frame (Year >= 1989) with additional conflict columns based on the \emph{type} argument:
#' \itemize{
#'   \item *: violence/statebased_conflict/nonstate_conflict/onesided_violence: Logical variable. Minimum 1 statebased/nonstate/onsided conflict/violence ID in country-year.
#'   \item *_fatality_count: Number of fatalities in country-year. Numerical variable. Should not be used for summarisation in the agreement-level ODA data frame as the number is at country-level.
#'   \item *_count: Number of conflicts/organised violence IDs in country-year. Numerical variable. Should not be used for summarisation in the agreement-level ODA data frame as the number is at country-level.
#'   \item *_intensity: Intensity of the largest conflict in country-year. Categorical variable. Major/war (>1 000 fatalities) vs minor (<1 000 fatalities).
#'   \item *_intensity_sum: Intensity of the sum of conflicts in country-year. Categorical variable. Major/war (>1 000 fatalities) vs minor (<1 000 fatalities).
#'   \item statebased_intensity_lag2years: Intensity of the largest statebased conflict in country-year including a two year lag based on the statebased_intensity variable. Categorical variable. War (>1 000 fatalities) vs minor (<1 000 fatalities).
#' }
#'
#' @export
#' @examples
#' ?add_cols_conflict()

add_cols_conflict <- function(data, type = "statebased") {
  
  # Check if the ODA data frame has the iso3 country code -------------------
  # Include ISO3 codes in ODA data frame if not already present
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
  
  # Path to GED conflict data (rds). Notice that the user id is replaced by the general "placeholder_user" text
  default_path_raw <-
    "C:/Users/placeholder_user/UD Office 365 AD/Norad-Avd-Kunnskap - Statistikk og analyse/11. Analyseprosjekter/Faste arrangementer/Tall som teller/2024/ucdp/GEDEvent_v23_1.rds."
  
  # Replace placeholder_user in path with vec_user
  default_path <- gsub("placeholder_user", vec_user, default_path_raw)
  
  # Load UCDP Georeferenced Event Dataset (GED) of idividual events of organised violence
  ged_raw <- readRDS(default_path)
  
  
  # Create aggregated dataframe of unique country-years observations and violence variables ----------
  
  # Check that only 1 value is provided in the type argument
  if(length(type) > 1) {
    stop("Error: Only one type of violence can be selected at a time. Use value all to include variables for all types of violence")
  }
  
  # If the all value is provided in the type argument, give the type argument the vector of all types of violence
  if(type == "all") {
    type <- c("violence", "statebased", "nonstate", "onesided")
  }
  
  # Organised violence (statebased, onesided, non-state) conflict variables
  if(any(type %in% "violence")) {
    df_country_violence <- ged_raw |>
      dplyr::group_by(.data$country_id, .data$year) |>
      dplyr::summarise(
        
        # Minimum 1 statebased conflict
        violence = TRUE,
        
        # Number of statebased fatalities
        violence_fatality_count = sum(.data$best),
        
        # Number of statebased conflicts
        violence_count = dplyr::n_distinct(.data$conflict_new_id),
        
        # Intensity of the largest statebased conflict
        violence_intensity = dplyr::if_else(max(.data$best) > 1000, "major", "minor"),
        
        # Intensity of the sum of statebased conflicts
        violence_intensity_sum = dplyr::if_else(sum(.data$best) > 1000, "major", "minor")
      ) |>
      dplyr::ungroup()
  }
  
  # Statebased conflict variables: type of violence == 1
  if(any(type %in% "statebased")) {
    df_country_statebased <- ged_raw |>
      dplyr::filter(.data$type_of_violence == 1) |>
      dplyr::group_by(.data$country_id, .data$year) |>
      dplyr::summarise(
        # Minimum 1 conflict
        statebased_conflict = TRUE,
        
        # Number of fatalities
        statebased_fatality_count = sum(.data$best),
        
        # Number of conflicts
        statebased_conflict_count = dplyr::n_distinct(.data$conflict_new_id),
        
        # Intensity of the largest conflict
        statebased_intensity = dplyr::if_else(max(.data$best) > 1000, "war", "minor"),
        
        # Intensity of the sum of conflicts
        statebased_intensity_sum = dplyr::if_else(sum(.data$best) > 1000, "war", "minor")
      ) |>
      dplyr::ungroup()
  }
  
  # Nonstate conflict variables: type of violence == 2
  if(any(type %in% "nonstate")) {
    df_country_nonstate <- ged_raw |>
      dplyr::filter(.data$type_of_violence == 2) |>
      dplyr::group_by(.data$country_id, .data$year) |>
      dplyr::summarise(
        # Minimum 1 conflict
        nonstate_conflict = TRUE,
        
        # Number of fatalities
        nonstate_fatality_count = sum(.data$best),
        
        # Number of conflicts
        nonstate_conflict_count = dplyr::n_distinct(.data$conflict_new_id),
        
        # Intensity of the largest conflict
        nonstate_intensity = dplyr::if_else(max(.data$best) > 1000, "major", "minor"),
        
        # Intensity of the sum of conflicts
        nonstate_intensity_sum = dplyr::if_else(sum(.data$best) > 1000, "major", "minor")
      ) |>
      dplyr::ungroup()
  }
  
  # Onesided violence variables: type of violence == 3
  if(any(type %in% "onesided")) {
    df_country_onesided <- ged_raw |>
      dplyr::filter(.data$type_of_violence == 3) |>
      dplyr::group_by(.data$country_id, .data$year) |>
      dplyr::summarise(
        # Minimum 1 conflict
        onesided_violence = TRUE,
        
        # Number of fatalities
        onesided_fatality_count = sum(.data$best),
        
        # Number of conflicts
        onesided_violence_count = dplyr::n_distinct(.data$conflict_new_id),
        
        # Intensity of the largest conflict
        onesided_intensity = dplyr::if_else(max(.data$best) > 1000, "major", "minor"),
        
        # Intensity of the sum of conflicts
        onesided_intensity_sum = dplyr::if_else(sum(.data$best) > 1000, "major", "minor")
      ) |>
      dplyr::ungroup()
  }
  
  # Select the relevant data frames of conflict columns from the type argument in the function
  if (all(type == c("violence", "statebased", "nonstate", "onesided"))) {
    # All columns
    df_selected <- df_country_violence |>
      dplyr::left_join(df_country_statebased, by = c("country_id", "year")) |>
      dplyr::left_join(df_country_nonstate, by = c("country_id", "year")) |>
      dplyr::left_join(df_country_onesided, by = c("country_id", "year"))
    
    # Specifying false values
    df_selected <- df_selected |>
      dplyr::mutate(dplyr::across(
        c(
          statebased_conflict,
          nonstate_conflict,
          onesided_violence
        ),
        ~ dplyr::if_else(is.na(.x), FALSE, .x)
      ))
    
  } else if (type == "statebased") {
    # Statebased columns
    df_selected <- df_country_statebased
    
  } else if (type == "nonstate") {
    # Nonstate columns
    df_selected <- df_country_nonstate
    
  } else if (type == "onesided") {
    # Onesided columns
    df_selected <- df_country_onesided
  }
  
  # Include iso3 country code and name. Custom codes for Yugoslavia (Serbia) and Yemen
  df_selected <- df_selected |>
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
  df_selected <- df_selected |>
    dplyr::select(-country_id) |>
    dplyr::relocate(iso3, .before = 1)
  
  
  # Include the selected country level conflict columns into the agreement level ODA data frame -----------
  
  # Filter ODA dataframe from year 1989 onwards, in line with the conflict data
  data <- data |> 
    dplyr::filter(.data$Year >= 1989)
  
  # Merge the selected conflict columns into the ODA data frame
  data <- dplyr::left_join(data, df_selected, by = c("Year" = "year", "iso3" = "iso3"))
  
  # Mutate the present logical conflict columns so that the NA is changed to FALSE
  # First, find the present logical conflict columns
  vec_conflict_variables <- c("statebased_conflict", "nonstate_conflict", "onesided_violence", "violence")
  vec_conflict_variables <- vec_conflict_variables[vec_conflict_variables %in% colnames(data)]
  
  # Change NA values in these logical conflict columns to FALSE
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vec_conflict_variables), ~dplyr::if_else(is.na(.x), FALSE, .x)))
  
  # Mutate the present conflict intensity columns so that NA is "none" if it is country-specific observations. Other NAs are still NA.
  # First, find the present conflict intensity columns
  vec_conflict_intesity_variables <- c("statebased_intensity", "nonstate_intensity", "onesided_intensity", "violence_intensity")
  vec_conflict_intesity_variables <- vec_conflict_intesity_variables[vec_conflict_intesity_variables %in% colnames(data)]
  
  # Change NA values in these conflict intensity columns to "None" for country-specific observations. Other NAs are still NA.
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vec_conflict_intesity_variables), ~dplyr::if_else(
      is.na(.x) & !stringr::str_detect(.data$`Recipient country`, "Regional|regional|Multilateral|Global|Administration"),
      "None", .x)))
  
  # Include a lagged statebased_intensity variable (consider to include for other conflict types also)
  # The variable must be created after the merge with the ODA data frame to have all relevant years, to avoid year gaps in the GED data
  
  if(any(type %in% "statebased")) {
    df_temp_statebased_intensity_lag2years <- data |>
      dplyr::filter(.data$statebased_conflict == TRUE) |> 
      dplyr::distinct(.data$iso3, .data$Year, .data$statebased_intensity) |>
      dplyr::group_by(.data$iso3) |>
      dplyr::arrange(.data$iso3, .data$Year) |>
      dplyr::mutate(
        statebased_intensity_lag2years = dplyr::case_when(
          statebased_intensity == "war" |
            dplyr::lag(.data$statebased_intensity, 1) == "war" |
            dplyr::lag(.data$statebased_intensity, 2) == "war" ~ "war",
          statebased_intensity == "minor" |
            dplyr::lag(.data$statebased_intensity, 1) == "minor" |
            dplyr::lag(.data$statebased_intensity, 2) == "minor" ~ "minor",
          .default = .data$statebased_intensity
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-statebased_intensity)

    data <- dplyr::left_join(data, df_temp_statebased_intensity_lag2years, by = c("Year" = "Year", "iso3" = "iso3"))
  }
  
  # Return the ODA data frame including the selected conflict variables. The data frame is filtered by year > 1989 
  return(data)
}
