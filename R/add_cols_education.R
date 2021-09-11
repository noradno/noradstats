#' Add column \emph{Education incl. Education in Emergencies} to dataframe
#'
#' @param data Input dataframe of Norwegian development assistance
#'
#' @return Returns dataframe with additional column:
#' \itemize{
#'   \item education_incl_emergencies: Includes \emph{Target area = Education} and \emph{DAC Sub sector (code+name) = 12 - Education in Emergencies}. 3 levels: 1) \emph{Education}, 2) \emph{Education in Emergencies}, 3) \emph{None}.
#'}
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' ?add_cols_education()
#'
add_cols_education <- function(data) {
  data %>%

    # column education_incl_emergencies
    dplyr::mutate(
      education_incl_emergencies = dplyr::case_when(
        .data$`Target area` == "Education" ~ "Education",
        .data$`DAC Sub sector (code+name)` == "12 - Education in Emergencies" ~ "Education in Emergencies",
        TRUE ~ "None")) %>%

    dplyr::mutate(
      education_incl_emergencies = forcats::fct_relevel(
        .data$education_incl_emergencies,
        "Education",
        "Education in Emergencies",
        "None"))
}
