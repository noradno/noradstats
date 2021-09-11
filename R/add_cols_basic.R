#' Add basic columns to dataframe
#'
#' @param data Input dataframe of Norwegian development assistance
#'
#' @return Returns dataframe with additional columns:
#' \itemize{
#'   \item target_area_no: Norwegian translation of column \emph{Target area}.
#'   \item partner_group_visual_no: Visual grouping and Norwegian translation of column \emph{Group of agreement partner}.
#'   \item partner_group_visual: Visual grouping of column \emph{Group of agreement partner}.
#'   \item main_region_no: Norwegian translation of column \emph{Main region}.
#' }
#'
#' @export
#' 
#' @importFrom rlang .data
#'
#' @examples
#' ?add_cols_basic()
add_cols_basic <- function(data) {
  data %>%

    # column target_area_no
    dplyr::mutate(
      target_area_no = dplyr::case_when(
        .data$`Target area` == "Economic infrastructure and services" ~ paste0(stringi::stri_unescape_unicode("\\u00d8"), "konomisk utvikling og tjenester"),
        .data$`Target area` == "Education" ~ "Utdanning",
        .data$`Target area` == "Emergency assistance" ~ paste0("N", stringi::stri_unescape_unicode("\\u00f8"), "dhjelp"),
        .data$`Target area` == "Environment and energy" ~ paste0("Milj", stringi::stri_unescape_unicode("\\u00f8"), " og energi"),
        .data$`Target area` == "Governance, civil society and conflict prevention" ~ "Styresett, sivilt samfunn og konfliktforebygging",
        .data$`Target area` == "Health and social services" ~ "Helse og sosiale tjenester",
        .data$`Target area` == "In donor costs and unspecified" ~ "Kostnader i Norge og uspesifisert",
        .data$`Target area` == "Multilateral" ~ paste0("Multilateral kjernest", stringi::stri_unescape_unicode("\\u00f8"), "tte"),
        .data$`Target area` == "Multisector and other" ~ "Multisektor og annet",
        .data$`Target area` == "Production sectors and trade" ~ "Produksjon og handel",
        TRUE ~ "NA"
      )
    ) %>%

    # column partner_group_visual_no
    dplyr::mutate(
      partner_group_visual_no = dplyr::case_when(
        .data$`Group of Agreement Partner` == "Governments/Ministries in developing countries" |
          .data$`Group of Agreement Partner` == "Public sector in developing countries" ~ "Offentlig sektor i mottakerland",
        .data$`Group of Agreement Partner` == "Multilateral institutions" ~ "Multilaterale organisasjoner",
        .data$`Group of Agreement Partner` == "NGO International" |
          .data$`Group of Agreement Partner` == "NGO Local" |
          .data$`Group of Agreement Partner` == "NGO Other donor countries" ~ "Andre ikke-statlige organisasjoner",
        .data$`Group of Agreement Partner` == "NGO Norwegian" ~ "Norske ikke-statlige organisasjoner",
        .data$`Group of Agreement Partner` == "Norwegian public sector" |
          .data$`Group of Agreement Partner` == "Public sector other donor countries" ~ "Offentlig sektor i Norge/ andre giverland",
        .data$`Group of Agreement Partner` == "Private sector in developing countries" |
          .data$`Group of Agreement Partner` == "Private sector in other donor countries" |
          .data$`Group of Agreement Partner` == "Norwegian private sector" |
          .data$`Group of Agreement Partner` == "Other countries private sector" |
          .data$`Group of Agreement Partner` == "Consultants"  ~ "Privat sektor",
        .data$`Group of Agreement Partner` == "Public-private partnerships" |
          .data$`Group of Agreement Partner` == "Network"  ~ "Offentlig-privat samarbeid og nettverk",
        .data$`Group of Agreement Partner` == "Unknown" ~ "Uspesifisert",
        TRUE ~ "NA"
      )
    ) %>%

    # column partner_group_visual
   dplyr::mutate(
      partner_group_visual = dplyr::case_when(
        .data$`Group of Agreement Partner` == "Governments/Ministries in developing countries" |
          .data$`Group of Agreement Partner` == "Public sector in developing countries" ~ "Public sector in recipient country",
        .data$`Group of Agreement Partner` == "Multilateral institutions" ~ "Multilateral organisations",
        .data$`Group of Agreement Partner` == "NGO International" |
          .data$`Group of Agreement Partner` == "NGO Local" |
          .data$`Group of Agreement Partner` == "NGO Other donor countries" ~ "Other non-governmental organisations",
        .data$`Group of Agreement Partner` == "NGO Norwegian" ~ "Norwegian non-governmental organisations",
        .data$`Group of Agreement Partner` == "Norwegian public sector" |
          .data$`Group of Agreement Partner` == "Public sector other donor countries" ~ "Public sector in Norway/other donors",
        .data$`Group of Agreement Partner` == "Private sector in developing countries" |
          .data$`Group of Agreement Partner` == "Private sector in other donor countries" |
          .data$`Group of Agreement Partner` == "Norwegian private sector" |
          .data$`Group of Agreement Partner` == "Other countries private sector" |
          .data$`Group of Agreement Partner` == "Consultants"  ~ "Private sector",
        .data$`Group of Agreement Partner` == "Public-private partnerships" |
          .data$`Group of Agreement Partner` == "Network"  ~ "Public-private partnerships and networks",
        .data$`Group of Agreement Partner` == "Unknown" ~ "Unspecified",
        TRUE ~ "NA"
      )
    ) %>%

    # column main_region_no
    dplyr::mutate(
      main_region_no = dplyr::case_when(
        .data$`Main Region` == "Africa" ~ "Afrika",
        .data$`Main Region` == "Asia" ~ "Asia",
        .data$`Main Region` == "Not geographically allocated" ~ "Geografisk uspesifisert",
        .data$`Main Region` == "Europe" ~ "Europa",
        .data$`Main Region` == "America" ~ "Amerika",
        .data$`Main Region` == "The Middle East" ~ paste0("Midt", stringi::stri_unescape_unicode("\\u00f8"), "sten"),
        .data$`Main Region` == "Oceania" ~ "Oseania"
      )
    )
}
