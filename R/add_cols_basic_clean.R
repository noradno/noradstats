#' Add basic columns to data frame of Norwegian development assistance
#'
#' @param data Input dataframe of Norwegian development assistance in snake_case column names
#'
#' @return Returns dataframe with additional columns:
#' \itemize{
#'   \item target_area_no: Norwegian translation of column \emph{Target area}.
#'   \item partner_group_visual_no: Visual grouping and Norwegian translation of column \emph{Group of agreement partner}.
#'   \item partner_group_visual: Visual grouping of column \emph{Group of agreement partner}.
#'   \item main_region_no: Norwegian translation of column \emph{Main region}.
#'   \item income_category_no: Norwegain translation of column \emph{Income category}.
#'   \item earmarked: Earmarked aid (bilateral, earmarked to multilaterals, triangular co-operation)
#'   \item countryspecified: Country specific aid (income category not unspecified)
#'   \item geographically_specified: Geographically specific aid (main region not unspecified)
#'   \item earmarked_subsaharan: Earmarked aid to Sub-Saharan Africa
#'   \item gross_disbursed_nok: Gross disbursed amount in Norwegian kroner. Converts negative amounts extended to zero.
#'   \item disbursed: Disbursed amount in US dollars. Based on column disbursed_1000.
#' }
#'
#' @export
#'
#' @examples
#' ?add_cols_basic_clean()

add_cols_basic_clean <- function(data) {
  data  |>
    dplyr::mutate(
      # column target_area_no
      target_area_no = dplyr::case_when(
        target_area == "Economic infrastructure and services" ~ paste0(
          stringi::stri_unescape_unicode("\\u00d8"),
          "konomisk utvikling og tjenester"
        ),
        target_area == "Education" ~ "Utdanning",
        target_area == "Emergency assistance" ~ paste0("N", stringi::stri_unescape_unicode("\\u00f8"), "dhjelp"),
        target_area == "Environment and energy" ~ paste0(
          "Milj",
          stringi::stri_unescape_unicode("\\u00f8"),
          " og energi"
        ),
        target_area == "Governance, civil society and conflict prevention" ~ "Styresett, sivilt samfunn og konfliktforebygging",
        target_area == "Health and social services" ~ "Helse og sosiale tjenester",
        target_area == "In donor costs and unspecified" ~ "Kostnader i Norge og uspesifisert",
        target_area == "Multilateral" ~ paste0(
          "Multilateral kjernest",
          stringi::stri_unescape_unicode("\\u00f8"),
          "tte"
        ),
        target_area == "Multisector and other" ~ "Multisektor og annet",
        target_area == "Production sectors and trade" ~ "Produksjon og handel",
        .default = NA_character_
      ),
      
      # column partner_group_visual_no
      partner_group_visual_no = dplyr::case_when(
        group_of_agreement_partner == "Governments/Ministries in developing countries" |
          group_of_agreement_partner == "Public sector in developing countries" ~ "Offentlig sektor i mottakerland",
        group_of_agreement_partner == "Multilateral institutions" ~ "Multilaterale organisasjoner",
        group_of_agreement_partner == "NGO International" |
          group_of_agreement_partner == "NGO Local" |
          group_of_agreement_partner == "NGO Other donor countries" ~ "Andre ikke-statlige organisasjoner",
        group_of_agreement_partner == "NGO Norwegian" ~ "Norske ikke-statlige organisasjoner",
        group_of_agreement_partner == "Norwegian public sector" |
          group_of_agreement_partner == "Public sector other donor countries" ~ "Offentlig sektor i Norge/ andre giverland",
        group_of_agreement_partner == "Private sector in developing countries" |
          group_of_agreement_partner == "Private sector in other donor countries" |
          group_of_agreement_partner == "Norwegian private sector" |
          group_of_agreement_partner == "Other countries private sector" |
          group_of_agreement_partner == "Consultants"  ~ "Privat sektor",
        group_of_agreement_partner == "Public-private partnerships" |
          group_of_agreement_partner == "Network"  ~ "Offentlig-privat samarbeid og nettverk",
        group_of_agreement_partner == "Unknown" ~ "Uspesifisert",
        .default = NA_character_
      ),
      
      # column partner_group_visual
      partner_group_visual = dplyr::case_when(
        group_of_agreement_partner == "Governments/Ministries in developing countries" |
          group_of_agreement_partner == "Public sector in developing countries" ~ "Public sector in recipient country",
        group_of_agreement_partner == "Multilateral institutions" ~ "Multilateral organisations",
        group_of_agreement_partner == "NGO International" |
          group_of_agreement_partner == "NGO Local" |
          group_of_agreement_partner == "NGO Other donor countries" ~ "Other non-governmental organisations",
        group_of_agreement_partner == "NGO Norwegian" ~ "Norwegian non-governmental organisations",
        group_of_agreement_partner == "Norwegian public sector" |
          group_of_agreement_partner == "Public sector other donor countries" ~ "Public sector in Norway/other donors",
        group_of_agreement_partner == "Private sector in developing countries" |
          group_of_agreement_partner == "Private sector in other donor countries" |
          group_of_agreement_partner == "Norwegian private sector" |
          group_of_agreement_partner == "Other countries private sector" |
          group_of_agreement_partner == "Consultants"  ~ "Private sector",
        group_of_agreement_partner == "Public-private partnerships" |
          group_of_agreement_partner == "Network"  ~ "Public-private partnerships and networks",
        group_of_agreement_partner == "Unknown" ~ "Unspecified",
        .default = NA_character_
      ),
      
      # column main_region_no
      main_region_no = dplyr::case_when(
        main_region == "Africa" ~ "Afrika",
        main_region == "Asia" ~ "Asia",
        main_region == "Not geographically allocated" ~ "Geografisk uspesifisert",
        main_region == "Europe" ~ "Europa",
        main_region == "America" ~ "Amerika",
        main_region == "The Middle East" ~ paste0("Midt", stringi::stri_unescape_unicode("\\u00f8"), "sten"),
        main_region == "Oceania" ~ "Oseania",
        .default = NA_character_
      ),
      
      # column income_category_no
      income_category_no = dplyr::case_when(
        income_category == "Upper Middle-Income Countries" ~ paste0(
          "H",
          stringi::stri_unescape_unicode("\\u00f8"),
          "yere mellominntektsland"
        ),
        income_category == "Lower Middle-Income Countries" ~ "Lavere mellominntektsland",
        income_category == "Low-Income Countries " ~ "Lavinntektsland",
        income_category == "Least Developed Countries" ~ "Minst utviklede land",
        income_category == "Unspecified" ~ "Uspesifisert",
        .default = NA_character_
      ),
      
      # column type_of_assistance_group
      type_of_assistance_group = dplyr::case_when(
        type_of_assistance %in% c(
          "Bilateral",
          "Earmarked to multilaterals",
          "Triangular co-operation"
        ) ~ "Earmarked support",
        .default = type_of_assistance
      ),
      
      # column earmarked
      earmarked = type_of_assistance %in% c(
        "Bilateral",
        "Earmarked to multilaterals",
        "Triangular co-operation"
      ),
      
      # column countryspecific
      countryspecified = income_category != "Unspecified",
      
      # column geospecific
      geographically_specified = main_region != "Not geographically allocated",
      
      # column earmarked_subsaharan
      earmarked_subsaharan = main_region == "Africa" &
        !recipient_country %in% c(
          "Algeria",
          "Egypt",
          "Libya",
          "Morocco",
          "Tunisia",
          "North of Sahara, regional",
          "Africa Regional"
        ),
      
      # column gross_disbursed_nok
      gross_disbursed_nok = dplyr::if_else(
        amounts_extended_1000_nok < 0,
        0,
        amounts_extended_1000_nok * 1e3
      ),
      
      # column disbursed
      disbursed = disbursed_1000 * 1e3
    )
}
