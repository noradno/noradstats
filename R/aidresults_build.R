# ------------------------------------------------------------------------------
# Aidresults: build and export publication datasets
# ------------------------------------------------------------------------------

# ---- Column schemas (internal names; publication contract) -------------------

aidresults_cols_oda <- c(
  "agreement_number", "project_number", "responsible_unit", "agreement_title",
  "agreement_partner", "group_of_agreement_partner", "group_of_agreement_partner_code",
  "channel_code_id", "teaching_research_institution", "impl_inst",
  "group_of_impl_inst", "agreement_period", "agreement_signed",
  "agr_compl_date", "commited_1000_nok", "commited_1000", "commited_acc_1000_nok",
  "commited_acc_1000", "agreement_amount_1000_nok", "agreement_amount_1000",
  "description_of_agreement", "dac_main_sector_code", "dac_main_sector_code_name",
  "dac_sub_sector_code", "dac_sub_sector_code_name", "target_area",
  "recipient_country", "recipient_country_no", "recipient_country_crs",
  "country_group_no", "main_region", "form_of_assistance_code",
  "form_of_assistance", "type_of_assistance_code", "type_of_assistance",
  "agreement_phase_code", "agreement_phase", "type_of_flow", "type_of_agreement",
  "amount_tied_1000_nok", "pm_environment", "pm_climate_change_adaptation",
  "pm_reproductive_maternal_newborn_and_child_health",
  "pm_research_and_experimental_development",
  "pm_disaster_risk_reduction",
  "pm_inclusion_and_empowerment_of_persons_with_disabilities",
  "pm_gender_equality", "pm_democracy_and_inclusive_governance",
  "pm_bio_diversity", "pm_desertification", "pm_climate_change_mitigation",
  "fa_covid_19", "sdg_focus", "sdg_main_target", "sdg_description",
  "extending_agency", "income_category", "program_category_code",
  "program_category_code_name", "chapter_code", "chapter_code_name",
  "post_code", "post_code_name", "sub_post_code", "sub_post", "year",
  "disbursed_nok", "disbursed_1000_nok", "disbursed_mill_nok",
  "disbursed_mrd_nok", "disbursed_1000", "disbursed_mill",
  "index_disbursed_1000", "amounts_extended_1000_nok",
  "amounts_extended_1000_usd", "amounts_received_1000_nok",
  "amounts_received_1000_usd", "planned_disbursed_1000_nok"
)

aidresults_cols_pta_disbursements <- c(
  "disb_code", "agreement_no", "agreement_title", "group_no",
  "group_title", "agr_phase", "year", "date", "status_of_payment",
  "amount", "agio", "agreement_period_from", "agreement_period_to",
  "recipient", "type_of_disbursement", "cost_center", "cost_center_name",
  "chapter", "chapter_post", "chapter_post_sub", "chapter_post_sub_name",
  "recipient_country", "case_no", "programme_area", "agreement_tag",
  "agreement_signed_date", "agreement_partner", "impl_institution",
  "agreement_partner_group", "grant_mgmt_regime_code",
  "grant_mgmt_regime_description", "type_of_assistance",
  "form_of_assistance", "sector", "main_sector",
  "main_sector_description", "sub_sector", "sub_sector_description",
  "special_project", "covid_19", "environment", "gender_equality",
  "democracy_and_inclusive_governance", "bio_diversity",
  "research_and_experimental_development", "desertification",
  "climate_change_mitigation", "climate_change_adaptation",
  "reproductive_maternal_newborn_and_child_health",
  "disaster_risk_reduction",
  "inclusion_and_empowerment_of_persons_with_disabilities",
  "agreement_description", "result"
)

aidresults_cols_pta_agreement_totals <- c(
  "agreement_no", "agreement_title", "agreement_partner", "agr_period",
  "estimated_amount", "agreed_original_amount",
  "agreed_additional_grant", "total_agreed",
  "expected_agreement_total", "disbursed",
  "total_prognosis", "not_planned"
)

# ---- Prep functions ----------------------------------------------------------

#' Prepare ODA dataset for aidresults
#'
#' Builds the ODA (historical, disbursement-level) dataset used by aidresults.
#' Applies anonymisation mappings, selects the publication schema, and renames
#' columns to official English names.
#'
#' @param maps Optional. Output from [build_anonymization_maps()]. If not provided,
#'   maps will be built from required source datasets internally.
#' @param lang Language for official column names. Defaults to "en".
#' @return A tibble ready for publication.
#' @export
prep_aidresults_oda <- function(maps = NULL, lang = "en") {
  
  df_oda <- read_oda()
  
  if (is.null(maps)) {
    df_pta_disb <- read_pta_disbursement_level()
    rules <- anonymization_rules()
    maps <- build_anonymization_maps(
      df_oda_disbursements = df_oda,
      df_pta_disbursements = df_pta_disb,
      rules = rules
    )
  }
  
  # Apply shared anonymisation mappings
  df_oda <- apply_anonymization_maps_oda(df_oda, maps = maps)
  
  df_oda |>
    dplyr::select(dplyr::all_of(aidresults_cols_oda)) |>
    rename_to_official(dataset = "oda", lang = lang)
}

#' Prepare PTA disbursement-level dataset for aidresults
#'
#' Builds the PTA disbursement-level dataset (current year and forward).
#' Applies anonymisation mappings and formats the dataset for publication.
#'
#' @param maps Optional. Output from [build_anonymization_maps()]. If not provided,
#'   maps will be built from required source datasets internally.
#' @param lang Language for official column names. Defaults to "en".
#' @return A tibble ready for publication.
#' @export
prep_aidresults_pta_disbursements <- function(maps = NULL, lang = "en") {
  
  df_pta <- read_pta_disbursement_level()
  
  if (is.null(maps)) {
    df_oda <- read_oda()
    rules <- anonymization_rules()
    maps <- build_anonymization_maps(
      df_oda_disbursements = df_oda,
      df_pta_disbursements = df_pta,
      rules = rules
    )
  }
  
  # Apply shared anonymisation mappings
  df_pta <- apply_anonymization_maps_pta_disbursements(df_pta, maps = maps)
  
  df_pta |>
    dplyr::select(dplyr::all_of(aidresults_cols_pta_disbursements)) |>
    rename_to_official(dataset = "pta_disbursements", lang = lang)
}

#' Prepare PTA agreement totals dataset for aidresults
#'
#' Builds the PTA agreement-level totals dataset (historical and current).
#' Applies anonymisation mappings and formats the dataset for publication.
#'
#' @param maps Optional. Output from [build_anonymization_maps()]. If not provided,
#'   maps will be built from required source datasets internally.
#' @param lang Language for official column names. Defaults to "en".
#' @return A tibble ready for publication.
#' @export
prep_aidresults_pta_agreement_totals <- function(maps = NULL, lang = "en") {
  
  df_totals <- read_pta_agreement_totals()
  
  if (is.null(maps)) {
    df_oda <- read_oda()
    df_pta_disb <- read_pta_disbursement_level()
    rules <- anonymization_rules()
    maps <- build_anonymization_maps(
      df_oda_disbursements = df_oda,
      df_pta_disbursements = df_pta_disb,
      rules = rules
    )
  }
  
  # Apply shared anonymisation mappings
  df_totals <- apply_anonymization_maps_agreement_totals(df_totals, maps = maps)
  
  df_totals |>
    dplyr::select(dplyr::all_of(aidresults_cols_pta_agreement_totals)) |>
    rename_to_official(dataset = "pta_agreement_totals", lang = lang)
}

# ---- Build + export ----------------------------------------------------------

#' Build all aidresults datasets
#'
#' Builds all datasets used by aidresults and returns them as a named list.
#' Column names are returned in English by default.
#'
#' @param lang Language for official column names. Defaults to "en".
#' @return A named list with tibbles for each dataset.
#' @export
build_aidresults_datasets <- function(lang = "en") {
  
  # Read core sources once and build maps consistently
  df_oda <- read_oda()
  df_pta_disb <- read_pta_disbursement_level()
  
  rules <- anonymization_rules()
  
  maps <- build_anonymization_maps(
    df_oda_disbursements = df_oda,
    df_pta_disbursements = df_pta_disb,
    rules = rules
  )
  
  list(
    oda =
      df_oda |>
      apply_anonymization_maps_oda(maps = maps) |>
      dplyr::select(dplyr::all_of(aidresults_cols_oda)) |>
      rename_to_official(dataset = "oda", lang = lang),
    
    pta_disbursements =
      df_pta_disb |>
      apply_anonymization_maps_pta_disbursements(maps = maps) |>
      dplyr::select(dplyr::all_of(aidresults_cols_pta_disbursements)) |>
      rename_to_official(dataset = "pta_disbursements", lang = lang),
    
    pta_agreement_totals =
      read_pta_agreement_totals() |>
      apply_anonymization_maps_agreement_totals(maps = maps) |>
      dplyr::select(dplyr::all_of(aidresults_cols_pta_agreement_totals)) |>
      rename_to_official(dataset = "pta_agreement_totals", lang = lang)
  )
}

#' Export aidresults datasets to Excel files
#'
#' Builds all aidresults datasets and writes them to Excel files in a directory.
#' This is the main entry point for publishing the current aidresults files.
#'
#' @param path_dir Output directory.
#' @param lang Language for official column names. Defaults to "en".
#' @return Invisibly returns the built datasets.
#' @export
export_aidresults_xlsx <- function(path_dir, lang = "en") {
  
  datasets <- build_aidresults_datasets(lang = lang)
  
  if (!dir.exists(path_dir)) {
    dir.create(path_dir, recursive = TRUE)
  }
  
  writexl::write_xlsx(
    datasets$oda,
    file.path(path_dir, "statsys_aktiv.xlsx")
  )
  
  writexl::write_xlsx(
    datasets$pta_disbursements,
    file.path(path_dir, "pta_disbursement_level.xlsx")
  )
  
  writexl::write_xlsx(
    datasets$pta_agreement_totals,
    file.path(path_dir, "pta_agreement_totals.xlsx")
  )
  
  message(
    "Aidresults files written to: ", normalizePath(path_dir),
    "\n- statsys_aktiv.xlsx",
    "\n- pta_disbursement_level.xlsx",
    "\n- pta_agreement_totals.xlsx"
  )
  
  invisible(datasets)
}

