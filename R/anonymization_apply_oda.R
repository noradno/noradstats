#' Apply anonymisation mappings to ODA disbursement-level data
#'
#' Applies partner/impl mappings and masks title/description for flagged agreements.
#'
#' @param df_oda_disbursements ODA disbursement-level data.
#' @param maps Output from [build_anonymization_maps()].
#'
#' @return An anonymised ODA tibble.
#' @keywords internal
apply_anonymization_maps_oda <- function(df_oda_disbursements, maps) {
  
  # Require the ODA columns that will be masked
  stopifnot(all(c(
    "agreement_number", "agreement_partner", "impl_inst",
    "agreement_title", "description_of_agreement"
  ) %in% names(df_oda_disbursements)))
  
  txt <- maps$anonymized_text
  
  # Apply partner mapping using agreement_number -> agreement_no normalization
  df <- df_oda_disbursements |>
    dplyr::mutate(agreement_no = agreement_number) |>
    dplyr::left_join(maps$df_partner_map, by = "agreement_no") |>
    dplyr::mutate(
      agreement_partner = dplyr::if_else(!is.na(partner_masked), partner_masked, agreement_partner)
    ) |>
    dplyr::select(-agreement_no, -partner_masked)
  
  # Apply implementing institution mapping from ODA-derived decisions
  df <- df |>
    dplyr::mutate(agreement_no = agreement_number) |>
    dplyr::left_join(maps$df_impl_map, by = "agreement_no") |>
    dplyr::mutate(
      impl_inst = dplyr::if_else(!is.na(impl_masked), impl_masked, impl_inst)
    ) |>
    dplyr::select(-agreement_no, -impl_masked)
  
  # Mask title/description for agreements flagged in the decision set
  df |>
    dplyr::mutate(
      agreement_title = dplyr::if_else(agreement_number %in% maps$vec_mask_title_desc, txt, agreement_title),
      description_of_agreement = dplyr::if_else(agreement_number %in% maps$vec_mask_title_desc, txt, description_of_agreement)
    )
}
