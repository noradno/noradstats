#' Apply anonymisation mappings to PTA disbursement-level data
#'
#' Applies partner mapping (ODA + PTA-new) and impl mapping (ODA-derived), and masks
#' title/description for flagged agreements.
#'
#' @param df_pta_disbursements PTA disbursement-level data.
#' @param maps Output from [build_anonymization_maps()].
#'
#' @return An anonymised PTA disbursement-level tibble.
#' @keywords internal
apply_anonymization_maps_pta_disbursements <- function(df_pta_disbursements, maps) {
  
  # Require the PTA columns that will be masked
  stopifnot(all(c(
    "agreement_no", "agreement_partner", "impl_institution",
    "agreement_title", "agreement_description"
  ) %in% names(df_pta_disbursements)))
  
  txt <- maps$anonymized_text
  
  # Apply partner mapping by agreement_no
  df <- df_pta_disbursements |>
    dplyr::left_join(maps$df_partner_map, by = "agreement_no") |>
    dplyr::mutate(
      agreement_partner = dplyr::if_else(!is.na(partner_masked), partner_masked, agreement_partner)
    ) |>
    dplyr::select(-partner_masked)
  
  # Apply implementing institution mapping by agreement_no
  df <- df |>
    dplyr::left_join(maps$df_impl_map, by = "agreement_no") |>
    dplyr::mutate(
      impl_institution = dplyr::if_else(!is.na(impl_masked), impl_masked, impl_institution)
    ) |>
    dplyr::select(-impl_masked)
  
  # Mask title/description for agreements flagged in the decision set
  df |>
    dplyr::mutate(
      agreement_title = dplyr::if_else(agreement_no %in% maps$vec_mask_title_desc, txt, agreement_title),
      agreement_description = dplyr::if_else(agreement_no %in% maps$vec_mask_title_desc, txt, agreement_description)
    )
}
