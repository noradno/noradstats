#' Apply anonymisation mappings to PTA agreement totals data
#'
#' Agreement totals lacks recipient country and partner groups, so masking is applied
#' only by agreement_no using the derived mapping tables.
#'
#' @param df_pta_agreement_totals PTA agreement totals data.
#' @param maps Output from [build_anonymization_maps()].
#'
#' @return An anonymised agreement totals tibble.
#' @keywords internal
apply_anonymization_maps_agreement_totals <- function(df_pta_agreement_totals, maps) {
  
  # Require the totals columns that will be masked
  stopifnot(all(c("agreement_no", "agreement_partner", "agreement_title") %in% names(df_pta_agreement_totals)))
  
  txt <- maps$anonymized_text
  
  # Apply partner mapping by agreement_no
  df <- df_pta_agreement_totals |>
    dplyr::left_join(maps$df_partner_map, by = "agreement_no") |>
    dplyr::mutate(
      agreement_partner = dplyr::if_else(!is.na(partner_masked), partner_masked, agreement_partner)
    ) |>
    dplyr::select(-partner_masked)
  
  # Mask title for agreements flagged in the decision set
  df |>
    dplyr::mutate(
      agreement_title = dplyr::if_else(agreement_no %in% maps$vec_mask_title_desc, txt, agreement_title)
    )
}
