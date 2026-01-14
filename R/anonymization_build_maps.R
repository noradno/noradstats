#' Build anonymisation mappings for aid results publication
#'
#' Derives which agreements must be anonymised, and how, based on:
#' - ODA disbursements (full context: recipient country + partner/impl groups)
#' - PTA disbursements (adds decisions for new agreements not present in ODA)
#'
#' @param df_oda_disbursements ODA disbursement-level data.
#' @param df_pta_disbursements PTA disbursement-level data.
#' @param rules Output from [anonymization_rules()].
#'
#' @return A named list containing:
#'   \describe{
#'     \item{anonymized_text}{Masking label.}
#'     \item{df_partner_map}{Mapping agreement_no -> partner_masked.}
#'     \item{df_impl_map}{Mapping agreement_no -> impl_masked (ODA-derived).}
#'     \item{vec_mask_title_desc}{Agreement numbers requiring masked title/description.}
#'   }
#' @keywords internal
build_anonymization_maps <- function(df_oda_disbursements, df_pta_disbursements, rules) {
  
  # Require the minimal ODA columns used to derive anonymisation decisions
  stopifnot(all(c(
    "agreement_number", "year", "recipient_country",
    "group_of_agreement_partner", "group_of_impl_inst", "type_of_agreement"
  ) %in% names(df_oda_disbursements)))
  
  # Require the minimal PTA columns used to derive decisions for new agreements
  stopifnot(all(c(
    "agreement_no", "recipient_country", "agreement_partner_group"
  ) %in% names(df_pta_disbursements)))
  
  txt <- rules$anonymized_text
  
  # Identify ODA agreements triggering anonymisation (country + year)
  vec_oda_agreements <- df_oda_disbursements |>
    dplyr::filter(
      recipient_country %in% rules$recipient_countries,
      year >= rules$oda_min_year
    ) |>
    dplyr::distinct(agreement_number) |>
    dplyr::pull(agreement_number)
  
  # Build partner mapping from ODA (standard agreements only)
  df_partner_map_oda <- df_oda_disbursements |>
    dplyr::filter(
      agreement_number %in% vec_oda_agreements,
      type_of_agreement == "standard",
      group_of_agreement_partner %in% rules$partner_groups
    ) |>
    dplyr::distinct(agreement_number, group_of_agreement_partner) |>
    dplyr::mutate(
      agreement_no = agreement_number,
      partner_masked = paste0(txt, " - ", group_of_agreement_partner)
    ) |>
    dplyr::select(agreement_no, partner_masked)
  
  # Build implementing institution mapping from ODA
  df_impl_map_oda <- df_oda_disbursements |>
    dplyr::filter(
      agreement_number %in% vec_oda_agreements,
      group_of_impl_inst %in% rules$partner_groups
    ) |>
    dplyr::distinct(agreement_number, group_of_impl_inst) |>
    dplyr::mutate(
      agreement_no = agreement_number,
      impl_masked = paste0(txt, " - ", group_of_impl_inst)
    ) |>
    dplyr::select(agreement_no, impl_masked)
  
  # Agreements that must have title/description masked (derived from ODA)
  vec_mask_title_desc_oda <- union(df_partner_map_oda$agreement_no, df_impl_map_oda$agreement_no)
  
  # Identify PTA agreements not already covered by ODA-derived decisions
  vec_new_agreements <- df_pta_disbursements |>
    dplyr::distinct(agreement_no) |>
    dplyr::filter(!(agreement_no %in% vec_mask_title_desc_oda)) |>
    dplyr::pull(agreement_no)
  
  # Build partner mapping for *new* agreements using PTA context
  df_partner_map_pta_new <- df_pta_disbursements |>
    dplyr::filter(
      agreement_no %in% vec_new_agreements,
      recipient_country %in% rules$recipient_countries,
      agreement_partner_group %in% rules$partner_groups
    ) |>
    dplyr::distinct(agreement_no, agreement_partner_group) |>
    dplyr::mutate(
      partner_masked = paste0(txt, " - ", agreement_partner_group)
    ) |>
    dplyr::select(agreement_no, partner_masked)
  
  # New agreements masked via partner mapping should also have title/description masked
  vec_mask_title_desc_pta_new <- df_partner_map_pta_new$agreement_no
  
  # Combine partner maps (PTA only contributes for new agreements)
  df_partner_map <- dplyr::bind_rows(df_partner_map_oda, df_partner_map_pta_new) |>
    dplyr::distinct(agreement_no, .keep_all = TRUE)
  
  list(
    anonymized_text = txt,
    df_partner_map = df_partner_map,
    df_impl_map = dplyr::distinct(df_impl_map_oda, agreement_no, .keep_all = TRUE),
    vec_mask_title_desc = union(vec_mask_title_desc_oda, vec_mask_title_desc_pta_new)
  )
}
