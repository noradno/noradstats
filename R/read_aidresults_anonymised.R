#' Read anonymised datasets for aid results publication
#'
#' Reads the relevant datasets from DuckDB via noradstats readers and applies
#' a consistent anonymisation approach across sources:
#' - ODA disbursement-level data (historical, 1960..last year)
#' - PTA disbursement-level data (current year and forward)
#' - PTA agreement totals (agreement-level, 1960..today)
#'
#' @return A named list with three tibbles:
#'   \describe{
#'     \item{df_oda_disbursements}{Anonymised ODA disbursement-level data.}
#'     \item{df_pta_disbursements}{Anonymised PTA disbursement-level data.}
#'     \item{df_pta_agreement_totals}{Anonymised PTA agreement totals data.}
#'   }
#' @export
read_aidresults_anonymised <- function() {
  
  # Read raw datasets from the noradstats DuckDB layer
  df_oda <- read_oda()
  df_pta_disb <- read_pta_disbursement_level()
  df_pta_totals <- read_pta_agreement_totals()
  
  # Use stable anonymisation rules defined in noradstats
  rules <- anonymization_rules()
  
  # Derive a single set of anonymisation mappings from ODA + PTA context
  maps <- build_anonymization_maps(df_oda, df_pta_disb, rules)
  
  # Apply the same mappings to each dataset to ensure consistent masking
  list(
    df_oda_disbursements = apply_anonymization_maps_oda(df_oda, maps),
    df_pta_disbursements = apply_anonymization_maps_pta_disbursements(df_pta_disb, maps),
    df_pta_agreement_totals = apply_anonymization_maps_agreement_totals(df_pta_totals, maps)
  )
}
