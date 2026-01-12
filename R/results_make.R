# Internal builder functions for SharePoint-based results datasets (not exported)
#
# These functions transform raw SharePoint Lists (via read_sp_list()) into
# analysis-ready tibbles. The resulting datasets are written to DuckDB by
# create_sharepoint_results_to_db().

# ---- Agreement-level builders ----

# AgreementPortfolio -> agreement to portfolio objective mapping (long format)
# Returns: agreement_no (chr), port_obj_name (chr)
make_agr_portobj <- function() {
  read_sp_list("AgreementPortfolio") |>
    dplyr::select(title, objective_level1) |>
    tidyr::unnest_longer(objective_level1, names_repair = "check_unique") |>
    dplyr::rename(
      agreement_no  = title,
      port_obj_name = objective_level1
    )
}

# AgreementPortfolioProgressAssessment -> assessments per agreement + portfolio objective
# Returns: agreement_no (chr), assessment_year (int), port_obj_name (chr),
#          assessment_type (chr), assessment (chr)
make_agr_portassessments <- function() {
  read_sp_list("AgreementPortfolioProgressAssessment") |>
    dplyr::select(
      title,
      assessment_year,
      objective,
      assessment_type,
      assessment
    ) |>
    dplyr::rename(
      agreement_no  = title,
      port_obj_name = objective
    ) |>
    dplyr::mutate(
      assessment_year = as.integer(assessment_year)
    )
}

# AgreementProgressAssessment -> assessment text split into aspect (Positives/Negatives/Conclusion)
# and scope (Internal/Public). Keeps only rows where assessment text is present.
make_agr_assessments <- function() {
  read_sp_list("AgreementProgressAssessment") |>
    dplyr::select(
      title,
      assessment_year,
      assessment_type,
      assessment_score,         # meaningful for Internal scope
      public_confirmed,         # meaningful for Public scope
      assessment_positives_internal,
      assessment_negatives_internal,
      assessment_conclusion_internal,
      assessment_positives_public,
      assessment_negatives_public,
      assessment_conclusion_public
    ) |>
    dplyr::rename(agreement_no = title) |>
    dplyr::mutate(
      assessment_year  = as.integer(assessment_year),
      assessment_score = as.integer(assessment_score),
      # Coerce to logical early; SharePoint often returns TRUE/FALSE or Yes/No
      public_confirmed = as.logical(public_confirmed)
    ) |>
    tidyr::pivot_longer(
      cols = c(
        assessment_positives_internal, assessment_negatives_internal, assessment_conclusion_internal,
        assessment_positives_public,   assessment_negatives_public,   assessment_conclusion_public
      ),
      names_to      = c("aspect", "scope"),
      names_pattern = "^assessment_(positives|negatives|conclusion)_(internal|public)$",
      values_to     = "assessment"
    ) |>
    dplyr::mutate(
      scope = dplyr::recode(scope, internal = "Internal", public = "Public"),
      aspect = dplyr::recode(
        aspect,
        positives  = "Positives",
        negatives  = "Negatives",
        conclusion = "Conclusion"
      ),
      # Keep score only for Internal rows; integer NA elsewhere
      assessment_score = dplyr::if_else(
        scope == "Internal",
        assessment_score,
        NA_integer_
      ),
      # Keep public_confirmed only for Public rows; logical NA elsewhere
      public_confirmed = dplyr::if_else(
        scope == "Public",
        public_confirmed,
        NA
      )
    ) |>
    dplyr::filter(!is.na(assessment))
}

# ---- Portfolio-level builders ----

# PortfolioObjectives -> high-level objectives
# Returns: port (chr), port_obj_highlevel_name (chr)
make_port_portobj_highlevel <- function() {
  read_sp_list("PortfolioObjectives") |>
    dplyr::filter(objective_level == "High-Level Objective") |>
    dplyr::select(portfolio, objective) |>
    dplyr::rename(
      port                    = portfolio,
      port_obj_highlevel_name = objective
    )
}

# PortfolioObjectives -> level 1 objectives
# Returns: port (chr), port_obj_name (chr)
make_port_portobj <- function() {
  read_sp_list("PortfolioObjectives") |>
    dplyr::filter(objective_level == "Objective Level 1") |>
    dplyr::select(portfolio, objective) |>
    dplyr::rename(
      port         = portfolio,
      port_obj_name = objective
    )
}

# PortfolioAssessment -> portfolio-level assessments (wide)
# Returns: portfolio (chr), assessment_year (int), assessment fields + measures
make_portfolio_analysis <- function() {
  read_sp_list("PortfolioAssessment") |>
    dplyr::select(
      title,
      assessment_year,
      general_remarks,
      results_assessment,
      impact_assessment,
      composition_assessment,
      context_knowledge_assessment,
      conclusion,
      public_assessment,
      general_remarks_measures,
      public_confirmed,
      results_assessment_measures,
      context_knowledge_assessment_measur,
      composition_assessment_measures
    ) |>
    dplyr::rename(portfolio = title) |>
    dplyr::mutate(
      assessment_year  = as.integer(assessment_year),
      public_confirmed = as.logical(public_confirmed)
    )
}
