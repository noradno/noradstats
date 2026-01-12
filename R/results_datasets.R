# Results datasets registry
#
# This file defines the canonical registry of SharePoint-based results datasets.
#
# Responsibilities:
# - declares which results datasets exist
# - documents their expected schema (see below)
# - maps each dataset to a make_*() builder function (defined in results_make.R)
#
# This registry is consumed by:
# - create_sharepoint_results_to_db() to build and write tables to DuckDB
# - read_sharepoint_results() to read the tables back into R

# The schemas below describe the expected structure and column types produced by the functions defined in results_make.R
#
# results_agr_portobj
# - agreement_no            <chr>
# - port_obj_name           <chr>
#
# results_agr_portassessments
# - agreement_no            <chr>
# - assessment_year         <int>
# - port_obj_name           <chr>
# - assessment_type         <chr>
# - assessment              <chr>
#
# results_agr_assessments
# - agreement_no            <chr>
# - assessment_year         <int>
# - assessment_type         <chr>
# - assessment_score        <int>   (NA except Internal scope)
# - public_confirmed        <lgl>   (NA except Public scope)
# - aspect                  <chr>   ("Positives", "Negatives", "Conclusion")
# - scope                   <chr>   ("Internal", "Public")
# - assessment              <chr>   (non-NA only)
#
# results_port_portobj
# - port                    <chr>
# - port_obj_name           <chr>
#
# results_port_portobj_highlevel
# - port                    <chr>
# - port_obj_highlevel_name <chr>
#
# results_portfolio_analysis
# - portfolio                         <chr>
# - assessment_year                   <int>
# - general_remarks                   <chr>
# - results_assessment                <chr>
# - impact_assessment                 <chr>
# - composition_assessment            <chr>
# - context_knowledge_assessment      <chr>
# - conclusion                        <chr>
# - public_assessment                 <chr>
# - general_remarks_measures          <chr>
# - public_confirmed                  <lgl>
# - results_assessment_measures       <chr>
# - context_knowledge_assessment_measur <chr>
# - composition_assessment_measures   <chr>


# Not exported

results_datasets <- function() {
  list(
    agr_portobj = list(
      table   = "results_agr_portobj",
      builder = make_agr_portobj
    ),
    agr_portassessments = list(
      table   = "results_agr_portassessments",
      builder = make_agr_portassessments
    ),
    agr_assessments = list(
      table   = "results_agr_assessments",
      builder = make_agr_assessments
    ),
    port_portobj = list(
      table   = "results_port_portobj",
      builder = make_port_portobj
    ),
    port_portobj_highlevel = list(
      table   = "results_port_portobj_highlevel",
      builder = make_port_portobj_highlevel
    ),
    portfolio_analysis = list(
      table   = "results_portfolio_analysis",
      builder = make_portfolio_analysis
    )
  )
}
