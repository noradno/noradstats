#' Add LDCs columns to an existing oda data frame
#'
#' This function takes an existing oda data frame as input and adds LDCs columns.
#' The function returns the oda data frame with the following additional columns for LDCs:
#' \describe{
#'   \item{\code{ldcs_nok}}{Numeric variable of total disbursed oda to LDCs (earmarked and imputed multilateral).}
#'   \item{\code{ldcs_tag}}{Logical variable to identify LDCs activities, meaning earmarked ODA for LDCs and imputed multilateral ODA for LDCs}
#'   \item{\code{ldcs_channel}}{Categorical variable to separate earmarked oda to LDCs from imputed multilateral oda to LDCs}
#' }
#'
#' ## Important:
#' Before running this function, you must have already loaded the oda data frame by using
#' \code{noradstats::read_oda()}.
#' 
#' @import dplyr
#' @importFrom noradstats read_imputed_multi_org_incomegroup_shares
#' @param df_oda A oda data frame, which must already be loaded into the environment.
#' @return A oda data frame with additional LDCs columns.
#' @export
#'
#' @examples
#' # Load the oda data
#' df_oda <- read_oda()
#'
#' # Add LDCs column to the df_oda data
#' df_oda_ldcs <- add_cols_ldcs(df_oda)
#'
#' # Ensure that the df_oda data frame is available before running this function,
#' # using the `read_oda()` function
add_cols_ldcs <- function(df_oda) {

  # Check that the input data contain ODA data only and give warning if other types flows are also included.
  if (any(df_oda$type_of_flow != "ODA")) {
    warning("Attention: The data frame should contain observations where 'type_of_flow' is 'ODA' only.
    Please ensure you have used `read_oda()` and not `read_statsys()`.")
  }

  # Import imputed multi org income gorup shares from DuckDB and filter for LDCs shares
  df_multi_ldcs <- read_imputed_multi_org_incomegroup_shares() |> 
    filter(income_group == "LDCs") |> 
    rename(imputed_multi_org_incomegroup_share = share) |> 
    select(agreement_partner, year, imputed_multi_org_incomegroup_share)
  
  # Join the imputed_multi_org_incomegroup_share column to oda data
  df_oda <- df_oda |> 
    left_join(df_multi_ldcs, join_by(agreement_partner == agreement_partner, year == year))
  
# Logical variable to identify LDCs activities
df_oda <- df_oda |>
  mutate(
    ldcs_tag = case_when(
      # Earmarked ODA to LDCs
      income_category == "Least Developed Countries" ~ TRUE,
    
      # Imputed multi core contributions to LDCs (should be improved)
      type_of_assistance == "Core contributions to multilat" & 
        agreement_partner %in% df_multi_ldcs$agreement_partner &
        year %in% df_multi_ldcs$year ~ TRUE,
      
      # Other activities (FALSE)
      .default = FALSE
    )
  )

# Numeric variable of ldcs_nok. Return zeros instead of NAs for core contributions with missing LDCs share.
  df_oda <- df_oda |> 
    mutate(
      # Earmarked oda to LDCs
      ldcs_nok = case_when(
        ldcs_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ disbursed_nok,
      # Imputed multilateral to LDCs
        ldcs_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ disbursed_nok * coalesce(imputed_multi_org_incomegroup_share, 0),
        .default = 0
      )
    )

# Categorical variables
df_oda <- df_oda |>
  mutate(
    # Categorical variable for channel of ODA to LDCs: earmarked and imputed multilateral
    ldcs_channel = case_when(
      ldcs_tag == TRUE & type_of_assistance != "Core contributions to multilat" ~ "Earmarked ODA to LDCs",
      ldcs_tag == TRUE & type_of_assistance == "Core contributions to multilat" ~ "Imputed multilateral ODA to LDCs",
      .default = NA
    )
  )

# Relocate columns
df_oda <- df_oda |> 
  relocate(imputed_multi_org_incomegroup_share, .after = last_col())

  # Return the final dataframe with the additional LDCs columns
  return(df_oda)
}
