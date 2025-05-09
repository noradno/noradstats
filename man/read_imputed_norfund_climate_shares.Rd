% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_imputed_norfund_climate_shares.R
\name{read_imputed_norfund_climate_shares}
\alias{read_imputed_norfund_climate_shares}
\title{Read Imputed Norfund Climate Shares data into R}
\usage{
read_imputed_norfund_climate_shares()
}
\value{
Returns a tibble with two columns: `agreement_number`, `year`, and `climate_share` from the 'imputed_norfund_climate_share' table in the DuckDB database.
}
\description{
This function imports from the DuckDB database a data frame of the annual imputed Norfund climate shares (2 year averages) from 2015 and later.
}
\examples{
\dontrun{
# Read the imputed_norfund_climate_share table from the DuckDB database:
df_imputed_norfund_climate_shares <- read_imputed_norfund_climate_shares()

# Display the first few rows of the data:
head(df_imputed_norfund_climate_shares)
}
}
