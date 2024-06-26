% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_oda.R
\name{access_oda}
\alias{access_oda}
\title{Access database of Norwegian ODA data from R}
\usage{
access_oda()
}
\value{
Returns a remote tibble connected to the the Statsys table in the DuckDB database. The remote tibble is filtered to include ODA data. Frame agreement level data is excluded.
}
\description{
This function creates a proxy tibble connected to the Statsys table in the DuckDB database, and filters the proxy tibble to only include Norwegian Official Development (ODA). Frame agreement level data is excluded.
The data covers 1960 to recent year.
The DuckDB database file is located on Norads Microsoft Sharepoint site and is expected to be synced via Microsoft Teams to to the users local directory.
Use DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.
}
\examples{
?access_oda()

}
