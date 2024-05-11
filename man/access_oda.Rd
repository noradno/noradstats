% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_oda.R
\name{access_oda}
\alias{access_oda}
\title{Access database of Norwegian ODA data from R}
\usage{
access_oda()
}
\value{
Returns a remote tibble data frame linked to the Statsys database table.
}
\description{
This function connects to a specified DuckDB database table to retrieve Norwegian Official Development (ODA) data from 1960 to the recent year.
The database file is expected to be located in a synchronized SharePoint directory on the user's local computer.
Use DBI::dbDisconnect(con, shutdown=TRUE) to close connection to database.
}
\examples{
?access_oda()

}