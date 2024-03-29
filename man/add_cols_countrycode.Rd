% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_cols_countrycode.R
\name{add_cols_countrycode}
\alias{add_cols_countrycode}
\title{Add ISO3 country code columns to dataframe}
\usage{
add_cols_countrycode(data)
}
\arguments{
\item{data}{Input data frame of Norwegian development assistance, with column \emph{Recipient country} containing country names.}
}
\value{
Returns data frame with additional column:
\itemize{
  \item iso3: iso3 character code, identified using package \emph{countrycode} based on column \emph{Recipient country}. Non-matches are given NA values and are returned in a warning message. 
}
}
\description{
Add ISO3 country code columns to dataframe
}
\examples{
?add_cols_countrycode()
}
