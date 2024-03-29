% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_imputed_sectors.R
\name{get_imputed_sectors}
\alias{get_imputed_sectors}
\title{Get data on Norwegian imputed multilateral ODA to sectors by year, using the OECD SDMX API. Amounts in USD million and NOK million.}
\usage{
get_imputed_sectors(startyear = 2020, endyear = 2020)
}
\arguments{
\item{startyear}{Specify a numeric value of the first year in time period. Default value is \emph{2011}.}

\item{endyear}{Specity a numeric value of the last year in time period. Default value is \emph{2020}.}
}
\value{
Returns a dataframe (tibble) of Norwegian imputed multilateral ODA to sectors
}
\description{
Get data on Norwegian imputed multilateral ODA to sectors by year, using the OECD SDMX API. Amounts in USD million and NOK million.
}
\examples{
?df_imputed_sectors <- get_imputed_sectors()

}
