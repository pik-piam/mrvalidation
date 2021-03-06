% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidSOCDensity.R
\name{calcValidSOCDensity}
\alias{calcValidSOCDensity}
\title{calcValidSOCDensity}
\usage{
calcValidSOCDensity(datasource = "LPJ_IPCC2006")
}
\arguments{
\item{datasource}{Datasources for validation data, e.g. LPJ_IPCC2006, LPJmL_natural}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
calculates the validation data for the soil carbon densities (including weights for aggregation)
}
\examples{

\dontrun{ 
  calcOutput("ValidSOCDensity")
}

}
\seealso{
\code{\link{calcSOM}}
\code{\link{calcValidSOCStocks}}
}
\author{
Kristine Karstens
}
