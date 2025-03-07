% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidAgriResearchIntensity.R
\name{calcValidAgriResearchIntensity}
\alias{calcValidAgriResearchIntensity}
\title{calcValidAgriResearchIntensity}
\usage{
calcValidAgriResearchIntensity(datasource = "Pardey")
}
\arguments{
\item{datasource}{Datasource of validation data.}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
calculates the validation data for TC as Ag R&D investments
}
\examples{
\dontrun{
calcOutput("calcValidAgriResearchIntensity")
}

}
\author{
David Chen
}
