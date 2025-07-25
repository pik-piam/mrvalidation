% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidLivestockDemStructure.R
\name{calcValidLivestockDemStructure}
\alias{calcValidLivestockDemStructure}
\title{calcValidLivestockDemStructure}
\usage{
calcValidLivestockDemStructure(datasource = "FAO")
}
\arguments{
\item{datasource}{Datasource of validation data. If "FAO", we use FAO calories with FAO population data (slightly diverges from original data as the convert script for example splits up countries for the past).}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
calculates the validation data for the share of different livestock products (excluding fish) in total livestock calorie food supply
}
\examples{

\dontrun{
calcOutput("ValidLivestockDemStructure")
}

}
\seealso{
\code{\link[mrcommons]{calcFoodSupplyPast}},
\code{\link{calcValidLivestockShare}}
}
\author{
Isabelle Weindl
}
