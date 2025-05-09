% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidFoodExpenditure.R
\name{calcValidFoodExpenditure}
\alias{calcValidFoodExpenditure}
\title{calcValidFoodExpenditure}
\usage{
calcValidFoodExpenditure(detail = FALSE)
}
\arguments{
\item{detail}{if FALSE, only major food commoditiy groups are shown.}
}
\value{
List of magpie object with results on country level, weight on country level, unit and description.
}
\description{
validation for foode expenditure
}
\examples{
\dontrun{
calcOutput("ValidFoodExpenditure")
}
}
\author{
Benjamin Leon Bodirsky
}
