% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidGridCroplandNitrogenWithdrawals.R
\name{calcValidGridCroplandNitrogenWithdrawals}
\alias{calcValidGridCroplandNitrogenWithdrawals}
\title{calcValidGridCroplandNitrogenWithdrawals}
\usage{
calcValidGridCroplandNitrogenWithdrawals(irrigation = FALSE)
}
\arguments{
\item{irrigation}{FALSE for the sum of irrigated and rainfed, FALSE for seperated categories, 'rainfed' or 'irrigated for single categories}
}
\value{
List of magpie objects with results on cellular level, weight on cellular level, unit and description.
}
\description{
reports Cropland Nitrogen Withdrawals from soils on 0.5 degree grid
}
\examples{

\dontrun{ 
calcOutput("ValidGridCroplandNitrogenWithdrawals")
}

}
\seealso{
\code{\link{fullMADRATTOLPJML}}
}
\author{
Benjamin Leon Bodirsky
}
