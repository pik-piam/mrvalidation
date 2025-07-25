% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertHID.R
\name{convertHID}
\alias{convertHID}
\title{convertHID}
\usage{
convertHID(x, subtype)
}
\arguments{
\item{x}{MAgPIE object containing IrrigatedArea data on Country level}

\item{subtype}{: No subtype needed}
}
\value{
Irrigated Area data as MAgPIE object on country level Missing values are added as NA
}
\description{
Convert subtypes on ISO country level.
}
\details{
Convert Historic Irrigation Data
}
\examples{

\dontrun{
a <- readSource("HID", "national_1900_2005")
}

}
\seealso{
\code{\link[madrat]{readSource}}
}
\author{
Stephen Wirth
}
