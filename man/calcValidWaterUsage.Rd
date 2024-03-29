% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidWaterUsage.R
\name{calcValidWaterUsage}
\alias{calcValidWaterUsage}
\title{calcValidWaterUsage}
\usage{
calcValidWaterUsage(datasource = "shiklomanov_2000")
}
\arguments{
\item{datasource}{Currently available:
\itemize{
\item historical:
\itemize{
\item \code{"foley_2011"}
\item \code{"shiklomanov_2000"}
\item \code{"wada_2011"}
\item \code{"wisser_2008"}
\item \code{"CWatM:ipsl-cm5a-lr"}
\item \code{"CWatM:gfdl-esm2m"}
\item \code{"CWatM:miroc5"}
\item \code{"CWatM:hadgem2-es"}
\item \code{"LPJmL:ipsl-cm5a-lr"}
\item \code{"LPJmL:gfdl-esm2m"}
\item \code{"LPJmL:miroc5"}
\item \code{"LPJmL:hadgem2-es"}
\item \code{"H08:ipsl-cm5a-lr"}
\item \code{"H08:gfdl-esm2m"}
\item \code{"H08:miroc5"}
\item \code{"H08:hadgem2-es"}
\item \code{"MATSIRO:ipsl-cm5a-lr"}
\item \code{"MATSIRO:gfdl-esm2m"}
\item \code{"MATSIRO:miroc5"}
\item \code{"MATSIRO:hadgem2-es"}
\item \code{"MPI-HM:ipsl-cm5a-lr"}
\item \code{"MPI-HM:gfdl-esm2m"}
\item \code{"MPI-HM:miroc5"}
\item \code{"PCR-GLOBWB:ipsl-cm5a-lr"}
\item \code{"PCR-GLOBWB:gfdl-esm2m"}
\item \code{"PCR-GLOBWB:miroc5"}
\item \code{"PCR-GLOBWB:hadgem2-es"}
}
\item projections:
\itemize{
\item \code{"fischer_IIASA"}
\item \code{"hejazi_2013"}
 \item \code{molden_IWMI}
 \item \code{seckler_IWMI}
 \item \code{shiklomanov}
}
}}
}
\value{
list of magpie object, weight, unit, and description
}
\description{
Returns historical and projected water withdrawal
             from different data sources
}
\author{
Stephen Wirth, Anne Biewald, Felicitas Beier
}
