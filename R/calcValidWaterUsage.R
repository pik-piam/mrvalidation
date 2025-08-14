#' @title calcValidWaterUsage
#'
#' @description Returns historical and projected water withdrawal
#'              from different data sources
#'
#' @param datasource Currently available:
#' \itemize{
#' \item historical:
#' \itemize{
#' \item \code{"foley_2011"}
#' \item \code{"shiklomanov_2000"}
#' \item \code{"wada_2011"}
#' \item \code{"wisser_2008"}
#' \item \code{"CWatM:ipsl-cm5a-lr"}
#' \item \code{"CWatM:gfdl-esm2m"}
#' \item \code{"CWatM:miroc5"}
#' \item \code{"CWatM:hadgem2-es"}
#' \item \code{"LPJmL:ipsl-cm5a-lr"}
#' \item \code{"LPJmL:gfdl-esm2m"}
#' \item \code{"LPJmL:miroc5"}
#' \item \code{"LPJmL:hadgem2-es"}
#' \item \code{"H08:ipsl-cm5a-lr"}
#' \item \code{"H08:gfdl-esm2m"}
#' \item \code{"H08:miroc5"}
#' \item \code{"H08:hadgem2-es"}
#' \item \code{"MATSIRO:ipsl-cm5a-lr"}
#' \item \code{"MATSIRO:gfdl-esm2m"}
#' \item \code{"MATSIRO:miroc5"}
#' \item \code{"MATSIRO:hadgem2-es"}
#' \item \code{"MPI-HM:ipsl-cm5a-lr"}
#' \item \code{"MPI-HM:gfdl-esm2m"}
#' \item \code{"MPI-HM:miroc5"}
#' \item \code{"PCR-GLOBWB:ipsl-cm5a-lr"}
#' \item \code{"PCR-GLOBWB:gfdl-esm2m"}
#' \item \code{"PCR-GLOBWB:miroc5"}
#' \item \code{"PCR-GLOBWB:hadgem2-es"}
#' }
#' \item projections:
#' \itemize{
#' \item \code{"fischer_IIASA"}
#' \item \code{"hejazi_2013"}
#'  \item \code{molden_IWMI}
#'  \item \code{seckler_IWMI}
#'  \item \code{shiklomanov}
#' }
#' }
#'
#' @return list of magpie object, weight, unit, and description
#' @author Stephen Wirth, Anne Biewald, Felicitas Beier
#'
#' @importFrom magclass collapseDim dimSums add_dimension as.magpie getItems getNames getCells
#' @importFrom madrat calcOutput readSource toolAggregate toolCountryFill

calcValidWaterUsage <- function(datasource = "shiklomanov_2000", rev = 0) {

  if (datasource %in% c("foley_2011", "shiklomanov_2000", "wada_2011",
                        "wisser_2008", "fischer_IIASA", "hejazi_2013",
                        "molden_IWMI", "seckler_IWMI", "shiklomanov")) {

    out <- readSource("WaterUsage", datasource, convert = FALSE)

    if (datasource %in% c("wisser_2008", "fischer_IIASA", "hejazi_2013", "seckler_IWMI")) {
      out <- out[, , "data"]
    }

      if (datasource %in% c("foley_2011", "shiklomanov_2000", "wada_2011", "wisser_2008")) {

      out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
      out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

      } else if (datasource %in% c("fischer_IIASA", "hejazi_2013", "molden_IWMI",
                                 "seckler_IWMI", "shiklomanov", "aquastat_2008_12")) {

      out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "projection")
      out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

      }

    } else if (datasource %in% c("CWatM:ipsl-cm5a-lr",      "CWatM:gfdl-esm2m",
                                 "CWatM:miroc5",            "CWatM:hadgem2-es",
                                 "H08:ipsl-cm5a-lr",        "H08:gfdl-esm2m",
                                 "H08:miroc5",              "H08:hadgem2-es",
                                 "LPJmL:ipsl-cm5a-lr",      "LPJmL:gfdl-esm2m",
                                 "LPJmL:miroc5",            "LPJmL:hadgem2-es",
                                 "MATSIRO:ipsl-cm5a-lr",    "MATSIRO:gfdl-esm2m",
                                 "MATSIRO:miroc5",          "MATSIRO:hadgem2-es",
                                 "MPI-HM:ipsl-cm5a-lr",     "MPI-HM:gfdl-esm2m",
                                 "MPI-HM:miroc5",
                                 "PCR-GLOBWB:ipsl-cm5a-lr", "PCR-GLOBWB:gfdl-esm2m",
                                 "PCR-GLOBWB:miroc5",       "PCR-GLOBWB:hadgem2-es")) {

      out <- readSource("ISIMIP", subtype = paste("airww", datasource, "2b", sep = ":"), convert = TRUE)

      # unit transformation: from: kg m-2 s-1 is equal to mm/second, to: mm/month
      # (Note: 1 day = 60*60*24 = 86400 seconds)
      dayofmonths <- as.magpie(c(jan = 31, feb = 28, mar = 31, apr = 30,
                                 may = 31, jun = 30, jul = 31, aug = 31,
                                 sep = 30, oct = 31, nov = 30, dec = 31), temporal = 1)
      out      <- out * dayofmonths * 86400
      # transform mm/month to mm/year
      out      <- collapseDim(toolAggregate(out, data.frame(getItems(out, "month"), "year"),
                              dim = "month"))
      # Note: mm/year is equal to liter/m^2/year -> liter/year
      # Conversion from liter/m^2/year -> liter/year: multiply with landarea
      # landarea (given in Mha) -> m^2 (multiply 1e10)
      # liter -> km^3 (multiply with 1e-12)
      landarea <- calcOutput("LandArea", aggregate = FALSE)
      landarea <- dimSums(landarea, dim = c("x", "y"))
      landarea <- toolCountryFill(landarea, fill = 0)
      out      <- out * landarea * 1e-02

      out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
      out <- add_dimension(out, dim = 3.2, add = "model",    nm = datasource)

    } else {
      stop("Given datasource currently not supported!")
    }

  getNames(out, dim = 3)  <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"
  names(dimnames(out))[3] <- "scenario.model.variable"

  return(list(x           = out,
              weight      = NULL,
              unit        = "km^3",
              min         = 0,
              description = "Agricultural water withdrawal from different sources in km^3"))
}
