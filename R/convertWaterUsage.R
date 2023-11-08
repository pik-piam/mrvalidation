#' @title convertWaterUsage
#' @description Convert data on agricultural water use
#' Convert subtypes on ISO country level.
#'
#' @param x MAgPIE object containing IrrigatedArea data on Country level
#' @param subtype : No subtype needed
#' @return Data on water use as MAgPIE object on country level Missing values are added as NA
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("WaterUsage", "aquastat_2008_12")
#' }
#'
#' @importFrom madrat toolCountryFill toolGetMapping
convertWaterUsage <- function(x, subtype) {
  if (subtype == "aquastat_2008_12") {
    mapping <- toolGetMapping(type = "regional", name = "2c86f3f30ff7f2b9a3ea978f9d9d7f6d.csv",
                              where = "mappingfolder")
    y <- toolAggregate(x = x, rel = mapping, from = 1, to = 2, partrel = TRUE)
    y <- toolCountryFill(y)
    return(y)
  } else {
    stop(paste0("Subtype: ", subtype,
    "contains global or regional values with no disaggregation! run readSource\"Water_usage\", \"",
    subtype, "\", convert=F)"))
  }
}
