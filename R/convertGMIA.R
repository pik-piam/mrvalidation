#' @title convertGMIA
#' @description Convert Global Map on Irrigated Area Data
#'
#' Convert subtypes on ISO country level.
#'
#
#' @param x MAgPIE object containing IrrigatedArea data on Country level
#' @param subtype : No subtype needed
#'
#' @return Global Map on Irrigation data as MAgPIE object on country level Missing values are added as NA
#'
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("GlobalIrrigationMap", "all_data_national")
#' a <- readSource ("GMIA", "aei_pct", convert = FALSE)
#' a <- readSource ("GMIA", "aei_pct", convert = "correctonly")
#' }
#' @importFrom magclass getItems<- getRegions<-
#'
convertGMIA <- function(x, subtype) {
  if (subtype == "all_data_national") {
    map <- toolGetMapping(type = "regional", name = "regionmappingMAgPIE.csv")
    map$X[grep("Virgin Islands, U.S.", map$X)] <- "Virgin Islands, U"
    map$X <- toupper(map$X) # nolint
    getItems(x, dim = 1) <- toupper(getItems(x, dim = 1))
    y <- toolAggregate(x, rel = map, from = 1, to = 2, partrel = TRUE)
    y <- toolCountryFill(y)
  } else {
    map <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
    y <- toolAggregate(x, rel = map, from = 1, to = 3, partrel = TRUE)
    y <- toolCountryFill(y)
  }
  return(y)
}
