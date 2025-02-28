#' @title calcValidGrassLSUha
#' @description calculates the validation data for production of grass from managed pastures and rangelands separetely
#' @param datasource Currently available: \code{"MAgPIEown"}
#' @param faoversion Currently available: \code{"FAO", "FAOpre2010", "FAOpost2010"}
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Marcos Alves
#' @seealso
#' \code{\link{calcFAOmassbalance}},
#' @examples
#' \dontrun{
#' calcOutput("ValidGrassLSUha")
#' }
calcValidGrassLSUha <- function(datasource = "MAgPIEown") {

  if (datasource == "MAgPIEown") {
    # coordinate to country mapping for 67420 cells
    mappingCtry <- toolGetMappingCoord2Country()
    countries <- unique(mappingCtry$iso)
    mappingCtry$coordiso <- paste(mappingCtry$coords,
                                  mappingCtry$iso,
                                  sep = ".")

    biomass   <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production.dm"][, , "pasture"]
    biomass   <- collapseNames(biomass)[countries, , ]

    land <- calcOutput("LanduseInitialisation", nclasses = "nine",
                       cellular = TRUE, cells = "lpjcell",
                       aggregate = FALSE)

    years <- intersect(getYears(biomass), getYears(land))

    grasslLand   <- land[, years, c("past", "range")]
    grasslLand   <- setNames(grasslLand, c("pastr", "range"))
    grasslShares <- setNames(grasslLand[, , "pastr"] / dimSums(grasslLand, dim = 3), "pastr")
    grasslShares <- add_columns(grasslShares, addnm = "range", dim = 3.1)
    grasslShares[, , "range"] <- 1 - grasslShares[, , "pastr"]
    grasslShares[is.nan(grasslShares) | is.infinite(grasslShares)] <- 0

    livestock  <- setNames(readSource("GLW3"), "liv_numb")
    livstSplit <- livestock * grasslShares
    livstSplit <- collapseNames(livstSplit)
    livstSplitCtry <- toolAggregate(livstSplit, rel = mappingCtry,
                                    from = "coordiso", to = "iso")
    livstShareCtry <- livstSplitCtry[, , "pastr"] / dimSums(livstSplitCtry, dim = 3)
    livstShareCtry[is.nan(livstShareCtry) | is.infinite(livstShareCtry)] <- 0
    livstShareCtry <- add_columns(livstShareCtry, addnm = "range", dim = 3.1)
    livstShareCtry[, , "range"] <- 1 - livstShareCtry[, , "pastr"]

    # I am splitting biomass consumption assuming the share
    # between animals reared on rangelands and pastures correlates linearly
    # with the production of grass in pastures and rangelands in a country. That can be
    # derived by the fact that the feedbaskets assume the same productivity (feed ingreedients shares)
    # within a country.

    lsuSplit <- biomass[, years, ] * livstShareCtry / (8.9 * 365 / 1000)
    lsuSplit <- toolCountryFill(lsuSplit)
    lsuSplit[is.nan(lsuSplit) | is.na(lsuSplit) | is.infinite(lsuSplit)] <- 0
    lsuSplit <- setNames(lsuSplit,
                         paste0("Total lsu|+|Cattle|",
                                reportingnames(getNames(lsuSplit, dim = 1)),
                                " (millions)"))
    lsuSplit <- add_dimension(lsuSplit, dim = 3.1, add = "scenario", nm = "historical")
    lsuSplit <- add_dimension(lsuSplit, dim = 3.2, add = "model", nm = datasource)
  }

  return(list(x = lsuSplit,
              weight = NULL,
              unit = "Mt DM/yr",
              description = "Grass Production from pastures and rangelands")
  )
}
