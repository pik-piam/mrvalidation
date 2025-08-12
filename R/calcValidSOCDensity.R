#' @title calcValidSOCDensity
#' @description calculates the validation data for the soil carbon densities (including weights for aggregation)
#'
#' @param datasource Datasources for validation data
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link[mrcommons]{calcSOM}}
#' \code{\link{calcValidSOCStocks}}
#' @examples
#' \dontrun{
#' calcOutput("ValidSOCDensity")
#' }
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass mbind getYears setYears nregions getCells
#' @importFrom mstools toolCoord2Isocell

calcValidSOCDensity <- function(datasource = "GSOC") {

  if (datasource == "GSOC") {

    soilc <- readSource("GSOC",  convert = "onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)

    mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell", where = "mappingfolder")
    soilc   <- toolAggregate(soilc, weight = area, rel = mapping, from = "celliso", to = "iso", dim = 1)
    soilc   <- toolCountryFill(soilc, fill = 0)
    out     <- setNames(soilc, "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")

    out <- mbind(setYears(out, "y2015"), setYears(out, "y2016"), setYears(out, "y2017"))
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    area   <- toolAggregate(area, rel = mapping, from = "celliso", to = "iso", dim = 1)
    area   <- toolCountryFill(area, fill = 0)

    weight   <- out
    weight[] <- area

  } else if (datasource == "WISE") {

    soilc <- readSource("WISE",  convert = "onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)

    mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell", where = "mappingfolder")
    soilc   <- toolAggregate(soilc, weight = area, rel = mapping, from = "celliso", to = "iso", dim = 1)
    soilc   <- toolCountryFill(soilc, fill = 0)
    out     <- setNames(soilc, "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")

    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    area   <- toolAggregate(area, rel = mapping, from = "celliso", to = "iso", dim = 1)
    area   <- toolCountryFill(area, fill = 0)

    weight   <- out
    weight[] <- area

  } else if (datasource == "SoilGrids") {

    soilc <- toolCoord2Isocell(readSource("SoilGrids", subtype = "cstock_0_30", convert = "onlycorrect"))
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)

    mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell", where = "mappingfolder")
    soilc   <- toolAggregate(soilc, weight = area, rel = mapping, from = "celliso", to = "iso", dim = 1)
    soilc   <- toolCountryFill(soilc, fill = 0)
    out     <- setNames(soilc, "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")

    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    area   <- toolAggregate(area, rel = mapping, from = "celliso", to = "iso", dim = 1)
    area   <- toolCountryFill(area, fill = 0)

    weight   <- out
    weight[] <- area

  } else {
    stop("No data exist for the given datasource!")
  }

  names(dimnames(out))[3]    <- "scenario.model.variable"
  names(dimnames(weight))[3] <- "scenario.model.variable"

  return(list(x = out,
              weight = weight,
              unit = "t C per ha",
              description = "Soil Carbon")
  )
}
