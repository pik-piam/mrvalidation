#' @title calcValidSOCStocks
#' @description calculates the validation data for the soil carbon pools
#'
#' @param datasource Datasources for validation data, e.g. LPJ_IPCC2006, LPJmL_natural
#' @param baseyear baseyear for calculating soil carbon stock change
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' \code{\link{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' \dontrun{
#' calcOutput("ValidSOCStocks")
#' }
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass mbind getYears setYears nregions
#' @importFrom mrcommons toolCoord2Isocell

calcValidSOCStocks <- function(datasource = "LPJ_IPCC2006", baseyear = 1995) {

  if (datasource == "LPJ_IPCC2006") {

    mapping    <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")
    somStock  <- calcOutput("SOM", subtype = "stock",   aggregate = FALSE)
    somStock  <- toolAggregate(somStock, rel = mapping,
                               from = ifelse(nregions(somStock) > 1, "celliso", "cell"), to = "iso", dim = 1)
    somStock  <- toolCountryFill(somStock, fill = 0)
    somStock  <- mbind(somStock, add_dimension(dimSums(somStock, dim = 3.1), add = "landuse", nm = "total"))

    out <- mbind(
      setNames(somStock[, , "total"][, , "soilc"],
               "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)"),
      setNames(somStock[, , "cropland"][, , "soilc"],
               "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),
      setNames(somStock[, , "noncropland"][, , "soilc"],
               "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
    )

    somChang <- somStock - setYears(somStock[, baseyear, ], NULL)

    out <- mbind(out,
      setNames(somChang[, , "total"][, , "soilc"],
               paste0("Resources|Soil Carbon|Actual|Stock Change|",
                      "SOC in top 30 cm (Mt C wrt ", baseyear, ")")),
      setNames(somChang[, , "cropland"][, , "soilc"],
               paste0("Resources|Soil Carbon|Actual|Stock Change|",
                      "SOC in top 30 cm|+|Cropland Soils (Mt C wrt ", baseyear, ")")),
      setNames(somChang[, , "noncropland"][, , "soilc"],
               paste0("Resources|Soil Carbon|Actual|Stock Change|",
                      "SOC in top 30 cm|+|Noncropland Soils (Mt C wrt ", baseyear, ")"))
    )

    out <- mbind(out,
      setNames(somStock[, , "total"][, , "target_soilc"],
               "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm (Mt C)"),
      setNames(somStock[, , "cropland"][, , "target_soilc"],
               "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),
      setNames(somStock[, , "noncropland"][, , "target_soilc"],
               "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
    )

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)


  } else if (datasource == "LPJmL4Paper") {

    soilc <- calcOutput("LPJmL", version = "LPJmL4", climatetype = "LPJmL4Paper",
                        subtype = "soilc_layer", aggregate = FALSE)
    soilc <- collapseNames(soilc[, , 1] + 1 / 3 * soilc[, , 2])
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)
    stock <- soilc * area

    mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")
    stock   <- toolAggregate(stock, rel = mapping, from = "celliso", to = "iso", dim = 1)
    stock   <- toolCountryFill(stock, fill = 0)
    out     <- setNames(stock, "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  } else if (datasource == "GSOC") {

    soilc <- readSource("GSOC",  convert = "onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)
    stock <- soilc * area

    mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")
    stock   <- toolAggregate(stock, rel = mapping, from = "celliso", to = "iso", dim = 1)
    stock   <- toolCountryFill(stock, fill = 0)
    out     <- setNames(stock, "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")

    out <- mbind(setYears(out, "y2015"), setYears(out, "y2016"), setYears(out, "y2017"))
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  } else if (datasource == "WISE") {

    soilc <- readSource("WISE",  convert = "onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)
    stock <- soilc * area

    mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")
    stock   <- toolAggregate(stock, rel = mapping, from = "celliso", to = "iso", dim = 1)
    stock   <- toolCountryFill(stock, fill = 0)
    out     <- setNames(stock, "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")

    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"),
                 setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  } else if (datasource == "SoilGrids") {

    soilc <- readSource("SoilGrids", subtype = "cstock_0_30", convert = "onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE,
                        cellular = TRUE, selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)
    stock <- soilc * area

    mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")
    stock   <- toolAggregate(stock, rel = mapping, from = "celliso", to = "iso", dim = 1)
    stock   <- toolCountryFill(stock, fill = 0)
    out     <- setNames(stock, "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")

    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"),
                 setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  } else {
    stop("No data exist for the given datasource!")
  }

  names(dimnames(out))[3] <- "scenario.model.variable"

  return(list(x = out,
              weight = NULL,
              unit = "Mt C",
              description = "Soil Carbon")
  )
}
