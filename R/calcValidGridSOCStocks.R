#' @title calcValidGridSOCStocks
#' @description calculates the validation data for the gridded soil carbon pools
#'
#' @param datasource Datasources for validation data, e.g. LPJ_IPCC2006, LPJmL_natural, ...
#' @param baseyear baseyear for calculating soil carbon stock change (for LPJ_IPCC2006 only)
#' @param intensive If FALSE (default) total stocks will be returned; otherwise (TRUE) carbon densities.
#'
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidGridSOCStocks")
#' }
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass mbind getYears setYears nregions
#' @importFrom madrat toolSplitSubtype

calcValidGridSOCStocks <- function(datasource = "LPJ_IPCC2006", baseyear = 1995, intensive = FALSE) {

  if (datasource == "LPJ_IPCC2006") {

    SOM_stock  <- calcOutput("SOM", subtype = "stock",   aggregate = FALSE)
    SOM_stock  <- mbind(SOM_stock, add_dimension(dimSums(SOM_stock, dim = 3.1), add = "landuse", nm = "total"))

    out <- mbind(
      setNames(SOM_stock[, , "total"][, , "soilc"],
               "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)"),
      setNames(SOM_stock[, , "cropland"][, , "soilc"],
               "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),
      setNames(SOM_stock[, , "noncropland"][, , "soilc"],
               "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
    )

    SOM_chang <- SOM_stock - setYears(SOM_stock[, baseyear, ], NULL)

    out <- mbind(out, setNames(SOM_chang[, , "total"][, , "soilc"],
                      paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm (Mt C wrt ",
                                 baseyear, ")")),
                      setNames(SOM_chang[, , "cropland"][, , "soilc"],
                      paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Cropland Soils (Mt C wrt ",
                                 baseyear, ")")),
                      setNames(SOM_chang[, , "noncropland"][, , "soilc"],
                      paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Noncropland Soils (Mt C wrt ",
                                 baseyear, ")"))
    )

    out <- mbind(out,
                 setNames(SOM_stock[, , "total"][, , "target_soilc"],
                          "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm (Mt C)"),
                 setNames(SOM_stock[, , "cropland"][, , "target_soilc"],
                          "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),
                 setNames(SOM_stock[, , "noncropland"][, , "target_soilc"],
                          "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
    )

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)
    weight <- NULL

  } else if (datasource %in% c("LPJmL_rev21", "LPJmL4Paper",
                               "SoilGrids", "GSOC", "WISE", "SoilGrids2:new",
                               "SoilGrids2:q05_new", "SoilGrids2:q95_new",
                               "SOCDebtPaper")) {

    if (datasource == "LPJmL_rev21") {

      soilc  <- readSource("LPJml_rev21", "soilc_layer", convert = "onlycorrect")
      out <- soilc[, , "mm0_200"] + setNames(soilc[, , "mm201_500"], NULL) / 3

    } else if (datasource == "LPJmL4Paper") {

      out <- calcOutput("LPJmL", version = "LPJmL4", climatetype = "LPJmL4Paper",
                        subtype = "soilc_layer", aggregate = FALSE)
      out <- collapseNames(out[, , 1] + 1 / 3 * out[, , 2])

    } else if (datasource == "GSOC") {

      out <- readSource("GSOC",  convert = "onlycorrect")
      out <- mbind(setYears(out, "y2015"), setYears(out, "y2016"), setYears(out, "y2017"))

    } else if (datasource == "WISE") {

      out <- readSource("WISE",  convert = "onlycorrect")
      out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))

    } else if (datasource == "SoilGrids") {

      out <- readSource("SoilGrids", subtype = "cstock_0_30", convert = "onlycorrect")
      out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))

    } else if (grepl("SoilGrids2", datasource)) {

      var <- toolSplitSubtype(datasource, list(soilgrids = "SoilGrids2", variable = NULL))$variable
      out <- readSource("SoilGrids", subtype = paste0("cstock_0_30_", var), convert = "onlycorrect")
      out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))
      
    }  else if (datasource == "SOCDebtPaper") {
      
      out <- toolCoord2Isocell(readSource("SoilCarbonDebt", convert = "onlycorrect"))
      out <- mbind(setNames(setYears(out[ , , "SOCS_1960"], "y1960"), "SOC"),
                   setNames(setYears(out[ , , "SOCS_1990"], "y1990"), "SOC"), 
                   setNames(setYears(out[ , , "SOCS_2010"], "y2010"), "SOC"))
    }

    area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, cellular = TRUE,
                        selectyears = "past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)

    if (intensive) {
      weight <- area
    } else {
      weight <- NULL
      out    <- out * area
    }

    out <- setNames(out, "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  } else if (grepl("LPJmL4", datasource)) {

      ds <- toolSplitSubtype(datasource, list(version = NULL, climatemodel = NULL, scenario = NULL))

      LPJmL4_soilc     <- calcOutput("LPJmL_new", version = ds$version,
                                     climatetype = paste(ds$climatemodel, ds$scenario, sep = ":"),
                                     subtype = "soilc_layer", stage = "raw", aggregate = FALSE)
      LPJmL4_litc      <- calcOutput("LPJmL_new", version = ds$version,
                                     climatetype = paste(ds$climatemodel, ds$scenario, sep = ":"),
                                     subtype = "litc", stage = "raw", aggregate = FALSE)
      out              <- setNames(LPJmL4_soilc[, , "layer1"] + 1 / 3 * LPJmL4_soilc[, , "layer2"] +
                                   LPJmL4_litc, "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")

      out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
      out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

      area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, cellular = TRUE,
                          selectyears = "past_all", aggregate = FALSE)
      area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)

      if (intensive) {
        weight <- area
      } else {
        weight <- NULL
        out    <- out * area
      }

  } else if (datasource == "SoilGrids2_range") {

    out <- mbind(setNames(readSource("SoilGrids", subtype = "cstock_0_30_q05_new", convert = "onlycorrect"), "Q0p05"),
                 setNames(readSource("SoilGrids", subtype = "cstock_0_30_q95_new", convert = "onlycorrect"), "Q0p95"))
    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))

    out <- setNames(out, "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)


  } else stop("No data exist for the given datasource!")

  names(dimnames(out))[3] <- "scenario.model.variable"

  return(list(x = out,
              weight = weight,
              unit = "Mt C",
              description = "Cellular Soil Carbon",
              isocountries = FALSE)
  )
}
