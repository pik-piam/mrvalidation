#' @title calcValidSOCShare
#' @description calculates the validation data for the soil carbon shares
#'
#' @param datasource Datasources only "histSOCbudget"
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidSOCShare")
#' }
#'
#' @importFrom magclass mbind getYears setYears

calcValidSOCShare <- function(datasource = "histSOCbudget") {

  if (datasource == "histSOCbudget") {

    steadyState  <- calcOutput("SteadyState", aggregate = FALSE)
    landuse      <- calcOutput("Landuse", aggregate = FALSE)[, getYears(steadyState), ]
    somStock     <- calcOutput("SoilCarbon", aggregate = FALSE, output = "actualstate")[, getYears(steadyState), ]
    natStock     <- calcOutput("SoilCarbon", aggregate = FALSE, output = "naturalstate")[, getYears(steadyState), ]

    somStock  <- mstools::toolConv2CountryByCelltype(dimSums(somStock, dim = 3.1), cells = "lpjcell")
    natStock  <- mstools::toolConv2CountryByCelltype(dimSums(natStock, dim = 3.1), cells = "lpjcell")
    equStock  <- mstools::toolConv2CountryByCelltype(dimSums(steadyState * landuse, dim = 3.1), cells = "lpjcell")
    pEquStock <- mstools::toolConv2CountryByCelltype(dimSums(steadyState[, , "natveg"] * landuse,
                                                             dim = 3.1), cells = "lpjcell")
    pEquStock <- collapseNames(pEquStock)

    somStock  <- mbind(somStock, add_dimension(dimSums(somStock, dim = 3.1), add = "landuse", nm = "total"))
    natStock  <- mbind(natStock, add_dimension(dimSums(natStock, dim = 3.1), add = "landuse", nm = "total"))
    equStock  <- mbind(equStock, add_dimension(dimSums(equStock, dim = 3.1), add = "landuse", nm = "total"))
    pEquStock <- mbind(pEquStock, add_dimension(dimSums(pEquStock, dim = 3.1), add = "landuse", nm = "total"))

    somShare <- somStock / natStock
    unit          <- "(tC/tC)"
    zeroOrderName <- "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm"
    out <- mbind(setNames(somShare[, , "total"],  paste0(zeroOrderName, " ", unit)),
                 setNames(somShare[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
                 setNames(somShare[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

    weight <- mbind(setNames(natStock[, , "total"],  paste0(zeroOrderName, " ", unit)),
                    setNames(natStock[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
                    setNames(natStock[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

    zeroOrderName <- "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm"
    equSomShare   <- equStock / pEquStock
    out <- mbind(out,
                 setNames(equSomShare[, , "total"],  paste0(zeroOrderName, " ", unit)),
                 setNames(equSomShare[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
                 setNames(equSomShare[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

    weight <- mbind(weight,
                    setNames(pEquStock[, , "total"],  paste0(zeroOrderName, " ", unit)),
                    setNames(pEquStock[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
                    setNames(pEquStock[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    weight <- add_dimension(weight, dim = 3.1, add = "scenario", nm = "historical")
    weight <- add_dimension(weight, dim = 3.2, add = "model", nm = datasource)

  } else {
    stop("No data exist for the given datasource!")
  }

  names(dimnames(out))[3]    <- "scenario.model.variable"
  names(dimnames(weight))[3] <- "scenario.model.variable"

  return(list(x           = out,
              weight      = weight,
              unit        = "tC per tC",
              description = "Soil Carbon Share")
  )
}
