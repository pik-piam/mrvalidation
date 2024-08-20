#' @title calcValidTotalLaborCosts
#'
#' @description total labor costs in crop and livestock production
#'
#' @param datasource "USDA" (which uses FAO VoP and USDA cost shares) or "ILO" (which is based on ILO datasets and
#' calibrated to the USDA/FAO approach, but includes costs for some countries without VoP data) or "GTAP"
#' @param dataVersionILO "" for the oldest version, or "monthYear" (e.g. "Aug23") for a newer version
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidTotalLaborCosts", datasource = "USDA")
#' }
#'

calcValidTotalLaborCosts <- function(datasource = "USDA", dataVersionILO = "Aug24") {

  out <- calcOutput("LaborCosts", datasource = datasource, dataVersionILO = dataVersionILO,
                    subsectors = TRUE, otherLivst = FALSE, aggregate = FALSE)[, , c("Crops", "Livestock")]

  out <- setNames(out, c("Costs Optimization|Input Factors|Labor costs|+|Crop products (million US$2017/yr)",
                         "Costs Optimization|Input Factors|Labor costs|+|Livestock products (million US$2017/yr)"))

  if (datasource == "USDA") datasource <- "USDA/FAO"
  if (dataVersionILO == "") dataVersionILO <- "Aug21"
  if (datasource == "ILO") datasource <- paste0("ILO_", dataVersionILO, " based")
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = NULL,
              unit = "million US$2017/yr",
              description = "labor costs in agriculture"))
}
