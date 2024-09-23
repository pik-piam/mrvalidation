#' @title calcValidWageDevelopment
#'
#' @description wage index: hourly labor costs in crop+livestock production relative to a baseyear
#'
#' @param datasource Available datasources are:
#' \itemize{
#' \item ILO_raw : based on ILO hourly labor costs data
#' \item ILO_completed : based on ILO hourly labor costs data completed with a regression with GDP pc MER
#' \item USDA_FA0_raw : based on USDA/FAO hourly labor costs data
#' \item USDA_FA0_completed : based on USDA/FAO hourly labor costs data completed with a regression with GDP pc MER
#' }
#' @param baseYear year relative to which the wage development should be calculated
#' @param dataVersionILO "" for the oldest version, or "monthYear" (e.g. "Aug23") for a newer version
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidWageDevelopment", datasource = "ILO_completed")
#' }
#'

calcValidWageDevelopment <- function(datasource = "ILO_completed", baseYear = 2000, dataVersionILO = "Aug24") {

  hourlyCosts <- setNames(calcOutput("ValidHourlyLaborCosts", datasource = datasource,
                                     dataVersionILO = dataVersionILO, aggregate = FALSE), NULL)

  if (!baseYear %in% getYears(hourlyCosts, as.integer = TRUE)) stop("Baseyear not available.")

  wageIndex <- collapseDim(hourlyCosts / hourlyCosts[, baseYear, ], dim = 2.2)
  wageIndex <- setNames(wageIndex, paste0("Labor|Wages|Hourly labor costs relative to ", baseYear, " (index)"))

  # population in 2000 as weight for aggregation to world regions and globally
  weight <- wageIndex
  weight[, , ] <- calcOutput("Population", naming = "scenario",
                             aggregate = FALSE, years = c(baseYear))[, , "SSP2", drop = TRUE]

  description <- paste("Wage index calculated as hourly labor costs in agriculture relative to 2000")

  # USDA_FAO still depends on ILO employment
  if (dataVersionILO == "") dataVersionILO <- "Aug21"
  datasource <- paste(datasource, dataVersionILO, sep = "_")
  out <- add_dimension(wageIndex, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = weight,
              unit = "",
              description = description))

}
