#' @title calcValidWageDevelopment
#'
#' @description wage index: hourly labor costs in crop+livestock production relative to a baseyear
#'
#' @param datasource So far only "ILO_completed" (based on regression between ILO hourly labor costs and GDP pc MER,
#' calibrated to match USDA/FAO total labor costs)
#' @param baseYear year relative to which the wage development should be calculated
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidWageDevelopment", datasource = "ILO_completed")
#' }
#'

calcValidWageDevelopment <- function(datasource = "ILO_completed", baseYear = 2000) {

  if (datasource == "ILO_completed") {
    hourlyCosts <- calcOutput("HourlyLaborCosts", datasource = "USDA_FAO", aggregate = FALSE)

    if (!baseYear %in% getYears(hourlyCosts, as.integer = TRUE)) stop("Baseyear not available.")

    wageIndex <- collapseDim(hourlyCosts / hourlyCosts[, baseYear, ], dim = 2.2)
    wageIndex <- setNames(wageIndex, paste0("Labor|Wages|Hourly labor costs relative to ", baseYear, " (index)"))

    # population in 2000 as weight for aggregation to world regions and globally
    weight <- wageIndex
    weight[, , ] <- calcOutput("Population", naming = "scenario",
                                aggregate = FALSE, years = c(baseYear))[, , "SSP2", drop = TRUE]

    description <- paste("Wage index calculated as hourly labor costs in agriculture relative to 2000 (based on ILO",
                           "data completed with a regression with GDP, calibrated using USDA and FAO data)")
  } else {
    stop("Datsource not available")
  }

  out <- add_dimension(wageIndex, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = weight,
              unit = "",
              description = description))

}
