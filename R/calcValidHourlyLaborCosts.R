#' @title calcValidHourlyLaborCosts
#'
#' @description hourly labor costs in crop+livestock production
#'
#' @param datasource So far only "ILO_completed" (based on regression between ILO hourly labor costs and GDP pc MER,
#' calibrated to match USDA/FAO total labor costs)
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidHourlyLaborCosts", datasource="ILO_completed")
#' }
#'

calcValidHourlyLaborCosts <- function(datasource = "ILO_completed") {

  if (datasource == "ILO_completed") {
    hourlyCosts <- calcOutput("HourlyLaborCosts", datasource = "USDA_FAO", aggregate = FALSE)
    hourlyCosts <- setNames(hourlyCosts, "Labor|Wages|Hourly labor costs (USDMER05/h)")

    # total hours worked in 2010 as weight for aggregation to world regions
    agEmpl <- calcOutput("AgEmplILO", aggregate = FALSE, subsectors = FALSE)
    weeklyHours <- calcOutput("WeeklyHoursILO", aggregate = FALSE)
    weight <- hourlyCosts
    weight[, , ] <- agEmpl[, 2010, ] * weeklyHours[, 2010, ]

    description <- paste("Hourly labor costs in agriculture (based on ILO data completed with a",
                           "regression with GDP, calibrated using USDA and FAO data)")
  } else {
    stop("Datsource not available")
  }

  out <- add_dimension(hourlyCosts, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = weight,
              unit = "USDMER05/h",
              description = description))

}
