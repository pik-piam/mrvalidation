#' @title calcValidHourlyLaborCosts
#'
#' @description hourly labor costs in crop+livestock production
#'
#' @param datasource Available datasources are:
#' \itemize{
#' \item ILO_raw : ILO hourly labor costs data
#' \item ILO_completed : ILO hourly labor costs data completed with a regression with GDP pc MER
#' \item USDA_FA0_raw : USDA/FAO hourly labor costs data
#' \item USDA_FA0_completed : USDA/FAO hourly labor costs data completed with a regression with GDP pc MER
#' }
#' @param dataVersionILO "" for the oldest version, or "monthYear" (e.g. "Aug23") for a newer version
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidHourlyLaborCosts", datasource = "ILO_completed")
#' }
#'

calcValidHourlyLaborCosts <- function(datasource = "ILO_completed", dataVersionILO = "Aug24") {

  if (datasource == "ILO_completed") {
    hourlyCosts <- calcOutput("HourlyLaborCosts", datasource = "ILO", dataVersionILO = dataVersionILO,
                              fillWithRegression = TRUE, calibYear = NULL, cutAfterCalibYear = FALSE,
                              aggregate = FALSE)

    description <- paste("Hourly labor costs in agriculture (based on ILO data completed with a",
                         "regression with GDP)")

  } else if (datasource == "ILO_raw") {
    hourlyCosts <- calcOutput("HourlyLaborCosts", datasource = "ILO", dataVersionILO = dataVersionILO,
                              fillWithRegression = FALSE, aggregate = FALSE)

    description <- paste("Hourly labor costs in agriculture (based on ILO data completed with a",
                         "regression with GDP)")

  } else if (datasource == "USDA_FAO_completed") {
    hourlyCosts <- calcOutput("HourlyLaborCosts", datasource = "USDA_FAO", dataVersionILO = dataVersionILO,
                              fillWithRegression = TRUE, calibYear = NULL, cutAfterCalibYear = FALSE,
                              aggregate = FALSE)

    description <- paste("Hourly labor costs in agriculture (based on ILO data completed with a",
                         "regression with GDP)")

  } else if (datasource == "USDA_FAO_raw") {
    hourlyCosts <- calcOutput("HourlyLaborCosts", datasource = "USDA_FAO", dataVersionILO = dataVersionILO,
                              fillWithRegression = FALSE, aggregate = FALSE)

    description <- paste("Hourly labor costs in agriculture (based on USDA/FAO data completed with a",
                         "regression with GDP)")
  } else {
    stop("Datsource not available")
  }

  hourlyCosts <- setNames(hourlyCosts, "Labor|Wages|Hourly labor costs (US$2017/h)")

  # total hours worked as weight for aggregation to world regions
  agEmpl <- calcOutput("AgEmplILO", dataVersionILO = dataVersionILO, aggregate = FALSE, subsectors = FALSE)
  weeklyHours <- calcOutput("WeeklyHoursILO", dataVersionILO = dataVersionILO, aggregate = FALSE)
  weight <- hourlyCosts
  years <- intersect(getYears(hourlyCosts, as.integer = TRUE),
                     intersect(getYears(agEmpl, as.integer = TRUE), getYears(weeklyHours, as.integer = TRUE)))
  weight[, years, ] <- agEmpl[, years, ] * weeklyHours[, years, ]
  weight[, setdiff(getYears(hourlyCosts,
                            as.integer = TRUE), years), ] <- agEmpl[, max(years), ] * weeklyHours[, max(years), ]
  weight[hourlyCosts == 0] <- 0

  # USDA_FAO still depends on ILO employment
  if (dataVersionILO == "") dataVersionILO <- "Aug21"
  datasource <- paste(datasource, dataVersionILO, sep = "_")
  out <- add_dimension(hourlyCosts, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = weight,
              unit = "USD2017$MER/h",
              description = description))

}
