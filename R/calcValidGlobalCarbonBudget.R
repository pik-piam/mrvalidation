#' @title ValidGlobalCarbonBudget
#' @description validation for total and cumulative land emissions from the Global Carbon Budget, including
#' all bookkeeping models
#' @author Michael Crawford
#'
#' @param cumulative cumulative from y2000
#'
#' @return a MAgPIE object
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidGlobalCarbonBudget")
#' }

calcValidGlobalCarbonBudget <- function(cumulative = FALSE) {

  allOut <- readSource("GlobalCarbonBudget")

  allOut <- add_dimension(allOut, dim = 3.1, add = "scenario", nm = "historical")

  if (cumulative) {
    allOut[, "y1995", ] <- 0
    allOut <- magclass::as.magpie(apply(allOut, c(1, 3), cumsum))

    # convert from Mt CO2 per year to Gt C02 per year
    allOut <- allOut * 10e-4

    reportingNames <- magclass::getNames(allOut, dim = 3)
    reportingNames <- stringr::str_replace(
      reportingNames,
      "Emissions\\|CO2\\|Land(\\||$)",
      "Emissions|CO2|Land|Cumulative\\1"
    )
    magclass::getNames(allOut, dim = 3) <- reportingNames
  }

  # append units
  reportingNames <- magclass::getNames(allOut, dim = 3)
  if (cumulative) {
    reportingNames <- paste0(reportingNames, " (Gt CO2)")
  } else {
    reportingNames <- paste0(reportingNames, " (Mt CO2/yr)")
  }
  magclass::getNames(allOut, dim = 3) <- reportingNames

  return(list(
    x           = allOut,
    weight      = NULL,
    unit        = "Mt or Gt (if cumulative) CO2 per year",
    description = "Gross emissions, indirect emissions, and net land CO2 flux from GCB"
  ))
}
