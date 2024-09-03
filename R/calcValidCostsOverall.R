#' @title calcValidCostsOverall
#' @description Returns historical values of the overall value of production for the Agricultural,
#' Forestry sectors, and fisheries (mio.17USD)
#' @param datasource datasource for validation. FAO Value of Production dataset.
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' calcOutput("calcValidCostsOverall")
#' }
#'
calcValidCostsOverall <- function(datasource = "FAO") {

  #### Value of Production for the agriculture, forestry and fisheries sector

  if (datasource == "FAO") {

  # Value of Production in mio. 17USDmer units
  x <- dimSums(calcOutput("VoPAFF", aggregate = FALSE), dim = 3.1)


  getNames(x) <- paste0("Costs|Gross value of production (million US$2017/yr)")
  x <- add_dimension(x, dim = 3.1, add = "scenario", nm = "historical")
  x <- add_dimension(x, dim = 3.2, add = "model", nm = datasource)

  weight <- NULL
  units <- "(million US$2017/yr)"

  } else {
    warning("Only FAO datasource available")
  }

  return(list(x = x, weight = weight,
              unit = units,
              description = "Validation for overall costs indicators(million US$2017/yr)"))

}
