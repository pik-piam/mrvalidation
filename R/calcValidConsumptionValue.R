#' @title calcValidConsumptionValue
#' @description Validation for consumption Value
#' @param datasource datasource for validation (FAO, FAOpre2010, FAOpost2010)
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @importFrom magclass collapseNames
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' calcOutput("ValidConsumptionValue")
#' }
calcValidConsumptionValue <- function(datasource = "FAO") {
  kall <- findset("kall")
  if (datasource == "FAO") {
    # Food and material demand
    foodMat <- collapseNames(
                             dimSums((
                                      calcOutput("FAOmassbalance",
                                                 aggregate = FALSE)[, , kall][, , c("food", "other_util")])[, , "dm"],
                             dim = 3.2))
  } else if (datasource == "FAOpre2010") {
    foodMat <- collapseNames(
                             dimSums((
                                      calcOutput("FAOmassbalance_pre",
                                                 version = "pre2010",
                                                 aggregate = FALSE)[, , kall][, , c("food", "other_util")])[, , "dm"],
                             dim = 3.2))
  } else if (datasource == "FAOpost2010") {
    foodMat <- collapseNames(
                             dimSums((
                                      calcOutput("FAOmassbalance_pre",
                                                 version = "FAOpost2010",
                                                 aggregate = FALSE)[, , kall][, , c("food", "other_util")])[, , "dm"],
                             dim = 3.2))

  } else {
    stop("unknown datasource")
  }

  # Price consumers (World Prices)
  pricesKallCon <- setYears(calcOutput("IniFoodPrice", products = "kall", aggregate = FALSE), NULL)

  out <- dimSums(foodMat * pricesKallCon, dim = 3)


  getNames(out) <- "Value|Consumption Value (million US$2017/yr)"
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = NULL,
              unit = "million US$17/yr",
              description = "Consumption Value"))
}
