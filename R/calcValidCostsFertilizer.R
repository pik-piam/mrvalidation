#' @title calcValidCostsFertilizer
#' @description calculates validation data for fertilizer costs
#'
#' @param datasource Source of validation data.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#'
#' \dontrun{
#' calcOutput("ValidCostsFertilizer", datasource = "FAO")
#' }
#'
#' @importFrom magpiesets reporthelper summationhelper reportingnames
#' @importFrom magclass getItems
#'
calcValidCostsFertilizer <- function(datasource = "FAO") {
  if (datasource == "FAO") {
    totalUseNutrients <- calcOutput("FertilizerUseFAO", subtype = "N", by = "nutrient", aggregate = FALSE)
    fertPriceNutrient <- calcOutput("FertilizerPricesFAO", subtype = "N", by = "nutrient", aggregate = FALSE)
    years <- intersect(getItems(totalUseNutrients, dim = 2), getItems(fertPriceNutrient, dim = 2))

    fertilizerCosts <- totalUseNutrients[, years, ] * fertPriceNutrient[, years, ] / 1e6
    getNames(fertilizerCosts) <- paste0("Costs|Fertilizer (million US$2017/yr)")
    getSets(fertilizerCosts) <- c("region", "year", "variable")

    fertilizerCosts <- add_dimension(fertilizerCosts, dim = 3.1, add = "scenario", nm = "historical")
    fertilizerCosts <- add_dimension(fertilizerCosts, dim = 3.2, add = "model", nm = datasource)
    weight <- NULL
    units <- "(million US$2017/yr)"
  } else {
    stop("Datasource not valid")
  }

  return(list(x = fertilizerCosts,
              weight = weight,
              unit = units,
              description = "Fertilizer costs"))
}
