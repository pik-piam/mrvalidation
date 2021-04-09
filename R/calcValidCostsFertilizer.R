#' @title calcValidCostsFertilizer
#' @description calculates validation data for fertilizer costs
#' 
#' @param datasource Datasource of validation data. 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidCostsFertilizer")
#' }
#' 
#' @importFrom magpiesets reporthelper summationhelper reportingnames
#' 
calcValidCostsFertilizer <- function(datasource = "Vittis") {
  if (datasource == "Vittis") {
    fertilizer_costs <- calcOutput("ProductionCosts", datasource = "Vittis", aggregate = FALSE)[, , "Fertilizer", drop = TRUE]
    
    reporting_names <- reportingnames(getNames(fertilizer_costs))
    fertilizer_costs <- reporthelper(fertilizer_costs, dim = 3.1, level_zero_name = "Costs|Fertilizer", detail = TRUE, partly = TRUE)
    fertilizer_costs <- summationhelper(fertilizer_costs)
    fertilizer_costs <- fertilizer_costs[,, reporting_names, pmatch = "right"]
    
    getNames(fertilizer_costs) <- paste0(getNames(fertilizer_costs), " (million US$05/yr)")
    getSets(fertilizer_costs) <- c("region", "year", "variable")
    fertilizer_costs <- add_dimension(fertilizer_costs, dim = 3.1, add = "scenario", nm = "historical")
    fertilizer_costs <- add_dimension(fertilizer_costs, dim = 3.2, add = "model", nm = datasource)

    weight = NULL
    units = "(million US$05/yr)"
  } else {
    stop("Datasource not valid")
  }
  return(list(x = fertilizer_costs,
              weight = weight,
              unit = units,
              description = "Fertilizer costs"))
}