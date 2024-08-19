#' @title calcValidCostsLabor
#' @description calculates validation data for labor costs
#' 
#' @param datasource Datasource of validation data. 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidCostsLabor")
#' }
#' 
#' @importFrom magpiesets reporthelper summationhelper reportingnames 

calcValidCostsLabor <- function(datasource = "Vittis") {
  if (datasource == "Vittis") {
    labor_costs <- calcOutput("ProductionCosts", datasource = "Vittis", aggregate = FALSE)[, , "Labor", drop = TRUE]
    
    reporting_names <- reportingnames(getNames(labor_costs))
    labor_costs <- reporthelper(labor_costs, dim = 3.1, level_zero_name = "Costs|Labor", detail = TRUE, partly = TRUE)
    labor_costs <- summationhelper(labor_costs)
    labor_costs <- labor_costs[,, reporting_names, pmatch = "right"]
    
    getNames(labor_costs) <- paste0(getNames(labor_costs), " (million US$2017/yr)")
    getSets(labor_costs) <- c("region", "year", "variable")
    labor_costs <- add_dimension(labor_costs, dim = 3.1, add = "scenario", nm = "historical")
    labor_costs <- add_dimension(labor_costs, dim = 3.2, add = "model", nm = datasource)
    
    weight <- NULL
    unit <- "million US$2017/yr"
  } else {
    stop("Datasource not valid")
  }
  return(list(x = labor_costs,
              weight = weight,
              unit = unit,
              description = "Labor costs"))
}
