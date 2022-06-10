#' @title calcValidAgEmployment
#'
#' @description number of people employed in agriculture (crop+livestock production)
#'
#' @param datasource So far only "ILO".
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidAgEmployment", datasource="ILO")
#' }
#'

calcValidAgEmployment <- function(datasource = "ILO") {

  if (datasource == "ILO") {
    agEmpl <- calcOutput("AgEmplILO", aggregate = FALSE)[, , c("Livestock", "Crops")]
    agEmpl <- setNames(dimSums(agEmpl, dim = 3), "Agricultural employment (mio people)")
  } else {
    stop("Datsource not available")
  }

  agEmpl <- add_dimension(agEmpl, dim = 3.1, add = "scenario", nm = "historical")
  agEmpl <- add_dimension(agEmpl, dim = 3.2, add = "model", nm = datasource)

  return(list(x = agEmpl,
              weight = NULL,
              unit = "mio people",
              description = "Employment in agriculture (livestock+crop production), based on ILO modelled estimates"))

}
