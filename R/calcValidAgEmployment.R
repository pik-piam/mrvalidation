#' @title calcValidAgEmployment
#'
#' @description number of people employed in agriculture (crop+livestock production)
#'
#' @param datasource ILO for reporting aggregated employment in crop+livestock production, or ILO_FAO, which uses the
#' same aggregated employment data from ILO, but applies FAO value of production shares to disaggregated between
#' employment in crop and in livestock production.
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
    out <- setNames(dimSums(agEmpl, dim = 3), "Agricultural employment|Crop and livestock products (mio people)")
    description <- "Employment in agriculture (livestock+crop production) from ILO modelled estimates"
  } else if (datasource == "ILO_FAO") {
    agEmpl <- calcOutput("AgEmplILO", aggregate = FALSE)[, , c("Livestock", "Crops")]
    out <- setNames(agEmpl, c("Agricultural employment|+|Livestock products (mio people)",
                              "Agricultural employment|+|Crop products (mio people)"))
    description <- paste0("Employment in agriculture (livestock+crop production) from ILO modelled estimates, ",
                          "disaggregated by applying FAO value of production shares")
  } else {
    stop("Datsource not available")
  }

  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = NULL,
              unit = "mio people",
              description = description))

}
