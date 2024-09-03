#' @title calcValidAgEmployment
#'
#' @description number of people employed in agriculture (crop+livestock production)
#'
#' @param datasource ILO for reporting aggregated employment in crop+livestock production, or ILO_FAO, which uses the
#' same aggregated employment data from ILO, but applies FAO value of production shares to disaggregated between
#' employment in crop and in livestock production.
#' @param dataVersionILO "" for the oldest version, or "monthYear" (e.g. "Aug23") for a newer version
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidAgEmployment", datasource="ILO")
#' }
#'

calcValidAgEmployment <- function(datasource = "ILO", dataVersionILO = "Aug24") {

  if (datasource == "ILO") {
    agEmpl <- calcOutput("AgEmplILO", dataVersionILO = dataVersionILO, aggregate = FALSE)[, , c("Livestock", "Crops")]
    out <- setNames(dimSums(agEmpl, dim = 3), "Labor|Employment|Agricultural employment (mio people)")
    description <- "Employment in agriculture (livestock+crop production) from ILO modelled estimates"
  } else if (datasource == "ILO_FAO") {
    agEmpl <- calcOutput("AgEmplILO", dataVersionILO = dataVersionILO, aggregate = FALSE)[, , c("Livestock", "Crops")]
    out <- setNames(agEmpl, c("Labor|Employment|Agricultural employment|+|Livestock products (mio people)",
                              "Labor|Employment|Agricultural employment|+|Crop products (mio people)"))
    description <- paste0("Employment in agriculture (livestock+crop production) from ILO modelled estimates, ",
                          "disaggregated by applying FAO value of production shares")
  } else {
    stop("Datsource not available")
  }


  if (dataVersionILO == "") dataVersionILO <- "Aug21"
  datasource <- paste(datasource, dataVersionILO, sep = "_")

  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = NULL,
              unit = "mio people",
              description = description))

}
