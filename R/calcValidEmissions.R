#' @title calcValidEmissions
#' @description validation fo emissions
#'
#' @param datasource The Emission Inventory that shall be used. For futher
#' information, best see mrcommons function calcEmissionInventory. Options are
#' e.g.  CEDS, combined_CEDS_IPCC (including own estimates where available),
#' IPCC(own estimates), Nsurplus (own estimates)
#'
#' @return List of magpie object with results on country level, weight on
#' country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("ValidEmissions")
#' }
#'
#' @importFrom  magpiesets reportingnames
#'
calcValidEmissions <- function(datasource = "CEDS") {

  out <- calcOutput("LandEmissions", aggregate = FALSE, datasource = datasource, warnNA = FALSE)
  if ("Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)" %in% getNames(out, dim = 3)) {
    emisCO2 <- out[, , "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"]
    getNames(emisCO2, dim = 3) <- "Emissions|CO2|Land RAW|+|Land-use Change (Mt CO2/yr)"
    out <- mbind(out, emisCO2)
  }

  return(list(x = out,
              weight = NULL,
              unit = "Mt",
              description = "historic emissions in 1970-2015. NOx is in NO2 equivalents."))
}
