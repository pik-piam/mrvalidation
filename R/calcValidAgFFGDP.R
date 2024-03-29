#' @title calcValidAgFFGDP
#' @description validation for agricultural, fisheries, forestry value added gdp (Million 05USD)
#' @param datasource datasource for validation (WDI)
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author David Chen
#' @examples
#' \dontrun{
#' calcOutput("ValidAgFFGDP")
#' }
calcValidAgFFGDP <- function(datasource = "WDI") {
  if (datasource != "WDI") {
    stop("Unkown datasource.")
  }

  # This data is currently in constant 2015 US$MER.
  agffVA <- madrat::readSource("WDI", subtype = "NV.AGR.TOTL.KD")
  # Convert to constant 2005 US$MER.
  agffVA05 <- GDPuc::convertGDP(agffVA, "constant 2015 US$MER", "constant 2005 US$MER", replace_NAs = c("linear", 0))

  out <- agffVA05
  magclass::getNames(out) <- "Value|Agriculture, Forestry and Fisheries GDP (million US$05/yr)"
  out <- magclass::add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- magclass::add_dimension(out, dim = 3.2, add = "model", nm = "WDI")

  list(x = out,
       weight = NULL,
       unit = "million US$05/yr",
       description = "Agriculture Fisheries Forestry Value added GDP")
}
