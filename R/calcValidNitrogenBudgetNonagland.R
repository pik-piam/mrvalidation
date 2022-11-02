#' @title calcValidNitrogenBudgetNonagland
#' @description reports nitrogen budget for non-agricultural land types
#' @author Michael Crawford
#'
#' @return MAgPIE object with results on the iso level, unweighted, unit, and description
#'
#' @examples
#'
#' \dontrun{
#' calcOutput("calcValidNitrogenBudgetNonagland")
#' }

calcValidNitrogenBudgetNonagland <- function() {

  out <- calcOutput("NitrogenBudgetNonagland", cellular = TRUE, aggregate = FALSE)
  out <- dimSums(out, dim = 3.1)

  mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
  out <- toolAggregate(x = out, rel = mapping, from = "celliso", to = "iso")
  out <- toolCountryFill(x = out, fill = 0)

  out <- out[, , "surplus"] # At the moment we focus only on the nutrient surplus

  getNames(out) <- "Resources|Nitrogen|Non-agricultural Land Budget|Nutrient Surplus (Mt Nr/yr)"
  getSets(out)["d1.1"] <- "iso"
  getSets(out)["d3.1"] <- "variable"
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = "MAgPIE")
  getComment(out) <- NULL

  return(list(x           = out,
              weight      = NULL,
              unit        = "Mt Nr/yr",
              description = "Nitrogen Budget for non-agricultural land"))

}
