#' @title calcValidNitrogenSurplus
#' @description reports nutrient surplus for all land types
#' @author Michael Crawford
#'
#' @return MAgPIE object with results on the iso level, unweighted, unit, and description
#'
#' @examples
#'
#' \dontrun{
#' calcOutput("calcValidNitrogenSurplus")
#' }

calcValidNitrogenSurplus <- function() {

  # Manure nutrient surplus
  manure <- calcOutput("ValidManure", aggregate = FALSE)
  manure <- manure[, , "historical.Bodirsky.Resources|Nitrogen|Manure (Mt Nr/yr)"]

  # Non-agricultural land nutrient surplus
  nonagland <- calcOutput("ValidNitrogenBudgetNonagland", aggregate = FALSE)
  nonagland <- nonagland[, , "historical.MAgPIE.Resources|Nitrogen|Non-agricultural Land Budget|Nutrient Surplus (Mt Nr/yr)"] # nolint

  # Cropland land nutrient surplus
  cropland <- calcOutput("ValidNitrogenBudgetCropland", aggregate = FALSE)
  cropland <- cropland[, , "Bodirsky.historical.Resources|Nitrogen|Cropland Budget|Balance|+|Nutrient Surplus (Mt Nr/yr)"] # nolint

  # Pasture land nutrient surplus
  pasture <- calcOutput("ValidNitrogenBudgetPasture", aggregate = FALSE)
  pasture <- pasture[, , "Bodirsky.historical.Resources|Nitrogen|Pasture Budget|Balance|+|Nutrient Surplus (Mt Nr/yr)"] # nolint

  allReports <- list(manure, nonagland, cropland, pasture)
  yearsPresent <- Reduce(f = intersect, x = Map(getYears, allReports))
  out <- mbind(lapply(X = allReports, FUN = function(.x) .x[, yearsPresent, ]))
  out <- dimSums(out, dim = 3)

  getNames(out) <- "Resources|Nitrogen|Nutrient Surplus (Mt Nr/yr)"
  getSets(out)["d1.1"] <- "iso"
  getSets(out)["d3.1"] <- "variable"
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = "combined")
  getComment(out) <- NULL

  return(list(x           = out,
              weight      = NULL,
              unit        = "Mt Nr/yr",
              description = "Total nutrient surplus inc. non-agricultural land")
  )

}
