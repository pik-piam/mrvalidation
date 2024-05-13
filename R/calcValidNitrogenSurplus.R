#' @title calcValidNitrogenSurplus
#' @description reports nutrient surplus for all land types
#' @author Michael Crawford
#'
#' @return MAgPIE object with results on the iso level, unweighted, unit, and description
#'
#' @examples
#' \dontrun{
#' calcOutput("calcValidNitrogenSurplus")
#' }

calcValidNitrogenSurplus <- function() {

  # Manure nutrient surplus
  manure <- calcOutput("EmisNitrogenAWMSPast", aggregate = FALSE)
  manure <- dimSums(manure, dim = c(3.1, 3.2))
  manure <- manure[, , c("no3_n", "n2_n", "nh3_n", "no2_n", "n2o_n_direct")] # Remove recycling
  manure <- dimSums(manure, dim = 3)
  getNames(manure) <- "Bodirsky.historical.Resources|Nitrogen|Manure|Manure In Confinements|+|Losses (Mt Nr/yr)"
  getSets(manure) <- c("iso", "year", "model", "scenario", "d3")

  # Non-agricultural land nutrient surplus
  nonagland <- calcOutput("ValidNitrogenBudgetNonagland", aggregate = FALSE)
  nonagland <- nonagland[, , "historical.MAgPIE.Resources|Nitrogen|Non-agricultural Land Budget|Nutrient Surplus (Mt Nr/yr)"] # nolint

  # Cropland land nutrient surplus
  cropland <- calcOutput("ValidNitrogenBudgetCropland", aggregate = FALSE)
  cropland <- cropland[, , "based on Bodirsky et al 2014.historical.Resources|Nitrogen|Cropland Budget|Balance|+|Nutrient Surplus (Mt Nr/yr)"] # nolint

  # Pasture land nutrient surplus
  pasture <- calcOutput("ValidNitrogenBudgetPasture", aggregate = FALSE)
  pasture <- pasture[, , "Bodirsky.historical.Resources|Nitrogen|Pasture Budget|Balance|+|Nutrient Surplus (Mt Nr/yr)"] # nolint

  allReports <- list(manure, nonagland, cropland, pasture)
  yearsPresent <- Reduce(f = intersect, x = Map(getYears, allReports))
  out <- mbind(lapply(X = allReports, FUN = function(.x) .x[, yearsPresent, ]))
  out <- dimSums(out, dim = 3)

  getNames(out) <- "Resources|Nitrogen|Nutrient surplus from land and manure management (Mt Nr/yr)"
  getSets(out)["d1.1"] <- "iso"
  getSets(out)["d3.1"] <- "variable"
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = "MADRaT")
  getComment(out) <- NULL

  return(list(x           = out,
              weight      = NULL,
              unit        = "Mt Nr/yr",
              description = "Total nutrient surplus including non-agricultural land")
  )

}
