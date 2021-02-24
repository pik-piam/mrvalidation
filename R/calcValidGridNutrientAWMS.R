#' @title calcValidGridNutrientAWMS
#' @description reports Nutrient load in animal waste management on 0.5 degree grid
#' 
#' @param nutrient can be c, nr, p, k. For p and k, no losses are assumed in confinements.
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens, Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATtoLPJml}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridNitrogenBudgetCropland")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom mrcommons toolIso2CellCountries

calcValidGridNutrientAWMS <- function(nutrient=c("nr","c")) {
  
  past  <- findset("past")
  out   <- collapseNames(calcOutput("Excretion", cellular = TRUE, attributes = "npkc", aggregate = FALSE)[,past,])[,,nutrient]
 
  getNames(out, dim=3) <- reportingnames(getNames(out, dim=3))
  getNames(out, dim=2) <- reportingnames(getNames(out, dim=2))
  getNames(out, dim=1) <- reportingnames(getNames(out, dim=1))
  
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr and Mt C/yr",
              description="Manure quantities in different animal waste systems.",
              isocountries=FALSE)
  ) 
}

