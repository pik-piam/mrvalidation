#' @title calcValidGridNutrientLossesAWMS
#' @description reports Nutrient Losses in animal waste management on 0.5 degree grid
#' 
#' @param nutrient can be c, nr, p, k. For p and k, no losses are assumed in confinements.
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridNitrogenBudgetCropland")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-
#' @importFrom mrcommons toolIso2CellCountries

calcValidGridNutrientLossesAWMS <-function(nutrient=c("nr","c")) {
 
  past               <- findset("past")
  excretion          <- collapseNames(calcOutput("Excretion", attributes = "npkc", 
                                              cellular = TRUE, cells = "lpjcell",
                                              aggregate = FALSE)[,past,])
  EmissionFactors_n  <- dimSums(calcOutput("EF3confinement", aggregate = FALSE), dim = 3.3)
  LossRates_c        <- calcOutput("ClossConfinement", aggregate = FALSE)
  AnimalWasteMSShare <- collapseNames(calcOutput("AWMSconfShr", aggregate = FALSE)[,past,"constant"])

  EmissionFactors_n  <- toolIso2CellCountries(EmissionFactors_n, cells = "lpjcell")
  LossRates_c        <- toolIso2CellCountries(LossRates_c, cells = "lpjcell")
  AnimalWasteMSShare <- toolIso2CellCountries(AnimalWasteMSShare, cells = "lpjcell")
  
  fuel <- excretion[,,"fuel"]
  
  ManureNitrogen       <- dimSums(collapseNames(excretion[,,"confinement"])[,,"nr"] * AnimalWasteMSShare * EmissionFactors_n,
                                  dim = c("Manure_Management_System_Usage"))
  vcat(verbosity = 2, "no P and K losses in manure management assumed")
  ManurePhosphorKalium <- collapseNames(excretion[,,c("p","k")][,,"confinement"])
  ManureCarbon         <- dimSums(collapseNames(excretion[,,"confinement"])[,,"c"] * AnimalWasteMSShare * LossRates_c,
                                  dim = c("Manure_Management_System_Usage"))

  confinementLoss <- mbind(ManureNitrogen, ManurePhosphorKalium, ManureCarbon)
  confinementLoss <- add_dimension(confinementLoss,dim = 3.1,add = "type",nm = "Losses in confinements")
  out <- mbind(fuel, confinementLoss)
  out <- out[,,nutrient]

  getNames(out,dim=3)<-reportingnames(getNames(out,dim=3))
  getNames(out,dim=2)<-reportingnames(getNames(out,dim=2))

  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr and Mt C/yr",
              description="Losses in confinemnet and fuel use of manure",
              isocountries=FALSE)) 
}

