#' fullValidation
#' 
#' Function that produces the complete validation data set used for evaluation of MAgPIE outputs
#' 
#' @param rev data revision which should be used as input (positive numeric).
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#' 
#' \dontrun{ 
#' retrieveData("Validation")
#' }
#' @importFrom madrat getConfig 
fullVALIDATION <- function(rev=0.1) {
  
  # all validation data regional aggregations happens here
  # for the first variable output calculation, append paramenter should be set to FALSE so that the 
  ## eventually exitsting "validation.mif" file is deleted at the begining. 
  # setting rev to -1 will allow for just writing the validation
  valfile <- "validation.mif"
  
  #Population and Income
  calcOutput(type="ValidPopulation", aggregate="REG+GLO", file=valfile, append=FALSE, na_warning=FALSE, try=TRUE) #ready
  calcOutput(type="ValidIncome", datasource="James", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) 
  calcOutput(type="ValidIncome", datasource="James_OECD_Nakicenovic",aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) 
  calcOutput(type="ValidAgGDP", datasource="WDI", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  
  # Food Demand
  calcOutput(type="ValidKcal", datasource="FAO",aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidKcal", datasource="FAOmassbalance",aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidKcal", datasource="Bodirsky2015", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLivestockShare", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidVegfruitShare", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLivestockDemStructure", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidFoodExpenditureShare", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput(type="ValidFoodExpenditure", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  
  
  # Demand, Production, Trade, Self-Sufficieny
  calcOutput(type="ValidDemand", aggregate="REG+GLO", file=valfile, append=TRUE,detail=TRUE, try=TRUE) #ready
  calcOutput(type="ValidDemandBioenergy", aggregate="REG+GLO", file=valfile, append=TRUE, na_warning = FALSE, try=TRUE) #ready
  calcOutput(type="ValidProduction", aggregate="REG+GLO", file=valfile, append=TRUE,detail=TRUE, try=TRUE) #ready
  calcOutput(type="ValidTrade", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidSelfsuff", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidFeed", datasource="FAO", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidProcessing", datasource="FAO", nutrient="dm", detail=TRUE, indicator="primary_to_process", file=valfile, append=TRUE, try=TRUE)
  calcOutput(type="ValidProcessing", datasource="FAO", nutrient="dm", detail=TRUE, indicator="secondary_from_primary", file=valfile, append=TRUE, try=TRUE)
  
  
  # Resources: 
  #Croparea
  calcOutput(type="ValidCroparea", datasource="FAO", aggregate="REG+GLO", file=valfile, append=TRUE,detail=TRUE, try=TRUE) #ready
  # Land Cover
  calcOutput(type="ValidLand", datasource="FAO_crop_past", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLand", datasource="FAO_forest", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLand", datasource="LUH2v2", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLand", datasource="MAgPIEown", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLand", datasource="SSPResults", aggregate="REG+GLO", file=valfile, append=TRUE, na_warning = FALSE, try=TRUE) #ready
  # Land Cover Change
  calcOutput(type="ValidLandChange", datasource="FAO_crop_past", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLandChange", datasource="FAO_forest", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLandChange", datasource="LUH2v2", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLandChange", datasource="MAgPIEown", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidLandChange", datasource="SSPResults", aggregate="REG+GLO", file=valfile, append=TRUE, na_warning = FALSE, try=TRUE) #ready
  
  #WaterUsage
  calcOutput(type="ValidWaterUsage",datasource="foley_2011", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="shiklomanov_2000", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="wada_2011", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="wisser_2008", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="fischer_IIASA", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="hejazi_2013", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="molden_IWMI", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="seckler_IWMI", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidWaterUsage",datasource="shiklomanov", aggregate = FALSE, file="validation.mif", append = TRUE, try=TRUE)
  
  #Area equipped for Irrigation
  calcOutput(type="ValidAEI",datasource="LUH2v2", aggregate = "REG+GLO", file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidAEI",datasource="HID", aggregate = "REG+GLO", file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidAEI",datasource="GMIA", aggregate = "REG+GLO", file="validation.mif", append = TRUE, try=TRUE)
  
  #Area actually irrigated
  calcOutput(type="ValidAAI",datasource="LUH2v2", aggregate = "REG+GLO", file="validation.mif", append = TRUE, try=TRUE)
  calcOutput(type="ValidAAI",datasource="GMIA", aggregate = "REG+GLO", file="validation.mif", append = TRUE, try=TRUE)
  
  #Nitrogen
  calcOutput(type="ValidNitrogenBudgetCropland", datasource="Bodirsky", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidNitrogenBudgetCropland", datasource="Lassaletta2014", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidNitrogenBudgetCropland", datasource="FAO", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput(type="ValidNitrogenBudgetCropland", datasource="ACCMIP", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput(type="ValidNitrogenBudgetPasture", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput(type="ValidManure", datasource="IPCC", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidManure", datasource="Bodirsky", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  
  #Carbon Stocks
  calcOutput("ValidCarbonStocks", datasource = "LPJ_IPCC2006", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonStocks", datasource = "LPJmL_rev21", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonStocks", datasource = "LPJmLCarbon", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonStocks", datasource = "WISE", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonStocks", datasource = "GSOC", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonStocks", datasource = "SoilGrids", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  
  calcOutput("ValidCarbonDensity", datasource = "LPJ_IPCC2006", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonDensity", datasource = "LPJmL_rev21", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonDensity", datasource = "LPJmLCarbon", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonDensity", datasource = "WISE", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonDensity", datasource = "GSOC", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput("ValidCarbonDensity", datasource = "SoilGrids", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  
  
  #GHG emissions
  calcOutput(type="ValidEmissions", datasource="EDGAR_LU", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmissions", datasource="CEDS", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmissions", datasource="FAO_EmisLUC", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmissions", datasource="FAO_EmisAg", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmissions", datasource="PRIMAPhist", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmissions", datasource="IPCC", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmissions", datasource="Nsurplus2", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmisLucGlo", subtype="Canadell_2007", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmisLucGlo", subtype="Friedlingstein_2010", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmisLucGlo", subtype="Harris_2013", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmisLucGlo", subtype="Houghton_2012", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidEmisLucGlo", subtype="RCP", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE) #ready
  
  #Yield
  calcOutput(type="ValidYield", datasource="FAO", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  
  #Productivity
  calcOutput(type="ValidTau", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE) #ready
  
  #Prices
  calcOutput(type="ValidPriceAgriculture", datasource="WBGEM", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE) 
  calcOutput(type="ValidPriceAgriculture", datasource="IMPACT3.2.2World_Price", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE) #ready
  calcOutput(type="ValidPriceAgriculture", datasource="FAO", aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  calcOutput(type="ValidPriceAgriculture", datasource="IniFoodPrice", aggregate=FALSE, file=valfile, append=TRUE, try=TRUE)
  
  calcOutput(type="ValidPriceBioenergy", aggregate="REG+GLO", file=valfile, append=TRUE, na_warning = FALSE, try=TRUE) #ready
  calcOutput(type="ValidPriceGHG",   datasource="SSPResults", aggregate="REG+GLO", file=valfile, append=TRUE, na_warning = FALSE, try=TRUE)
  
  #PriceIndex
  calcOutput(type="ValidPriceIndex", datasource="FAO", baseyear="y2010", round=TRUE, aggregate="REG+GLO", file=valfile, append=TRUE, try=TRUE)
  
}
