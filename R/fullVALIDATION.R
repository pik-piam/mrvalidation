#' fullValidation
#'
#' Function that produces the complete validation data set used for evaluation of MAgPIE outputs
#'
#' @param rev data revision which should be used as input. Will be converted to
#' \code{\link[base]{numeric_version}} when called via \code{\link[madrat]{retrieveData}}.
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' retrieveData("Validation")
#' }
#'
fullVALIDATION <- function(rev = 0.1) {

  if (rev < numeric_version("4.66")) {
    stop("mrvalidation(>= 2.34.0) does not support revision below 4.66 anymore. ",
         "Please use a older snapshot/version of the library, if you need older revisions.")
  }
  # all validation data regional aggregations happens here
  # for the first variable output calculation, append paramenter should be set to FALSE so that the
  # eventually exitsting "validation.mif" file is deleted at the begining.
  valfile <- "validation.mif"


  # Population and Income
  calcOutput(type = "ValidPopulation", aggregate = "REG+GLO", file = valfile,
             append = FALSE, warnNA = FALSE, try = TRUE) # ready
  calcOutput(type = "ValidIncome", datasource = "WDI-MI_SSPs-MI",
             aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidAgFFGDP", datasource = "WDI", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidAgGDP", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidAgGDP", datasource = "FAO_consum", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidConsumptionValue", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidAgEmployment", datasource = "ILO", dataVersionILO = "Aug24", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidAgEmployment", datasource = "ILO_FAO", dataVersionILO = "Aug24", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidHourlyLaborCosts", datasource = "ILO_completed", dataVersionILO = "Aug24",
             aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidHourlyLaborCosts", datasource = "USDA_FAO_completed", dataVersionILO = "Aug24",
             aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidWageDevelopment", datasource = "ILO_completed", dataVersionILO = "Aug24", baseYear = 2000,
             aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidWageDevelopment", datasource = "ILO_completed", dataVersionILO = "Aug24", baseYear = 2010,
             aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidWageDevelopment", datasource = "USDA_FAO_completed", dataVersionILO = "Aug24",
             baseYear = 2000, aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidWageDevelopment", datasource = "USDA_FAO_completed", dataVersionILO = "Aug24",
             baseYear = 2010, aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidGini", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidPovertyLine", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)

  # Food Demand
  calcOutput(type = "ValidKcal", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidKcal", datasource = "FAOmassbalance", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLivestockShare", aggregate = "REG+GLO", file = valfile,
             append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidVegfruitShare", aggregate = "REG+GLO", file = valfile,
             append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLivestockDemStructure", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidFoodExpenditureShare", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidFoodExpenditure", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)


  # Demand, Production, Trade, Self-Sufficiency
  calcOutput(type = "ValidDemand", aggregate = "REG+GLO", file = valfile,
             append = TRUE, detail = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidDemandBioenergy", aggregate = "REG+GLO", file = valfile,
             append = TRUE, warnNA = FALSE, try = TRUE) # ready
  calcOutput(type = "ValidProduction", aggregate = "REG+GLO", file = valfile,
             append = TRUE, detail = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidTrade", aggregate = "REG+GLO", file = valfile,
             append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidTrade", aggregate = "REG+GLO", file = valfile,
             append = TRUE,  net_trade = FALSE, equalized = FALSE, try = TRUE) # ready
  calcOutput(type = "ValidTrade", aggregate = "REG+GLO",
             datasource = "FAOBilateral", file = valfile,
             append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidTrade", aggregate = "REG+GLO",
             datasource = "FAOBilateral",  file = valfile,
             append = TRUE,  net_trade = FALSE, equalized = FALSE, try = TRUE) # ready
  calcOutput(type = "ValidSelfsuff", aggregate = "REG+GLO", file = valfile,
             append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidFeed", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidProcessing", datasource = "FAO", nutrient = "dm", detail = TRUE,
             indicator = "primary_to_process", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidProcessing", datasource = "FAO", nutrient = "dm", detail = TRUE,
             indicator = "secondary_from_primary", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidGrassLSUha", aggregate = "REG+GLO", datasource = "MAgPIEown",
             file = valfile, append = TRUE, try = TRUE)

  # Resources:
  # Croparea
  calcOutput(type = "ValidCroparea", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, detail = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidCroparea", datasource = "ostberg2023", aggregate = "REG+GLO",
             file = valfile, append = TRUE, detail = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidCroparea", datasource = "FAOfallow", aggregate = "REG+GLO",
             file = valfile, append = TRUE, detail = TRUE, try = TRUE) # ready

  # Land Cover
  calcOutput(type = "ValidLand", datasource = "FAO_crop_past", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLand", datasource = "FAO_forest", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLand", datasource = "LUH2v2", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLand", datasource = "MAgPIEown", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLand", datasource = "SSPResults", aggregate = "REG+GLO",
             file = valfile, append = TRUE, warnNA = FALSE, try = TRUE) # ready
  calcOutput(type = "ValidLand", datasource = "FRA2020", aggregate = "REG+GLO",
             file = valfile, append = TRUE, warnNA = FALSE, try = TRUE) # ready

  # Land Cover Change
  calcOutput(type = "ValidLandChange", datasource = "FAO_crop_past", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLandChange", datasource = "FAO_forest", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLandChange", datasource = "LUH2v2", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLandChange", datasource = "MAgPIEown", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidLandChange", datasource = "SSPResults", baseyear = 2005, # 1995 not available as baseyear
             aggregate = "REG+GLO", file = valfile, append = TRUE, warnNA = FALSE, try = TRUE) # ready

  # WaterUsage
  calcOutput(type = "ValidWaterUsage", datasource = "foley_2011", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "shiklomanov_2000", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "wada_2011", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "wisser_2008", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "fischer_IIASA", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "hejazi_2013", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "molden_IWMI", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "seckler_IWMI", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "shiklomanov", aggregate = FALSE,
             file = "validation.mif", append = TRUE, try = TRUE)

  calcOutput(type = "ValidWaterUsage", datasource = "LPJmL:ipsl-cm5a-lr", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "MATSIRO:ipsl-cm5a-lr", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidWaterUsage", datasource = "MPI-HM:ipsl-cm5a-lr", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)

  # Area equipped for Irrigation
  calcOutput(type = "ValidAEI", datasource = "LUH2v2", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidAEI", datasource = "Mehta2024_Siebert2013", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidAEI", datasource = "HID", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidAEI", datasource = "GMIA", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)

  # Area actually irrigated
  calcOutput(type = "ValidAAI", datasource = "LUH2v2", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)
  calcOutput(type = "ValidAAI", datasource = "GMIA", aggregate = "REG+GLO",
             file = "validation.mif", append = TRUE, try = TRUE)

  # Nitrogen
  calcOutput(type = "ValidNitrogenBudgetCropland", datasource = "Bodirsky", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidNitrogenBudgetCropland", datasource = "Lassaletta2014", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidNitrogenBudgetCropland", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidNitrogenBudgetCropland", datasource = "ACCMIP", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidNitrogenBudgetPasture", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidNitrogenBudgetNonagland", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidManure", datasource = "IPCC", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidManure", datasource = "Bodirsky", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidNitrogenSurplus", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)

  # Carbon
  calcOutput("ValidResidues", aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput("ValidSOCStocks", datasource = "histSOCbudget", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput("ValidSOCShare", datasource = "histSOCbudget", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)

  # Carbon Stocks
  calcOutput("ValidCarbon", datasource = "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  # calcOutput("ValidGrassSoilCarbon", datasource = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", model = "c7491e",
  #           lpjml = "lpjml5p2_pasture", aggregate = FALSE, file = valfile, append = TRUE, try = TRUE)
  # Temporaraly swiched off for debugging


  # Growing Stocks
  calcOutput("ValidGS", datasource = "FAO", aggregate = "REG+GLO", indicator = "relative",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput("ValidGS", datasource = "FAO", aggregate = "REG+GLO", indicator = "absolute",
             file = valfile, append = TRUE, try = TRUE)

  # GHG emissions
  calcOutput(type = "ValidEmissions", datasource = "EDGAR_LU", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissions", datasource = "CEDS", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissions", datasource = "FAO_EmisLUC", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE, warnNA = FALSE) # ready
  calcOutput(type = "ValidEmissions", datasource = "FAO_EmisAg", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE, warnNA = FALSE) # ready
  calcOutput(type = "ValidEmissions", datasource = "GFED", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissions", datasource = "EDGAR6", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissions", datasource = "PRIMAPhist", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissions", datasource = "IPCC", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissions", datasource = "Nsurplus2", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmisLucGlo", subtype = "Canadell_2007", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmisLucGlo", subtype = "Friedlingstein_2010", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmisLucGlo", subtype = "Harris_2013", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmisLucGlo", subtype = "Houghton_2012", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmisLucGlo", subtype = "RCP", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidAnnualCarbonLTS", datasource = "Lauk_et_al", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidAnnualCarbonLTS", datasource = "Johnston_Radeloff", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidAnnualCarbonLTS", datasource = "Johnston_Radeloff_P", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidCumulativeCarbonLTS", datasource = "Johnston_Radeloff", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidCumulativeCarbonLTS", datasource = "Johnston_Radeloff_P", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmisLucGasser", subtype = "bookkeeping", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissionsAFOLU", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissionsAFOLU", datasource = "FAO", aggregate = "REG+GLO", cumulative = TRUE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissionsAFOLU", datasource = "EDGAR_LU", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissionsAFOLU", datasource = "EDGAR_LU", aggregate = "REG+GLO", cumulative = TRUE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidEmissionsPeatland", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidGlobalCarbonBudget", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidGlobalCarbonBudget", aggregate = FALSE, cumulative = TRUE,
             file = valfile, append = TRUE, try = TRUE) # ready

  # Yield
  calcOutput(type = "ValidYield", datasource = "FAO", aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidYield", datasource = "Ostberg2023_FAO_LUH2v2",
             aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)

  # Productivity
  calcOutput(type = "ValidTau", aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidTau", datasource = "FAOonline", aggregate = "REG+GLO", file = valfile,
             append = TRUE, try = TRUE)
  calcOutput(type = "ValidTauPastr", aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidLSUdensity", aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidAgriResearchIntensity", aggregate = "REG+GLO", datasource = "Pardey",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidFeedConversion", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)



  # Prices
  calcOutput(type = "ValidPriceAgriculture", datasource = "WBGEM", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidPriceAgriculture", datasource = "IMPACT3.2.2World_Price", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE) # ready
  calcOutput(type = "ValidPriceAgriculture", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidPriceAgriculture", datasource = "IniFoodPrice", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE)

  calcOutput(type = "ValidPriceBioenergy", aggregate = "REG+GLO", file = valfile, append = TRUE,
             warnNA = FALSE, try = TRUE) # ready
  calcOutput(type = "ValidPriceGHG",   datasource = "SSPResults", aggregate = "REG+GLO",
             file = valfile, append = TRUE, warnNA = FALSE, try = TRUE)

  # PriceIndex
  calcOutput(type = "ValidPriceIndex", datasource = "FAO", baseyear = "y2010", round = TRUE,
             aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE)

  # SDG
  calcOutput(type = "ValidSDG1", datasource = "James", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidSDG12", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)

  # Forestry run specific
  calcOutput(type = "ValidTimber", aggregate = "REG+GLO", file = valfile, append = TRUE, try = TRUE) # ready

  # Costs validation
  calcOutput(type = "ValidCostsOverall", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE) # Overall costs.
  calcOutput(type = "ValidCostsTransport", datasource = "GTAPtransport", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCostsTransport", datasource = "MAgPIEcalc", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCostsCapStocks", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCostsFertilizer", datasource = "FAO", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCostsLabor", datasource = "Vittis", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCostsTC", datasource = "Pardey", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCostsAEI", datasource = "IMPACT", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidTotalLaborCosts", datasource = "ILO", dataVersionILO = "Aug24", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidTotalLaborCosts", datasource = "USDA", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidTotalLaborCosts", datasource = "GTAP", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCostsTransport", datasource = "GTAPwholesale", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)

  calcOutput(type = "ValidFactorReqShares", subtype = "crop", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidFactorReqShares", subtype = "livestock", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)

  # Diversity indices
  calcOutput(type = "ValidBII", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCropareaDiversity", index = "shannon", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCropareaDiversity", index = "invsimpson", aggregate = "REG+GLO",
             file = valfile, append = TRUE, try = TRUE)

  # Global surface temperature
  calcOutput(type = "ValidGlobalSurfaceTemp", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE)
  calcOutput(type = "ValidCMIP6", aggregate = FALSE,
             file = valfile, append = TRUE, try = TRUE)

}
