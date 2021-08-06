#' @title calcValidGrassSoilCarbon
#' @description calculates the validation data for grasslands
#' 
#' @param datasource Datasources for validation data
#' @param model trained model ID
#' @param lpjml lpjml version
#' 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Marcos Alves
#' 
#' @examples
#' 
#' \dontrun{ 
#'   calcOutput("ValidGrassSoilCarbon")
#' }
#' @importFrom tidyr pivot_wider


calcValidGrassSoilCarbon <- function(datasource = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", model = "9eaf9b", lpjml){
  
    datasource_split <- toolSplitSubtype(datasource, list(version = NULL, climatemodel = NULL, scenario = NULL, years = NULL))

    land_ini_LUH2v2  <- calcOutput("LUH2v2", aggregate = F, landuse_types = "LUH2v2", cellular = TRUE)
    soilc_pastr_past_tha <- calcOutput("CollectSoilCarbonPastr", past_mngmt = "me2", lpjml = lpjml, climatemodel = datasource_split$climatemodel, aggregate = F, scenario = paste0(datasource_split$scenario, "_co2_Nreturn0p5_limN"), sar = 1)
    soilc_range_past_tha <- calcOutput("RangeSoilCarbonHist", subtype = datasource, model = model, lpjml = lpjml, file = "f31_range_soilc_hist.mz", aggregate = F)
    past <- getYears(soilc_range_past_tha)
    
    # mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    map_reg <- toolGetMapping(type = "regional", name = "clustermapping.csv")
    
    soilc_range_past_mt      <- setNames(land_ini_LUH2v2[, past, "range"] * soilc_range_past_tha, "range")
    soilc_pastr_past_mt      <- setNames(land_ini_LUH2v2[, past, "pastr"] * soilc_pastr_past_tha[,past, "pastr"], "pastr")
    
    soilc_grassL_past_mt     <- setNames(mbind(soilc_pastr_past_mt, soilc_range_past_mt), c("pastr", "range"))
    soilc_grassL_past_mt     <- toolAggregate(soilc_grassL_past_mt, rel = map_reg, from="cell",to="region")
    glo <- dimSums(soilc_grassL_past_mt, dim = 1)
    soilc_grassL_past_mt <- mbind(soilc_grassL_past_mt, glo)
    soilc_grassL_past_mt     <- setNames(soilc_grassL_past_mt, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(soilc_grassL_past_mt, dim = 1)),"|Total (MtC)"))
    soilc_grassL_past_mt_T   <- setNames(dimSums(soilc_grassL_past_mt, dim = 3), paste0("Resources|Soil Carbon|Grassland|Total (MtC)"))
    
    land_ini_LUH2v2_reg <- toolAggregate(land_ini_LUH2v2[, past, c("range", "pastr")], rel = map_reg, from="cell", to="region")
    glo <- dimSums(land_ini_LUH2v2_reg, dim = 1)
    land_ini_LUH2v2_reg <- mbind(land_ini_LUH2v2_reg, glo)
    soilc_range_pastr_tha_reg <- setNames(setNames(soilc_grassL_past_mt, c("pastr", "range")) / setNames(land_ini_LUH2v2_reg, c("pastr", "range")), paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(c("pastr", "range")),"|Density (tC per ha)"))
    soilc_range_pastr_tha_reg[is.infinite(soilc_range_pastr_tha_reg) | is.nan(soilc_range_pastr_tha_reg)] <- 0
    soilc_range_pastr_tha_reg_A <- setNames(dimSums(soilc_range_pastr_tha_reg, dim =3) / dimSums(land_ini_LUH2v2_reg, dim = 3),  paste0("Resources|Soil Carbon|Grassland|Density (tC per ha)"))
    
    stock <- mbind(soilc_grassL_past_mt, soilc_grassL_past_mt_T, soilc_range_pastr_tha_reg, soilc_range_pastr_tha_reg_A)
  
    stock <- add_dimension(stock, dim=3.1, add="scenario", nm="historical")
    stock <- add_dimension(stock, dim=3.2, add="model",    nm=datasource_split$climatemodel)
    
  return(list(x=stock,
              weight=NULL,
              unit="tC",
              description="Carbon Stocks", 
              isocountries = FALSE) 

  )
}
