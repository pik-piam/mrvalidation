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
  
  # Data1 <- NULL
  # Value <- NULL
  # 
    datasource_split <- toolSplitSubtype(datasource, list(version = NULL, climatemodel = NULL, scenario = NULL, years = NULL))
  #   
  #   environment_data <- calcOutput("CollectEnvironmentData_new", subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", sar = 1, aggregate = F, sel_feat = c("tas", "pr", "lwnet", "rsds", "CO2", "Ks", "Sf", "w_pwp", "w_fc", "w_sat", "hsg", "wet"))
  #   weights          <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "weights", sep = ":"), convert = F)
  #   mean_col         <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "mean_col", sep = ":"), convert = F)
  #   stddevs_col      <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "stddevs_col", sep = ":"), convert = F)
  #   mean_lab         <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "mean_lab", sep = ":"), convert = F)
  #   stddevs_lab      <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "stddevs_lab", sep = ":"), convert = F)
  #   inputs           <- as.vector(readSource("GrassSoilEmu", subtype = paste(datasource, model, "inputs", sep = ":"), convert = F))
  #   hist_lsu_ha      <- calcOutput("LsuDensityHist", disagg_type = "grassland", aggregate = F)
    land_ini_LUH2v2  <- calcOutput("LUH2v2", aggregate = F, landuse_types = "LUH2v2", cellular = TRUE)
    soilc_pastr_past <- calcOutput("CollectSoilCarbonPastr", past_mngmt = "me2", lpjml = lpjml, climatemodel = datasource_split$climatemodel, aggregate = F, scenario = paste0(datasource_split$scenario, "_co2_Nreturn0p5_limN"), sar = 1)
  # 
  #   past                  <- intersect(getYears(environment_data), getYears(hist_lsu_ha))
  #   environment_data_past <- environment_data[, past, ]
  #   hist_lsu_ha           <- hist_lsu_ha[, past, ]
  #   input_past            <- mbind(setNames(hist_lsu_ha[, , "range"], grep("lsu", inputs, value = T)), environment_data_past)
  #   input_past            <- as.data.frame(input_past)
  #   input_past_df         <- pivot_wider(input_past, names_from = Data1, values_from = Value)
  #   input_df_scaled_past  <- scale(input_past_df[, inputs], center = mean_col[inputs], scale = stddevs_col[inputs])
  #   soilc_range_past      <- toolNeuralNet(input_df_scaled_past, weights, "softplus")
  #   soilc_range_past      <- soilc_range_past * as.numeric(stddevs_lab) + as.numeric(mean_lab)
  #   soilc_range_past      <- cbind(input_past_df[, c("Cell", "Year")], soilc_range_past)
  #   soilc_range_past      <- as.magpie(soilc_range_past, spatial = 1)
  #   soilc_range_past      <- toolCell2isoCell(soilc_range_past)
    
    soilc_range_past <- calcOutput("RangeSoilCarbonHist", subtype = datasource, model = model, lpjml = lpjml, file = "f31_range_soilc_hist.mz", aggregate = F)
    past <- getYears(soilc_range_past)
    soilc_range_past      <- setNames(land_ini_LUH2v2[, past, "range"] * soilc_range_past, "range")
    soilc_pastr_past      <- setNames(land_ini_LUH2v2[, past, "pastr"] * soilc_pastr_past[,past, "pastr"], "pastr")
    soilc_grassL_past     <- setNames(mbind(soilc_pastr_past, soilc_range_past), c("pastr", "range"))
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    soilc_grassL_past   <- toolAggregate(soilc_grassL_past, rel = mapping,from="celliso",to="iso", dim=1)
    soilc_grassL_past   <- toolCountryFill(soilc_grassL_past, fill=0)
    soilc_grassL_past   <- setNames(soilc_grassL_past, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(soilc_grassL_past, dim = 1)),"|Total (tC)"))
    soilc_grassT_past   <- setNames(dimSums(soilc_grassL_past, dim = 3), paste0("Resources|Soil Carbon|Grassland|Total (tC)"))
    
    soilc_range_past   <- toolAggregate(soilc_range_past, rel = mapping,from="celliso",to="iso", dim=1)
    soilc_range_past   <- toolCountryFill(soilc_range_past, fill=0)
    soilc_range_past_reg   <- setNames(soilc_range_past, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(soilc_range_past, dim = 1)),"|Density (tC per ha)"))
    
    soilc_pastr_past   <- toolAggregate(soilc_pastr_past, rel = mapping,from="celliso",to="iso", dim=1)
    soilc_pastr_past   <- toolCountryFill(soilc_pastr_past, fill=0)
    soilc_pastr_past_reg   <- setNames(soilc_pastr_past, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(soilc_pastr_past, dim = 1)),"|Density (tC per ha)"))
    
    stock <- mbind(soilc_grassL_past, soilc_grassT_past, soilc_range_past_reg, soilc_pastr_past_reg)
  
    stock <- add_dimension(stock, dim=3.1, add="scenario", nm="historical")
    stock <- add_dimension(stock, dim=3.2, add="model",    nm=datasource_split$climatemodel)
    
  return(list(x=stock,
              weight=NULL,
              unit="tC",
              description="Carbon Stocks")
  )
}
