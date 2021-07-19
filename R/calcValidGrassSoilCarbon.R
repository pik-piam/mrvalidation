#' @title calcValidGrassSoilCarbon
#' @description calculates the validation data for grasslands
#' 
#' @param datasource Datasources for validation data
#' @param model trained model ID
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

calcValidGrassSoilCarbon <- function(datasource = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", model = "5f5fa2"){
  
  Data1 <- NULL
  Value <- NULL
  
    datasource_split <- toolSplitSubtype(datasource, list(version = NULL, climatemodel = NULL, scenario = NULL, years = NULL))
    
    environment_data <- calcOutput("CollectEnvironmentData_new", subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", sar = 1, aggregate = F, sel_feat = c("tas", "pr", "lwnet", "rsds", "CO2", "Ks", "Sf", "w_pwp", "w_fc", "w_sat", "hsg"))
    weights          <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "weights", sep = ":"), convert = F)
    mean_col         <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "mean_col", sep = ":"), convert = F)
    stddevs_col      <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "stddevs_col", sep = ":"), convert = F)
    mean_lab         <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "mean_lab", sep = ":"), convert = F)
    stddevs_lab      <- readSource("GrassSoilEmu", subtype = paste(datasource, model, "stddevs_lab", sep = ":"), convert = F)
    inputs           <- as.vector(readSource("GrassSoilEmu", subtype = paste(datasource, model, "inputs", sep = ":"), convert = F))
    hist_lsu_ha      <- calcOutput("LsuDensityHist", disagg_type = "grassland", aggregate = F)
    land_ini_LUH2v2  <- calcOutput("LUH2v2", aggregate = F, landuse_types = "LUH2v2", cellular = TRUE)

    past                  <- intersect(getYears(environment_data), getYears(hist_lsu_ha))
    environment_data_past <- environment_data[, past, ]
    hist_lsu_ha           <- hist_lsu_ha[, past, ]
    input_past            <- mbind(setNames(hist_lsu_ha[, , "range"], grep("lsu", inputs, value = T)), environment_data_past)
    input_past            <- as.data.frame(input_past)
    input_past_df         <- pivot_wider(input_past, names_from = Data1, values_from = Value)
    input_df_scaled_past  <- scale(input_past_df[, inputs], center = mean_col[inputs], scale = stddevs_col[inputs])
    soil_carbon_past      <- toolNeuralNet(input_df_scaled_past, weights, "softplus")
    soil_carbon_past      <- soil_carbon_past * as.numeric(stddevs_lab) + as.numeric(mean_lab)
    soil_carbon_past      <- cbind(input_past_df[, c("Cell", "Year")], soil_carbon_past)
    soil_carbon_past      <- as.magpie(soil_carbon_past, spatial = 1)
    soil_carbon_past      <- toolCell2isoCell(soil_carbon_past)
    soil_range_past       <- land_ini_LUH2v2[, past, "range"] * soil_carbon_past
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    stock   <- toolAggregate(soil_range_past, rel = mapping,from="celliso",to="iso", dim=1)
    stock   <- toolCountryFill(stock, fill=0)
    stock   <- setNames(stock, "Resources|Soil Carbon|Grassland|Range|Total (tC)")
    
    stock <- add_dimension(stock, dim=3.1, add="scenario", nm="historical")
    stock <- add_dimension(stock, dim=3.2, add="model",    nm=datasource_split$climatemodel)
    
  return(list(x=stock,
              weight=NULL,
              unit="tC",
              description="Carbon Stocks")
  )
}
