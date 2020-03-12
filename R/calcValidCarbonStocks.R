#' @title calcValidCarbonStocks
#' @description calculates the validation data for the carbon pools
#' 
#' @param datasource Datasources for validation data, e.g. LPJ_IPCC2006, LPJmL_natural
#' @param baseyear baseyear for calculating soil carbon stock change
#' 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' 
#' \dontrun{ 
#'   calcOutput("ValidCarbonStocks")
#' }
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass mbind getYears setYears nregions

calcValidCarbonStocks <- function(datasource="LPJ_IPCC2006", baseyear=1995){
  
  if(datasource=="LPJ_IPCC2006"){
    
    mapping    <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    SOM_stock  <- calcOutput("SOM", subtype="stock",   aggregate = FALSE)
    SOM_stock  <- toolAggregate(SOM_stock, rel=mapping, from=ifelse(nregions(SOM_stock)>1,"celliso","cell"), to="iso", dim=1)
    SOM_stock  <- toolCountryFill(SOM_stock,fill=0)
    SOM_stock  <- mbind(SOM_stock, add_dimension(dimSums(SOM_stock, dim=3.1), add="landuse", nm="total"))
    
    out <- mbind(
      setNames(SOM_stock[,,"total"][,,"soilc"],       "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)"),                    
      setNames(SOM_stock[,,"cropland"][,,"soilc"],    "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),   
      setNames(SOM_stock[,,"noncropland"][,,"soilc"], "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
    )
    
    SOM_chang <- SOM_stock - setYears(SOM_stock[,baseyear,],NULL)
    
    out <- mbind(out,
      setNames(SOM_chang[,,"total"][,,"soilc"],       paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm (Mt C wrt ",baseyear,")")),                    
      setNames(SOM_chang[,,"cropland"][,,"soilc"],    paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Cropland Soils (Mt C wrt ",baseyear,")")),   
      setNames(SOM_chang[,,"noncropland"][,,"soilc"], paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Noncropland Soils (Mt C wrt ",baseyear,")"))
    )

    out <- mbind(out,
      setNames(SOM_stock[,,"total"][,,"target_soilc"],       "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm (Mt C)"),                    
      setNames(SOM_stock[,,"cropland"][,,"target_soilc"],    "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),   
      setNames(SOM_stock[,,"noncropland"][,,"target_soilc"], "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
    )
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)

  } else if (datasource=="LPJmL_rev21"){
    
    litc   <- readSource("LPJml_rev21","litc",convert="onlycorrect")
    soilc  <- readSource("LPJml_rev21","soilc_layer",convert="onlycorrect")
    soilc2 <- soilc[,,"mm0_200"] + setNames(soilc[,,"mm201_500"],NULL)/3
    vegc   <- readSource("LPJml_rev21","vegc",convert="onlycorrect")
    
    out<-mbind(
      setNames(vegc[,getYears(soilc2),],"Resources|Carbon Stocks|Vegetation Carbon (Mt C)"),
      setNames(soilc2[,getYears(soilc2),],"Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)"),
      setNames(litc[,getYears(soilc2),],"Resources|Carbon Stocks|Litter Carbon (Mt C)")
    )
    
    area<-readSource("LUH2v2",subtype = "states",convert="onlycorrect")
    area<-setYears(dimSums(area[,2010,],dim=3),NULL)
    
    out<-out*area
    
    mapping<-toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    out<- toolAggregate(out,rel = mapping,from="celliso",to="iso",dim=1)
    
    out  <- toolCountryFill(out,fill=0)
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="climatescenarioX")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else if (datasource=="LPJmLCarbon"){
    
    soilc <- calcOutput("LPJmlCarbon", climatetype="historical", landtype="nat_veg", subtype="soilc_0-30", aggregate=FALSE)
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    stock <- soilc * area
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    stock   <- toolAggregate(stock,rel = mapping,from="celliso",to="iso",dim=1)
    stock   <- toolCountryFill(stock, fill=0)
    out     <- setNames(stock,"Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  
  } else if(datasource=="GSOC"){
    
    soilc <- readSource("GSOC",  convert="onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    stock <- soilc * area
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    stock   <- toolAggregate(stock,rel = mapping,from="celliso",to="iso",dim=1)
    stock   <- toolCountryFill(stock, fill=0)
    out     <- setNames(stock,"Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")
    
    out <- mbind(setYears(out, "y2015"), setYears(out, "y2016"), setYears(out, "y2017"))
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else if(datasource=="WISE"){
    
    soilc <- readSource("WISE",  convert="onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    stock <- soilc * area
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    stock   <- toolAggregate(stock,rel = mapping,from="celliso",to="iso",dim=1)
    stock   <- toolCountryFill(stock, fill=0)
    out     <- setNames(stock,"Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")
    
    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else if(datasource=="SoilGrids"){
    
    soilc <- readSource("SoilGrids", subtype="cstock_0_30", convert="onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    stock <- soilc * area
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    stock   <- toolAggregate(stock,rel = mapping,from="celliso",to="iso",dim=1)
    stock   <- toolCountryFill(stock, fill=0)
    out     <- setNames(stock,"Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)")
    
    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else stop("No data exist for the given datasource!")
  
  
  names(dimnames(out))[3] <- "scenario.model.variable"
  
  return(list(x=out,
              weight=NULL,
              unit="Mt C",
              description="Soil Carbon")
  )
}