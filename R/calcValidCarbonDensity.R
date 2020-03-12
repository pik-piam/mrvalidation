#' @title calcValidCarbonDensity
#' @description calculates the validation data for the carbon densities (including weights for aggregation)
#' 
#' @param datasource Datasources for validation data, e.g. LPJ_IPCC2006, LPJmL_natural
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{calcSOM}}
#' \code{\link{calcValidCarbonStocks}}
#' @examples
#' 
#' \dontrun{ 
#'   calcOutput("ValidCarbonDensity")
#' }
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass mbind getYears setYears nregions getCells

calcValidCarbonDensity <- function(datasource="LPJ_IPCC2006"){
  
  if(datasource=="LPJ_IPCC2006"){
    
    SOM_dens  <- calcOutput("SOM", subtype="density", aggregate = FALSE, supplementary = TRUE)
    SOM_area  <- Area_grid <- SOM_dens$weight
    SOM_dens  <- SOM_dens$x
    
    soilc     <- calcOutput("LPJmlCarbon", landtype="nat_veg", subtype="soilc_0-30", selectyears="past_all", aggregate=FALSE)
    getCells(soilc) <- getCells(SOM_area)
    mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)
    
    SOM_area  <- toolAggregate(SOM_area, rel=mapping, from=ifelse(nregions(SOM_area)>1,"celliso","cell"), to="iso", dim=1)
    SOM_area  <- toolCountryFill(SOM_area,fill=0)
    SOM_area  <- mbind(SOM_area, setNames(dimSums(SOM_area, dim=3), "total"))
    
    SOM_dens  <- toolAggregate(SOM_dens, weight=Area_grid, rel=mapping, from=ifelse(nregions(SOM_dens)>1,"celliso","cell"), to="iso", dim=1)
    SOM_dens  <- toolCountryFill(SOM_dens,fill=0)
    SOM_dens  <- mbind(SOM_dens, add_dimension(dimSums(SOM_dens*SOM_area[,,"total", invert=TRUE], dim=3.1)/dimSums(SOM_area[,,"total", invert=TRUE], dim=3), add="landuse", nm="total"))
    
    soilc      <- mbind(setNames(soilc,"cropland"), setNames(soilc,"noncropland"), setNames(soilc,"total"))
    Area_grid  <- mbind(Area_grid, setNames(dimSums(Area_grid, dim=3),"total"))
    soilc      <- toolAggregate(soilc, rel=mapping, weight=Area_grid, from=ifelse(nregions(soilc)>1,"celliso","cell"),  to="iso", dim=1)
    soilc      <- toolCountryFill(soilc,fill=0)
    
    cshare     <- SOM_dens/soilc
    
    out <- mbind(setNames(SOM_dens[,,"total"][,,"soilc"],       "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)"),                    
                 setNames(SOM_dens[,,"cropland"][,,"soilc"],    "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|Cropland Soils (tC/ha)"),   
                 setNames(SOM_dens[,,"noncropland"][,,"soilc"], "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|Noncropland Soils (tC/ha)")
    )
    
    out <- mbind(out,
                 setNames(SOM_dens[,,"total"][,,"target_soilc"],       "Resources|Soil Carbon|Target|Density|SOC in top 30 cm (tC/ha)"),                    
                 setNames(SOM_dens[,,"cropland"][,,"target_soilc"],    "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|+|Cropland Soils (tC/ha)"),   
                 setNames(SOM_dens[,,"noncropland"][,,"target_soilc"], "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|+|Noncropland Soils (tC/ha)")
    ) 

    out <- mbind(out,
                setNames(cshare[,,"total"][,,"soilc"],       "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm (tC/tC)"),                    
                setNames(cshare[,,"cropland"][,,"soilc"],    "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|+|Cropland Soils (tC/tC)"),   
                setNames(cshare[,,"noncropland"][,,"soilc"], "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|+|Noncropland Soils (tC/tC)")
    )
    
    
    out <- mbind(out,
                 setNames(cshare[,,"total"][,,"target_soilc"],       "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm (tC/tC)"),                    
                 setNames(cshare[,,"cropland"][,,"target_soilc"],    "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|+|Cropland Soils (tC/tC)"),   
                 setNames(cshare[,,"noncropland"][,,"target_soilc"], "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|+|Noncropland Soils (tC/tC)")
    )
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    weight    <- out
    weight[]  <- mbind(SOM_area[,,c("total","cropland","noncropland")],
                      SOM_area[,,c("total","cropland","noncropland")],
                      SOM_area[,,c("total","cropland","noncropland")],
                      SOM_area[,,c("total","cropland","noncropland")])
    
    out[is.na(out)==TRUE] <- 0
    
  } else if (datasource=="LPJmL_rev21"){
    
    soilc  <- readSource("LPJml_rev21","soilc_layer",convert="onlycorrect")
    soilc  <- soilc[,,"mm0_200"] + setNames(soilc[,,"mm201_500"],NULL)/3
    
    area   <- readSource("LUH2v2",subtype = "states",convert="onlycorrect")
    area   <- setYears(dimSums(area[,2010,],dim=3),NULL)
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    soilc   <- toolAggregate(soilc, weight=area, rel=mapping, from="celliso",to="iso",dim=1)
    soilc   <- toolCountryFill(soilc, fill=0)
    out     <- setNames(soilc,"Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="climatescenarioX")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    area   <- toolAggregate(area, rel=mapping, from="celliso",to="iso",dim=1)
    area   <- toolCountryFill(area, fill=0)
    
    weight   <- out
    weight[] <- area
    
  }  else if (datasource=="LPJmLCarbon"){
    
    soilc <- calcOutput("LPJmlCarbon", climatetype="historical", landtype="nat_veg", subtype="soilc_0-30", aggregate=FALSE)
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    soilc   <- toolAggregate(soilc, weight=area, rel=mapping, from="celliso",to="iso",dim=1)
    soilc   <- toolCountryFill(soilc, fill=0)
    out     <- setNames(soilc,"Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    area   <- toolAggregate(area, rel=mapping, from="celliso",to="iso",dim=1)
    area   <- toolCountryFill(area, fill=0)
    
    weight   <- out
    weight[] <- area
    
  } else if(datasource=="GSOC"){
    
    soilc <- readSource("GSOC",  convert="onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    soilc   <- toolAggregate(soilc, weight=area, rel=mapping, from="celliso",to="iso",dim=1)
    soilc   <- toolCountryFill(soilc, fill=0)
    out     <- setNames(soilc,"Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")
    
    out <- mbind(setYears(out, "y2015"), setYears(out, "y2016"), setYears(out, "y2017"))
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    area   <- toolAggregate(area, rel=mapping, from="celliso",to="iso",dim=1)
    area   <- toolCountryFill(area, fill=0)
    
    weight   <- out
    weight[] <- area
    
  } else if(datasource=="WISE"){
    
    soilc <- readSource("WISE",  convert="onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    soilc   <- toolAggregate(soilc, weight=area, rel=mapping, from="celliso",to="iso",dim=1)
    soilc   <- toolCountryFill(soilc, fill=0)
    out     <- setNames(soilc,"Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")
    
    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    area   <- toolAggregate(area, rel=mapping, from="celliso",to="iso",dim=1)
    area   <- toolCountryFill(area, fill=0)
    
    weight   <- out
    weight[] <- area
    
    
  } else if(datasource=="SoilGrids"){
    
    soilc <- readSource("SoilGrids", subtype="cstock_0_30", convert="onlycorrect")
    area  <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE)
    area  <- setYears(dimSums(area[,2010,],dim=3),NULL)
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    soilc   <- toolAggregate(soilc, weight=area, rel=mapping, from="celliso",to="iso",dim=1)
    soilc   <- toolCountryFill(soilc, fill=0)
    out     <- setNames(soilc,"Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)")
    
    out <- mbind(setYears(out, "y1995"), setYears(out, "y2000"), setYears(out, "y2005"), setYears(out, "y2010"))
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    area   <- toolAggregate(area, rel=mapping, from="celliso",to="iso",dim=1)
    area   <- toolCountryFill(area, fill=0)
    
    weight   <- out
    weight[] <- area
    
  } else stop("No data exist for the given datasource!")
  
  names(dimnames(out))[3]    <- "scenario.model.variable"
  names(dimnames(weight))[3] <- "scenario.model.variable"
  
  return(list(x=out,
              weight=weight,
              unit="t C per ha",
              description="Soil Carbon")
  )
}