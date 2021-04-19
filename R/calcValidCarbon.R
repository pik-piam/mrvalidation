#' @title calcValidCarbon
#' @description calculates the validation data for carbon pools
#' 
#' @param datasource Datasources for validation data
#' 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' 
#' @examples
#' 
#' \dontrun{ 
#'   calcOutput("ValidCarbon")
#' }
#' 

calcValidCarbon <- function(datasource="LPJmL4_for_MAgPIE_84a69edd:GSWP3-W5E5:historical"){

  if(datasource=="LPJmL4_for_MAgPIE_84a69edd:GSWP3-W5E5:historical"){
    
    soilc <- calcOutput("LPJmL_new", version="LPJmL4_for_MAgPIE_84a69edd", climatetype="GSWP3-W5E5:historical", stage="raw", subtype="soilc", aggregate=FALSE)
    litc  <- calcOutput("LPJmL_new", version="LPJmL4_for_MAgPIE_84a69edd", climatetype="GSWP3-W5E5:historical", stage="raw", subtype="litc",  aggregate=FALSE)
    vegc  <- calcOutput("LPJmL_new", version="LPJmL4_for_MAgPIE_84a69edd", climatetype="GSWP3-W5E5:historical", stage="raw", subtype="vegc",  aggregate=FALSE)
    
    stock <- mbind(soilc, litc, vegc)
    rm(soilc, litc, vegc)
    
    area  <- dimSums(calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE), dim=3)
    stock <- stock * setYears(area[,"y2010",], NULL)
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    stock   <- toolAggregate(stock, rel = mapping,from="celliso",to="iso",dim=1)
    stock   <- toolCountryFill(stock, fill=0)
    
    out <- mbind(
      setNames(dimSums(stock, dim=3), "Resources|Carbon (Mt C)"),
      setNames(stock[,,"soilc"],      "Resources|Carbon|+|Soil (Mt C)"),                    
      setNames(stock[,,"litc"],       "Resources|Carbon|+|Litter (Mt C)"),   
      setNames(stock[,,"vegc"],       "Resources|Carbon|+|Vegetation (Mt C)")
    )
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else if(grepl("LPJmL4",datasource)&!grepl("GSWP3-W5E5",datasource)){
    
    version     <- gsub("^(.[^:]*):(.*)", "\\1", datasource)
    climatetype <- gsub("^(.[^:]*):(.*)", "\\2", datasource)
    
    soilc <- calcOutput("LPJmL_new", version=version, climatetype=climatetype, subtype="soilc", stage="raw", aggregate=FALSE)
    litc  <- calcOutput("LPJmL_new", version=version, climatetype=climatetype, subtype="litc",  stage="raw", aggregate=FALSE)
    vegc  <- calcOutput("LPJmL_new", version=version, climatetype=climatetype, subtype="vegc",  stage="raw", aggregate=FALSE)
    
    stock <- mbind(soilc, litc, vegc)
    rm(soilc, litc, vegc)
    
    area  <- dimSums(calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE), dim=3)
    stock <- stock * setYears(area[,"y2010",],NULL)
    
    mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
    stock   <- toolAggregate(stock, rel = mapping,from="celliso",to="iso",dim=1)
    stock   <- toolCountryFill(stock, fill=0)
    
    out <- mbind(
      setNames(dimSums(stock, dim=3), "Resources|Carbon (Mt C)"),
      setNames(stock[,,"soilc"],      "Resources|Carbon|+|Soil (Mt C)"),                    
      setNames(stock[,,"litc"],       "Resources|Carbon|+|Litter (Mt C)"),   
      setNames(stock[,,"vegc"],       "Resources|Carbon|+|Vegetation (Mt C)")
    )
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="projection")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  }
  
  else stop("No data exist for the given datasource!")
  
  return(list(x=out,
              weight=NULL,
              unit="Mt C",
              description="Carbon Stocks")
  )
}