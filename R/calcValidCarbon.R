#' @title calcValidCarbon
#' @description calculates the validation data for carbon pools
#' 
#' @param datasource Datasources for validation data, e.g. LPJmL4:CRU_4
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

calcValidCarbon <- function(datasource="LPJmL4:CRU_4"){

  if(datasource=="LPJmL4:CRU_4"){
    
    soilc <- calcOutput("LPJmL", version="LPJmL4", climatetype="CRU_4", subtype="soilc", aggregate=FALSE)
    litc  <- calcOutput("LPJmL", version="LPJmL4", climatetype="CRU_4", subtype="litc",  aggregate=FALSE)
    vegc  <- calcOutput("LPJmL", version="LPJmL4", climatetype="CRU_4", subtype="vegc",  aggregate=FALSE)
    
    stock <- mbind(soilc, litc, vegc)[,findset("past_all"),]
    rm(soilc, litc, vegc)
    
    area  <- dimSums(calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=FALSE, cellular=TRUE, selectyears="past_all", aggregate = FALSE), dim=3)
    stock <- stock * area
    
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
    
  } else if(grepl("LPJmL4",datasource)&!grepl("CRU_4",datasource)){
    
    if(grepl("raw",datasource)){
      
      time  <- "raw"
      dof   <- NULL 
      harmonize_baseline <- NULL
      ref_year <- NULL
      
    } else {
      time  <- "spline"
      dof   <- 4 
      harmonize_baseline <- "CRU_4"
      ref_year <- "y2015"
      
    }

    version     <- gsub("^(.[^:]*):(.*)", "\\1", gsub("raw","", datasource))
    climatetype <- gsub("^(.[^:]*):(.*)", "\\2", gsub("raw","", datasource))
    
    soilc <- calcOutput("LPJmL", version=version, climatetype=climatetype, subtype="soilc",     
                        time=time, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year, aggregate=FALSE)
    litc  <- calcOutput("LPJmL", version=version, climatetype=climatetype, subtype="litc",     
                        time=time, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year, aggregate=FALSE)
    vegc  <- calcOutput("LPJmL", version=version, climatetype=climatetype, subtype="vegc",  aggregate=FALSE)
    
    stock <- mbind(soilc, litc, vegc)[,findset("past_all"),]
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