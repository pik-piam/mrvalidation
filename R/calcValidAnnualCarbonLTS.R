#' calcValidAnnualCarbonLTS
#' 
#' Returns historical Emissions stored in wood products 
#' 
#' @param datasource Currently available \code{"Lauk_et_al"} or \code{"Johnston_Radeloff"} or \code{"Johnston_Radeloff_P"} (p = projection)
#' @return List of magpie object with Emissions in wood products
#' @author Abhijeet Mishra
#' @import magpiesets 
#' @importFrom magclass getNames collapseNames

calcValidAnnualCarbonLTS <- function(datasource="Lauk_et_al") {
  
  if(datasource=="Lauk_et_al"){
    emis <- collapseNames(readSource("CarbonLTS",subtype = datasource))
    
    indicatorname="Emissions|CO2|Land|Land-use Change|Wood products|+|Storage"
    unit="Mt CO2/yr"
    out <- emis
    getNames(out) <- paste0(indicatorname, getNames(out)," (",unit,")")
    
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else if(datasource=="Johnston_Radeloff"){
    emis       <- readSource("CarbonLTS",subtype = datasource)
    
    annual     <- collapseNames(emis[,,"Annual (MtCO2/yr)"][,,"SSP2"])
    indicatorname="Emissions|CO2|Land|Land-use Change|Wood products|+|Storage"
    unit="Mt CO2/yr"
    getNames(annual) <- paste0(indicatorname, getNames(annual)," (",unit,")")
    
    future <- tail(getYears(annual),10)
    
    out_historical <- annual[,future,,invert=TRUE]

    out_historical <- add_dimension(out_historical, dim=3.1, add="scenario", nm="historical")
    out_historical <- add_dimension(out_historical, dim=3.2, add="model", nm=datasource)
    
    out <- out_historical
    

  } else if(datasource=="Johnston_Radeloff_P"){
    emis       <- readSource("CarbonLTS",subtype = "Johnston_Radeloff") ## original data also contains future projections
    
    annual     <- collapseNames(emis[,,"Annual (MtCO2/yr)"][,,"SSP2"])
    indicatorname="Emissions|CO2|Land|Land-use Change|Wood products|+|Storage"
    unit="Mt CO2/yr"
    getNames(annual) <- paste0(indicatorname, getNames(annual)," (",unit,")")
    
    future <- tail(getYears(annual),10)
    
    out_projection <- annual[,future,]
    
    out_projection <- add_dimension(out_projection, dim=3.1, add="scenario", nm="projection")
    out_projection <- add_dimension(out_projection, dim=3.2, add="model", nm=datasource)
    
    out <- out_projection
    
    } else stop("No validation data exists from the given datasource!")
  
  return(list(x=out*-1, ## Sequestartion is reported as negative
              weight=NULL,
              unit=unit,
              description="Annual sequestration from carbon stored in harvested wood.")
  )
}

