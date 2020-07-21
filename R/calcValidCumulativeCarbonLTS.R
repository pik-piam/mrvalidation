#' calcValidCumulativeCarbonLTS
#' 
#' Returns historical Emissions stored in wood products 
#' 
#' @param datasource Currently available \code{"Johnston_Radeloff"}
#' @return List of magpie object with Emissions in wood products
#' @author Abhijeet Mishra
#' @import magpiesets 
#' @importFrom magclass getNames collapseNames
#' @importFrom utils tail

calcValidCumulativeCarbonLTS <- function(datasource="Johnston_Radeloff") {
  
  if(datasource=="Johnston_Radeloff"){
    emis       <- readSource("CarbonLTS",subtype = datasource)
    
    cumulative <- collapseNames(emis[,,"Cumulative (GtCO2)"][,,"SSP2"])
    indicatorname="Emissions|CO2|Land|Cumulative|Land-use Change|Wood products|+|Storage"
    
    unit="Gt CO2"
    
    getNames(cumulative) <- paste0(indicatorname, getNames(cumulative)," (",unit,")")
    
    
    out <- cumulative - setYears(cumulative[,"y1995",],NULL)
    
    out <- out[,tail(getYears(out),10),,invert=TRUE]
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else if(datasource=="Johnston_Radeloff_P"){
    emis       <- readSource("CarbonLTS",subtype = "Johnston_Radeloff") ## original data also contains future projections
    
    cumulative <- collapseNames(emis[,,"Cumulative (GtCO2)"][,,"SSP2"])
    indicatorname="Emissions|CO2|Land|Cumulative|Land-use Change|Wood products|+|Storage"
    
    unit="Gt CO2"
    
    getNames(cumulative) <- paste0(indicatorname, getNames(cumulative)," (",unit,")")
    
    
    out <- cumulative - setYears(cumulative[,"y1995",],NULL)
    
    out <- out[,tail(getYears(out),10),]
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="projection")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else stop("No validation data exists from the given datasource!")
  
  return(list(x=out*-1,  ## Sequestartion is reported as negative
              weight=NULL,
              unit=unit,
              description="Cumulative sequestration from carbon stored in harvested wood.")
  )
}

