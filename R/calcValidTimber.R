#' calcValidTimber
#' 
#' Returns historical timber demand in mio m3 per yr 
#' 
#' @param datasource Currently only  available for the "FAO" source
#' @return List of magpie object with data and population
#' @author Abhijeet Mishra
#' @import magpiesets 
#' @importFrom magclass getNames
#' 
calcValidTimber <- function(datasource="FAO") {
  
  if(datasource=="FAO"){
    dem <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,,"domestic_supply"])
    prod <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,,"production"])

    indicatorname="Timber|Volumetric|Demand|"
    unit="Mm3/yr"
    getNames(dem) <- paste0(indicatorname, getNames(dem)," (",unit,")")
    
    indicatorname="Timber|Volumetric|Production|"
    unit="Mm3/yr"
    getNames(prod) <- paste0(indicatorname, getNames(prod)," (",unit,")")
    
    out <- mbind(dem,prod)
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    
  } else stop("No data exist for the given datasource!")
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Timber demand from FAO data")
  )
}

