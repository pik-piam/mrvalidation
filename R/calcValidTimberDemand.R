#' calcValidTimberDemand
#' 
#' Returns historical timber demand in mio m3 per yr 
#' 
#' @param datasource Currently only  available for the "FAO" source
#' @return List of magpie object with data and population
#' @author Abhijeet Mishra
#' @import magpiesets 
#' @importFrom magclass getNames
#' 
calcValidTimberDemand <- function(datasource="FAO") {
  
  if(datasource=="FAO"){
    a <- collapseNames(calcOutput("TimberDemand",aggregate = F)[,,"domestic_supply"]) 

    indicatorname="Timber demand|"
    unit="mio m3/yr"
    out <- a
    getNames(out) <- paste0(indicatorname, getNames(out)," (",unit,")")
    
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    
  } else stop("No data exist for the given datasource!")
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Timber demand from FAO data")
  )
}

