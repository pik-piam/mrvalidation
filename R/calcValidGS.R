#' calcValidGS
#' 
#' Returns historical FRA 2020 growing stock in million m3 
#' 
#' @param datasource Currently only  available for the "FRA" source
#' @return List of magpie object with growing stock
#' @author Abhijeet Mishra
#' @import magpiesets 
#' @importFrom magclass getNames
#' 
calcValidGS <- function(datasource="FRA") {
  
  if(datasource=="FRA"){
    a <- collapseNames(readSource("FRA2020",subtype = "growing_stock",convert = TRUE)) * 1000 ## Convert from Billion m3 to mio. m3
    
    indicatorname="Resources|Growing Stock|+|"
    unit="mio. m3"
    out <- a
    getNames(out) <- paste0(indicatorname, getNames(out)," (",unit,")")
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    weight = NULL
    
  } else stop("No validation data exists for the given datasource!")
  
  return(list(x=out,
              weight=weight,
              unit=unit,
              description="Growing stock (standing biomass above ground) from FRA data")
  )
}

