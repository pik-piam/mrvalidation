#' calcValidGrowingStock
#' 
#' Returns historical FRA 2015 growing stock in mio m3 per ha 
#' 
#' @param datasource Currently only  available for the "FAO" source
#' @return List of magpie object with growing stock
#' @author Abhijeet Mishra
#' @import magpiesets 
#' @importFrom magclass getNames
#' 
calcValidGrowingStock <- function(datasource="FAO") {
  
  if(datasource=="FAO"){
    gs <- readSource("FRA2015Doc","forest_gs")
    area <- setNames(readSource("FRA2015Doc","forest_area"),NULL)

    a <- setNames(gs/area,NULL)
    a[is.nan(a)] <- 0
    
    indicatorname="Resources|Growing Stock|+|Forest"
    unit="m3/ha"
    out <- a
    getNames(out) <- paste0(indicatorname, getNames(out)," (",unit,")")
    
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    weight = area
    
  } else stop("No validation data exists for the given datasource!")
  
  return(list(x=out,
              weight=weight,
              unit=unit,
              description="Growing stock from FAO data")
  )
}

