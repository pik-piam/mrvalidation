#' calcValidSDG1
#' 
#' Returns historical SDG1 INDICATOR_POVERTY  IN USD05/cap/yr 
#' 
#' @param datasource Currently only  available for the "James" source
#' @return List of magpie object with data and population
#' @author Edna J. Molina Bacca
#' @import magpiesets 
#' @importFrom magclass getNames
#' 
calcValidSDG1 <- function(datasource="James") {
  
  if(datasource=="James"){
    a <- readSource(type="James2019",subtype="IHME_USD05_PPP_pc")
    pop_weights  <- readSource("WDI",subtype = "SP.POP.TOTL") + 10^-10
    com_years<-intersect(getYears(a),getYears(pop_weights))
    
    a<-a[,com_years,]
    pop_weights<-pop_weights[,com_years,]
    
    indicatorname="SDG|SDG01|Per-capita income"
    unit="USD05/cap/yr"
    out <- a
    getNames(out) <- paste0(indicatorname, " (",unit,")")
    

    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    
  } else stop("No data exist for the given datasource!")
  
  return(list(x=out,
              weight=pop_weights,
              unit="USD05/cap/yr",
              description="")
  )
}

