#' @title calcValidOverallCosts
#' @description Returns historical values of the overall value of production for the Agricultural, Forestry sectors, and fisheries (mio.05USD)
#' @param datasource datasource for validation. FAO Value of Production dataset.
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{ 
#' calcOutput("calcValidOverallCosts")
#' }
 
calcValidOverallCosts <- function(datasource="FAO") {
  
  #Value of Production for the agriculture, forestry and fisheries sector
  
  if(datasource == "FAO"){
    
  x<-dimSums(calcOutput("VoP_AFF",aggregate = FALSE),dim=c(3))/(1+0.04)^15 #Original costs in 2020 values. Conversion factor for USD 05.
  weight=NULL
  units="mio.05USD"
  
  }
  
  return(list(x=x,weight=weight,unit=units,description="Value of Production for Agriculture, Forestry and Fisheries"))
  
}

