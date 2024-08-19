#' @title calcValidCostsCapStocks
#' @description Returns historical values for capital related costs
#' @param datasource datasource for validation. FAO Value of Production dataset.
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @importFrom magpiesets reporthelper summationhelper
#' @examples
#' \dontrun{
#' calcOutput("calcValidCostsCapStocks")
#' }

calcValidCostsCapStocks <- function(datasource="FAO") {
  
  Factor_requirements<- if (datasource == "FAO") calcOutput("FactorIntensity",output="requirements", method = "CapitalStock", aggregate= FALSE) else if(datasource == "USDA") calcOutput("FactorIntensity",output="requirements", method = "USDA",aggregate= FALSE)
  Production<- collapseNames(calcOutput("Production",products="kcr",aggregate=FALSE, attributes="dm"))
  names<-intersect(getNames(Factor_requirements),getNames(Production))
  years<- intersect(getYears(Factor_requirements),getYears(Production))
    
  CapitalStocks<-dimSums(setYears(Factor_requirements[,2005,names],NULL)*Production[,years,names])

  
  getNames(CapitalStocks)<-"Costs|Capital Stocks"
  getNames(CapitalStocks) <- paste0(getNames(CapitalStocks)," (million US$2017)")
  
  units = "million US$2017/yr"
  
  
  out<- add_dimension(CapitalStocks, dim=3.1, add="scenario", nm="historical")
  out<- add_dimension(out, dim=3.2, add="model", nm=datasource)
  
  weight<- NULL  
   
  
  return(list(x=out,weight=weight,
              unit=units,
              description="Validation for capital stocks (million US$2017/yr)"))
  
}
