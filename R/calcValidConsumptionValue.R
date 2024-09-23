#' @title calcValidConsumptionValue
#' @description Validation for consumption Value
#' @param datasource datasource for validation (FAO)
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @importFrom magclass collapseNames
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' calcOutput("ValidConsumptionValue")
#' }

calcValidConsumptionValue<- function(datasource="FAO") {
  
  if (datasource == "FAO"){
    
    
    #Food and material demand
    kall<-findset("kall")
    food_mat <- collapseNames(
                 dimSums((
                  calcOutput("FAOmassbalance", aggregate = FALSE)[, , kall][
                                                         , , c("food","other_util")])[, , "dm"], 
                             dim=3.2))

    #Price consumers (World Prices)
    prices_kall_con<-setYears(calcOutput("IniFoodPrice",products = "kall",aggregate=FALSE),NULL)

    out<-dimSums(food_mat*prices_kall_con,dim=3)
    
    
    
  }else{ 
    stop("unknown datasource")}
  
  getNames(out) <- "Value|Consumption Value (million US$2017/yr)"
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  
  return(list(x=out,
              weight=NULL,
              unit="million US$17/yr",
              description="Consumption Value"))
}