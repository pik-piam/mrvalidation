#' @title calcValidFoodExpenditure
#' @description validation for foode expenditure
#'
#' @param detail if FALSE, only major food commoditiy groups are shown.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidFoodExpenditure")
#' }
#' @importFrom  magpiesets findset
calcValidFoodExpenditure <- function(detail=FALSE) {
  
  price<-calcOutput("IniFoodPrice", datasource="FAO",aggregate = FALSE)
  pop<-collapseNames(calcOutput("Population",naming="indicator.scenario",aggregate = FALSE)[,,"SSP2"])
  
  demand<-dimSums(calcOutput("FAOmassbalance",aggregate = FALSE)[,,c("food","flour1")][,,"dm"],dim=c(3.2,3.3))
  demand<-demand[,,getNames(price)]
  pop<-pop[,getYears(demand),]
  demand_pc<-demand/pop[,getYears(demand),]
  
  
  out<-demand_pc*price
  out2<-reporthelper(x=out,level_zero_name = "Household Expenditure|Food|Expenditure",detail = detail,partly=TRUE)  
  out2[is.nan(out2)]=0
  
  return(list(x=out2,
              weight=pop,
              unit="USD/capita",
              description="Per-capita expenditure for food"))
}