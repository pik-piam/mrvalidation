#' @title calcValidFoodExpenditureShare
#' @description validation for food expenditure share
#'
#' @param detail if FALSE, only major food commoditiy groups are shown.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidFoodExpenditureShare")
#' }
#' 
calcValidFoodExpenditureShare <- function(detail=FALSE) {
  
  expenditure<-calcOutput("ValidFoodExpenditure",aggregate = FALSE)
  gdp<-collapseNames(calcOutput("GDPpc", naming = "scenario", aggregate = FALSE)[,,"SSP2"])
  gdp<-gdp[,getYears(expenditure),]
  pop<-collapseNames(calcOutput("Population",aggregate = FALSE)[,,"pop_SSP2"])
  pop<-pop[,getYears(expenditure),]
  
  expenditure_shr=expenditure/gdp
  expenditure_shr[is.nan(expenditure_shr)]=0
  getNames(expenditure_shr)<-sub(pattern = "\\|Expenditure",replacement = "|Expenditure Share",x = getNames(expenditure_shr))

  return(list(x=expenditure_shr,
              weight=gdp*pop,
              unit="US$2017/US$2017",
              description="Share of expenditure for different food items"))
}