#' @title calcValidNitrogenBudgetPasture
#' @description Validation Script for Nitrogen Budgets on Pastures
#'
#' @param datasource Bodirsky for own calculations, Lassaletta2014 for a country dataset from 
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014. 
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems: The Relationship between Yield and Nitrogen Input to Pasture.
#' Environmental Research Letters.
#' FAO for some N related parameters published in FAOSTAT.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidSNUpE}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidNitrogenBudgetPasture")
#' }
#' 

#' @importFrom magpiesets reportingnames
calcValidNitrogenBudgetPasture<-function(datasource="Bodirsky"){
  
  if(datasource=="Bodirsky"){
    budget<-calcOutput("NitrogenBudgetPasture",aggregate = FALSE)
    #budget[,,"som"] = -budget[,,"som"]
    
    all<-getNames(budget)
    withdrawaltypes<-c("harvest")
    balancetypes<-c("surplus","balanceflow")
    inputtypes<-setdiff(setdiff(all,withdrawaltypes),balancetypes)
    
    tmp<-budget[,,inputtypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Pasture Budget|Inputs|+|",reportingnames(getNames(tmp)))
    inputs<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Pasture Budget|Inputs"),
      tmp
    )
    
    tmp<-budget[,,withdrawaltypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Pasture Budget|Withdrawals|+|",reportingnames(getNames(tmp)))
    withdrawals<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Pasture Budget|Withdrawals"),
      tmp
    )
    
    tmp<-budget[,,balancetypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Pasture Budget|Balance|+|",reportingnames(getNames(tmp)))
    balance<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Pasture Budget|Balance"),
      tmp
    )
    
    out<-mbind(
      inputs,
      withdrawals,
      balance
    )
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  }else {stop("No data exist for the given datasource!")}
  out <- add_dimension(out, dim=3.1, add="model", nm=datasource)
  unit="Mt Nr/yr"
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Pasture Nitrogen Budget")
         )
}