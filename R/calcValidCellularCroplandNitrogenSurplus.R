#' @title calcValidCellular
#' @description reports Cropland Nitrogen Surplus and a Balanceflow that balances for unrealistically high nitrogen use efficiencies on 0.5 degree grid
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATtoLPJml}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidCellularCroplandNitrogenSurplus")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-

calcValidCellularCroplandNitrogenSurplus <-function() {
  
  out<-calcOutput("NitrogenSurplusByCrop",cellular=TRUE,aggregate=FALSE)  
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
  getNames(out,dim=2)<-reportingnames(getNames(out,dim=2))
  
  getComment(out)<-NULL

  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Nitrogen Surplus on Croplands and a Balanceflow to balance unrealistic high nitrogen use efficiencies by extraterrestrial nitrogen.",
              isocountries=FALSE)
  ) 
}

