#' @title calcValidGridCroplandNitrogenWithdrawals
#' @description reports Cropland Nitrogen Withdrawals from soils on 0.5 degree grid
#' @param irrigation FALSE for the sum of irrigated and rainfed, FALSE for seperated categories, 'rainfed' or 'irrigated for single categories
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATTOLPJML}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridCroplandNitrogenWithdrawals")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-


calcValidGridCroplandNitrogenWithdrawals <-function(irrigation=FALSE) {
 
  out<-calcOutput("NitrogenWithdrawalByCrop",cellular=TRUE,irrigation=irrigation,aggregate=FALSE)
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
  getNames(out,dim=2)<-reportingnames(getNames(out,dim=2))
  if(irrigation!=FALSE){
    getNames(out,dim=3)<-reportingnames(getNames(out,dim=3))   
    out<-dimOrder(out,perm = c(3,1,2))
  }
  
  getComment(out)<-NULL
 
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Nitrogen Withdrawals from Cropland Soils. Withdrawals are the sum of harvested crop (storage organs), aboveground crop residues and belowground crop residues minus the nitrogen that is fixed within the plant and minus the nitrogen incorporated in the seed",
              isocountries=FALSE)
  ) 
}

