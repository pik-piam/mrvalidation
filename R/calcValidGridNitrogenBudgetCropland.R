#' @title calcValidGridNitrogenBudgetCropland
#' @description reports Nitrogen Budget for Croplands on 0.5 degree grid
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATTOLPJML}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridNitrogenBudgetCropland")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-

calcValidGridNitrogenBudgetCropland <-function() {
 
  out<-calcOutput("NitrogenBudgetCropland",cellular=TRUE,aggregate=FALSE)
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
  
  getComment(out)<-NULL
  
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Nitrogen Budget for Croplands",
              isocountries=FALSE)
  ) 
}

