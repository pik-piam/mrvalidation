#' @title calcValidGridNitrogenBudgetNonagland
#' @description reports Nitrogen Budget for non-agricultural land types on 0.5 degree grid
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATTOLPJML}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridNitrogenBudgetNonagland")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-

calcValidGridNitrogenBudgetNonagland <-function() {
 
  out<-calcOutput("NitrogenBudgetNonagland",cellular=TRUE,aggregate=FALSE)
  out<-dimSums(out,dim=3.1)
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
  
  getComment(out)<-NULL
  
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Nitrogen Budget for Non-agricutlural land",
              isocountries=FALSE)
  ) 
}

