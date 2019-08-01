#' @title calcValidCellularCroplandNitrogenInputs
#' @description reports Nitrogen Inputs by crop type on 0.5 degree grid
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATtoLPJml}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidCellularCroplandNitrogenInputs")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-

calcValidCellularCroplandNitrogenInputs <-function() {
 
  out<-calcOutput("FertilizerByCrop", cellular=TRUE, aggregate=FALSE)
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
  getNames(out,dim=2)<-reportingnames(getNames(out,dim=2))
  
  getComment(out)<-NULL
 
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr/yr",
              description="Nitrogen inputs from different sources. Note that biological fixation within plants (e.g. by legumes) is accounted for as a reduction in withdrawal. Fixation here refers to freeliving nitrogen fixers outside of the farmed plants.",
              isocountries=FALSE)
  ) 
}

