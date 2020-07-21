#' @title calcValidGridYields
#' @description reports Yields on 0.5 degree grid
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATtoLPJml}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridYields")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-

calcValidGridYields <-function() {
  
  out<-calcOutput("Yield",cellular=TRUE,aggregate=FALSE,irrigation=TRUE)
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
  getNames(out,dim=2)<-reportingnames(getNames(out,dim=2))
  out<-clean_magpie(out)
  out<-dimOrder(out,perm = c(2,1))
  
  getComment(out)<-NULL
  
  return(list(x=out,
              weight=NULL,
              unit="t DM per ha physical area",
              description="Crop yields by plant type and irrigation",
              isocountries=FALSE)
  ) 
}

