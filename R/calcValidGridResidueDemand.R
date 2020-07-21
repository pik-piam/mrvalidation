#' @title calcValidGridResidueDemand
#' @description reports aboveground Crop Residue Demand on 0.5 degree grid
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATtoLPJml}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridResidueDemand")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-

calcValidGridResidueDemand<-function() {
  
  out<-calcOutput("ResFieldBalancePast",cellular=TRUE,products="kres",aggregate=FALSE)
  out<-out[,,c("c","nr")]
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
  getNames(out,dim=2)<-reportingnames(getNames(out,dim=2))
  getNames(out,dim=3)<-reportingnames(getNames(out,dim=3))
  
  getComment(out)<-NULL
  
  return(list(x=out,
              weight=NULL,
              unit="Mt Carbon and Mt reactive Nitrogen",
              description="Aboveground Crop Residue Production and its Fate",
              isocountries=FALSE)
  ) 
}

