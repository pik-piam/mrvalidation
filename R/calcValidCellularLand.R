#' @title calcValidCellularLand
#' @description reports the main Land types on 0.5 degree grid
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATtoLPJml}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidCellularLand")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-

calcValidCellularLand <-function() {
 
  out<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE)
  getNames(out,dim=1)<-reportingnames(getNames(out,dim=1))
 
  getComment(out)<-NULL
  
  return(list(x=out,
              weight=NULL,
              unit="Mha",
              description="Total land area in its primary land cover categories. Other includes non-forest natural vegetation like savannas.",
              isocountries=FALSE)
  ) 
}

