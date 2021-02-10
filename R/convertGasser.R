#' Convert Gasser Data
#' 
#' @title convertGasser
#' 
#' @description Convert subtypes on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing Gasser emissions data
#' @return Historical LUC emission data
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' 
#' @importFrom madrat toolCountryFill

convertGasser <- function(x){
  
  if("GLO" %in% getRegions(x)) {
    stop("Conversion routine not functional. Use convert=FALSE.")
  } else if(!("GLO" %in% getRegions(x))){
    y <- toolCountryFill(x,fill = 0)
    return(y)
    }
}