#' @title calcValidPriceAgriculture
#'
#' @description provides global prices from the IMPACT model projections, World Bank observations, and FAO 
#' obersvations for MAgPIE commodities in $2017/tDM
#' 
#' @param datasource Options of the source of data:  \code{IMPACT3.2.2World_Price}, \code{FAO}, \code{FAOp} and \code{WBGEM}. 
#'
#' @return List with a magpie object with commodity prices on global and country level.
#' @author Mishko Stevanovic
#' @seealso
#' \code{\link{readIMPACT3.2.2World_Price}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidPriceAgriculture", datasource="IMPACT3.2.2World_Price", aggregate=FALSE)
#' calcOutput("ValidPriceAgriculture", datasource="FAO")
#' }
#' 
#' @importFrom magpiesets findset reporthelper


calcValidPriceAgriculture <- function(datasource="FAO"){
  
  if(datasource=="IniFoodPrice") {
    out <- calcOutput("IniFoodPrice", datasource="FAO", aggregate=FALSE, supplementary=TRUE, products="kall")
    out$x<-add_dimension(add_dimension(out$x,dim = 3.1,add = "model",nm = "IniFoodPrice"),dim = 3.1,add = "scenario",nm = "historical")
    getYears(out$x) <- 2005
  } else {
    out <- calcOutput("PriceAgriculture", datasource=datasource, aggregate=FALSE, supplementary=TRUE)
  }
  # unlisting the returned object
  x            <- out$x
  weight       <- out$weight
  unit         <- out$unit
  description  <- out$description
  isocountries <- out$isocountries
  
  
  # assign the reporting names to magpie food commodities
  
  xn  <- sub("^.*\\.([^\\.]*$)","\\1",getNames(x))
  scmo <- sub("(^.*\\.)[^\\.]*$","\\1",getNames(x))
  rpl <- paste0("Prices|Agriculture|",reportingnames(xn))
  getNames(x) <- paste0(scmo,rpl)
  
  if(!is.null(weight)){
    dimnames(weight)[[3]] <- paste0("Prices|Agriculture|",reportingnames(dimnames(weight)[[3]]))
    getNames(weight) <- paste0(getNames(weight)," (US$2017/tDM)")
  }
  
  getNames(x) <- paste0(getNames(x)," (US$2017/tDM)")

  
  return(list(x=x,
              weight=weight,
              unit="US$2017/tDM",
              description=description,
              isocountries=isocountries))
}