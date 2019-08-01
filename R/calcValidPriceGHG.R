#' @title calcValidPriceGHG
#' @description Validates MAgPIE GHG emission price input against SSP GHG emission price projections 
#' @param datasource Currently available: \code{"SSPResults"}
#' 
#' @author Amsalu W. Yalew, Benjamin Leon Bodirsky, Florian Humpenoeder 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidPriceGHG")
#' }
#' 
#' @importFrom magclass fulldim 

calcValidPriceGHG <- function(datasource = "SSPResults") {
  
  if(datasource == "SSPResults"){
    
    #read the file from SSPResults
    SSPR <- calcOutput("ValidSSPResults", aggregate = FALSE)
    
    # extract the carbon price
    CO2P <- SSPR[,,"Price|Carbon (US$2005/t CO2)"]
    getNames(CO2P,dim=3) <- "Prices|GHG Emission|CO2 (US$2005/tCO2)"
    
    
    # Calculate N20 price based on Carbon price (* by GWP factor 265)
    N2OP <- CO2P*265
    getNames(N2OP,dim=3) <- "Prices|GHG Emission|N2O (US$2005/tN2O)"
    
    
    # Calcualte CH4 price based on Carbon price (* by GWP factor 265)
    CH4P <- CO2P*28
    getNames(CH4P,dim=3) <- "Prices|GHG Emission|CH4 (US$2005/tCH4)"
  
    
    #merging 
    out  <- mbind(CO2P,N2OP,CH4P)
    
    #weights
    CO2weight <- setNames(readSource(type="EDGAR_LU", subtype = "CO2"), "Prices|GHG Emission|CO2 (US$2005/tCO2)")
    N2Oweight <- setNames(readSource(type="EDGAR_LU", subtype = "N2O"), "Prices|GHG Emission|N2O (US$2005/tN2O)")
    CH4weight <- setNames(readSource(type="EDGAR_LU", subtype=  "CH4"), "Prices|GHG Emission|CH4 (US$2005/tCH4)")
    edgar     <- mbind(CO2weight, N2Oweight, CH4weight)
    
    
    weight<-out
    weight[,,]<-10^-10
    #loop - for each model (of the SSPS) set the weights.This avoids the mismatch problem
    for (model in fulldim(out)[[2]][[4]]){
      weight[,,model]<-weight[,,model]+setYears(edgar[,"y2005",], NULL)
    }

    
  } else {stop("unknown data source")} 
  
  return(list(x=out,
              weight= weight,
              unit="US$ 2005 per ton CO2, N2O, CH4",
              description="CO2, N2O, and CH4 GHG emission price validation based on SSP results"))
  
}
