#' @title calcValidCostsAEI
#' @description calculates the validation data for irrigation investment costs
#' 
#' @param datasource Datasource of validation data. 
#' @return list of magpie object with results on country level, weight on country level, unit and description.
#' @author Felicitas Beier
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidCostsAEI")
#' }
#' 

calcValidCostsAEI <- function(datasource="IMPACT") {
  
  if (datasource=="IMPACT") {
    
    out <- readSource("IMPACTIrrigInvCosts", convert=TRUE)
    out <- add_dimension(out, dim=3.2, add="variable", nm="Costs|AEI")
    
#NOTE Currently not in 05 USD!!! ---> needs to be added to convertIMPACTIrrigInvCosts.R
    getNames(out) <- paste(getNames(out), "(million US$05/yr)", sep=" ")
    unit          <- "million US$05/yr"
    
  } else { 
    stop("Only IMPACT irrigation investment costs available currently!")
  }
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="AEI Costs"))
}
