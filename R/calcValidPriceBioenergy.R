#' calcValidPriceBioenergy
#' 
#' Returns future projections of biomass prices
#' 
#' 
#' @return list of magpie object with data and weight
#' @author Florian Humpenoeder
calcValidPriceBioenergy <- function() {
  sel <- c("Price|Primary Energy|Biomass (US$2005/GJ)")
  out <- calcOutput("ValidSSPResults", aggregate = FALSE)[,,sel]
  getNames(out,dim=3) <- c("Prices|Bioenergy (US$05/GJ)")
  
  # set aggregation weights based on population
  weight <- calcOutput(type = "Population", PopulationFuture="SSP",aggregate=FALSE)
  weight <- weight[,getYears(out),"pop_SSP2"]
  weight <- setNames(weight, NULL)
  
  return(list(x=out,
              weight=weight,
              unit="US$05/GJ",
              description="biomass prices from SSP database")
  )
}
