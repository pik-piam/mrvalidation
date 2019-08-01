#' calcValidDemandBioenergy
#' 
#' Returns future projections of bioenergy demand
#' 
#' 
#' @return list of magpie object with data and weight
#' @author Florian Humpenoeder
calcValidDemandBioenergy <- function() {
  sel <- c("Primary Energy|Biomass|1st Generation (EJ/yr)","Primary Energy|Biomass|Energy Crops (EJ/yr)")
  out <- calcOutput("ValidSSPResults", aggregate = FALSE)[,,sel]
  getNames(out,dim=3) <- c("Demand|Bioenergy|++|1st generation (EJ/yr)","Demand|Bioenergy|++|2nd generation (EJ/yr)")
  y <- dimSums(out, dim = 3.3)
  y <- add_dimension(y,dim=3.3, add="variable", nm = "Demand|Bioenergy (EJ/yr)")
  out <- mbind(out, y)
  return(list(x=out,
              weight=NULL,
              unit="EJ/yr",
              description="1st and 2nd gen bioenergy demand from SSP database")
  )
}
