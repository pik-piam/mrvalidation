#' @title calcValidCostsTransport
#' @description calculates the validation data for transport costs 
#' 
#' @param datasource Datasource of validation data. 
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David Chen
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidCostsTransport")
#' }
#' 
#' @import mrmagpie

#' @importFrom magpiesets reporthelper summationhelper
calcValidCostsTransport<-function(datasource="GTAP"){
  
  if(datasource=="GTAP"){

  distance <- readSource("TransportDistance",convert="onlycorrect")
  production_kcr <- calcOutput("Production",cellular=TRUE, products="kcr", attributes="dm",aggregate=F)
  production_kli <- calcOutput("Production",cellular=TRUE, products="kli", attributes="dm",aggregate=F)
  production_pasture <- calcOutput("Production",cellular=TRUE, products="pasture", attributes="dm",aggregate=F)
  production_pasture <- add_dimension(production_pasture, add="pasture",nm="pasture",dim=3.1)
  production <- mbind(production_kcr, production_kli,production_pasture)
  
  production_distance <- collapseNames(distance*production, collapsedim=2)
  

  #costs per unit per distance
  costs <- readSource("TransportCostsGTAP", convert=FALSE)
  
  products <- intersect(getNames(production_distance), getNames(costs))
  
  out <- costs[,,products]*production_distance[,,products] 
  
  mapping <- toolGetMapping(name="CountryToCellMapping.csv",type="cell")
  out   <- toolAggregate(out, rel = mapping,from="celliso",to="iso",dim=1)
  out   <- toolCountryFill(out, fill=0)
  
  
#add missing product groups, so that report and summation helper work properly    
 missing_products <- setdiff(findset("kcr"), products)  
  out <- add_columns(out, addnm=missing_products, dim=3.1)
  out[,,missing_products] <- 0
  
  #delete Mainsolve also in magpie4 costs
 out <- reporthelper(out, dim=3.1, level_zero_name = "Costs|Transport", detail=FALSE)
 out <- summationhelper(out)
 getNames(out) <- paste(getNames(out), "(million US$05/yr)", sep=" ")
 unit = "million US$05/yr"
 
 out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
 out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
 
  }
  else{ stop("Only GTAP transport costs avilable currently!")}
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Transport Costs")
  )
}
