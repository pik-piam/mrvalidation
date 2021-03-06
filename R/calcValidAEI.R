#' @title calcValidAEI
#' 
#' @description Returns historical area equipped for irrigation.
#' 
#' @param datasource Currently available: \code{"LUH2v2"}, \code{"HID"} and \code{"GMIA"} 
#' @return list of magpie object with data and weight
#' @author Stephen Wirth, Anne Biewald
#' @importFrom magpiesets reportingnames
#' @importFrom magclass collapseNames
calcValidAEI <- function(datasource="LUH2v2"){

  if(datasource=="LUH2v2"){
   out <- calcOutput("AreaEquippedForIrrigation", aggregate = FALSE)
  } else if(datasource=="HID"){
    out <- readSource("HID", "national_1900_2005")/10^6
  } else if(datasource=="GMIA"){
    out <- calcOutput("GMIA", aggregate = FALSE)[,,"AEI_ha_"]/10^6
  } else {
    stop("Given datasource currently not supported!")
  }
  
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  out <- add_dimension(out, dim=3.3, add="variable", nm="Resources|Land Cover|Cropland|Area equipped for irrigation (million ha)")
  out <- collapseNames(out,collapsedim = 3.4)

  
  return(list(x=out,
              weight=NULL,
              unit="million ha",
              min=0,
              description="Area equipped for Irrigation in Mha")
  )
}
