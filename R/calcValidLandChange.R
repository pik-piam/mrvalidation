#' @title calcValidLandChange
#' 
#' @description Returns historical changes of cropland, pasture and forest area from LUH2 and FAOSTAT that can
#' be used for model validation.
#' 
#' @param baseyear baseyear for calculating land-use change
#' @param datasource Currently available: \code{"FAO"}, \code{"LUH2v2"}, \code{"MAgPIEown"} and \code{"SSPResults"}
#' @return list of magpie object with data and weight
#' @author Florian Humpenoeder
#' @importFrom utils read.csv
#' @importFrom magpiesets reportingnames 
#' 
#' 
calcValidLandChange <- function(baseyear=1995,datasource="MAgPIEown"){
  
  #get historic LandUse
  x <- calcOutput(type="ValidLand", datasource=datasource, aggregate=FALSE)
  y <- getYears(x,as.integer = TRUE)
  baseyear_sel <- y[which(abs(y-baseyear)==min(abs(y-baseyear)))]
  if(baseyear_sel != baseyear) warning(paste0("Year ",baseyear," is not available in historic dataset! Using ",baseyear_sel," as baseyear."))
  
  #calc land-use change wrt to baseyear
  x <- x - setYears(x[,baseyear_sel,],NULL)
  
  #rename variable and unit
  getNames(x) <- gsub("\\|Land Cover\\|","\\|Land Cover Change\\|",getNames(x))
  getNames(x) <- gsub("\\(million ha\\)",paste0("\\(million ha wrt ",baseyear_sel,"\\)"),getNames(x))
  
  return(list(x=x,
              weight=NULL,
              unit=paste0("million ha wrt ",baseyear_sel),
              description="Change of cropland, pasture, urban, other land and forest area over time from the following datasets: FAO, LUH2v2, MAgPIE-Input and SSPresults.
              Cropland: is the land under temporary agricultural crops (multiple-cropped areas are counted only once), temporary meadows for mowing or pasture, land under market and kitchen gardens and land temporarily fallow, and cultivated with long-term crops which do not have to be replanted for several years (such as cocoa and coffee); land under trees and shrubs producing flowers, such as roses and jasmine; 
              Pasture: is the land used permanently (for a period of five years or more) for herbaceous forage crops, either cultivated or naturally growing.
              Forest: is the land spanning more than 0.5 hectares with trees higher than 5 metres and a canopy cover of more than 10 percent (includes temporarily unstocked areas)")
  )
}
