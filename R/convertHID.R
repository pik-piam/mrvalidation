#' Convert Historic Irrigation Data
#' 
#' @title convertHID
#' 
#' @description Convert subtypes on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing IrrigatedArea data on Country level
#' @param subtype : No subtype needed
#' @return Irrigated Area data as MAgPIE object on country level Missing values are added as NA
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @importFrom madrat getISOlist
#' @examples
#' 
#' \dontrun{
#' a <- readSource("HID", "national_1900_2005")

#' }
#' 


convertHID <- function(x, subtype){
  if(subtype=="national_1900_2005"){
    y <- toolCountryFill(x[intersect(getRegions(x),getISOlist()),,], fill = 0, verbosity = 2)
    #disaggegation of "SMK" is made using the share of Agricultural area of "SMK" that belongs to Serbia (0.9) and to Montenegro (0.1) in 2006
    y["SRB",,] <- as.numeric(x["SMK",,])*0.9
    y["MNE",,] <- as.numeric(x["SMK",,])*0.1
    
  }
  else
  { 
    map <- toolMappingFile(type="cell", readcsv=T, name="CountryToCellMapping.csv")
    y <- toolAggregate(x, rel=map, from=1, to=3, partrel = T)
    y <- toolCountryFill(y)
  }
  return(y)
  
}