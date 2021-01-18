#' @title Gasser
#' @description Read historic land-use change CARBON emissions
#' 
#' @param subtype subtype
#' @return magpie object containing data land-use change CARBON emissions
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("Gasser")
#' }
#' @importFrom madrat toolSubtypeSelect toolMappingFile

readGasser <- function(subtype="regional"){
  
  if(subtype=="regional"){
    #C:/PIK/data_processing/inputdata/sources/Gasser/
    file <- "Bookkeeping LULCC emissions (ELUC).csv"
    
    dataset <- read.csv(file = file,header = TRUE,skip = 7,sep = ";")
    x <- as.magpie(dataset,spatial="Region")
    x <- x[,,grep(pattern = "U_",x = getNames(x),value = TRUE,invert = TRUE)]
    
    mapping_file_gasser <- "GasserMapping.csv"
    mapping_gasser <- read.csv(file = mapping_file_gasser,header = TRUE,sep = ";")
    mapping_magpie <- toolMappingFile(type = "regional",name = "h12.csv",readcsv = TRUE)
    colnames(mapping_magpie)[1] <- "ISOName"
    mapping <- merge(mapping_gasser,mapping_magpie,all=FALSE)
    
    weight <- collapseNames(setYears(readSource("FRA2020",subtype = "forest_area",convert = TRUE)[,"y2010","landArea"],NULL))
    
    out <- toolAggregate(x = x,rel = mapping,weight = weight[unique(mapping$CountryCode),,],from = "Region",to = "CountryCode",dim=1)
    
    getSets(out) <- c("region", "year", "source")
    getNames(out) <- c("Gasser_2020","LUH2_GCB_2019","FRA_2015")
    return(out)
  } else if(subtype=="global"){
    #C:/PIK/data_processing/inputdata/sources/Gasser/
    file <- "Bookkeeping LULCC emissions (ELUC) Disaggregated.csv"
    
    dataset <- read.csv(file = file,header = TRUE,skip = 7,sep = ";")
    x <- as.magpie(dataset)
    x <- x[,,grep(pattern = "U_",x = getNames(x,dim=2),value = TRUE,invert = TRUE)]
    out <- x
    getSets(out) <- c("region", "year", "variable", "source")
    getNames(out,dim=2) <- c("Gasser_2020","LUH2_GCB_2019","FRA_2015")
    return(out)
  } else {stop("Invalid subtype.")}
  
}
