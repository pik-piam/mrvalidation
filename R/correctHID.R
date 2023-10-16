#'@title {correctHID}
#' @description  Correct Irrigated Area
#' 
#' Correct Irrigated Area to 0.5 Degree x 0.5 Degree Grid.
#' Change resolution from 5 arcmin to 0.5 Degree by aggregating. Values in ha are summed up, Values in percent are calculated using mean.
#' 
#' @param x MAgPIE object containing Global Map on Irrigaiton data data at 0.5 Degree resolution
#' @param subtype : subtypes are the same as in readGMIA
#' @return Global Map on Irrigation data as MAgPIE object at a 0.5 Degree resolution.
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource("IrrigatedArea")
#' }
#' @importFrom raster raster extent<- as.matrix aggregate
#' @importFrom utils read.table
#' @importFrom magclass getRegions getYears<-
#' 

correctHID <- function(x, subtype){
  
  if(subtype == "AEI_EARTHSTAT_CP" |subtype == "AEI_EARTHSTAT_IR" | subtype == "AEI_HYDE_FINAL_CP" | subtype == "AEI_HYDE_FINAL_IR" | subtype == "AEI_HYDE_LOWER_CP" |
     subtype == "AEI_HYDE_LOWER_IR" | subtype == "AEI_HYDE_UPPER_CP" | subtype == "AEI_UPPER_FINAL_IR"){
    #read asci file
    years <- c("1900","1910","1920", "1930", "1940", "1950", "1960", "1970", "1980", "1985", "1990", "1995", "2000", "2005")
    # a <- read.table(paste0(subtype, "_", years))
    x <- lapply(paste0(subtype, "_",years, ".asc"),raster)
    

 
  #cellNames <- read.csv("CountryToCellMapping.csv", sep=";")
  mapping <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv", where = "mappingfolder")
  cellNames <- mapping[,1]
  lon <- seq(-179.75,179.75,by=0.5)
  lat <- rev(seq(-89.75,89.75,by=0.5))
  
  #read file
 # x <- raster(file)
  f1 <- function(var1, var2){
    extent(var1) <- var2
    return(var1)
  }
  x <- lapply(x, f1, var2=c(-180,180,-90,90))
  #extent(x) <- c(-180,180,-90,90)
  x <- lapply(x, aggregate, fact=6, fun=sum)
  #checkk for subtype and aggregate according to it
  # if(grepl("ha", subtype))
  # {
  # x <- aggregate(x,fact=6,fun=sum)
  # }
  # else
  # {
  #   x <- aggregate(x,fact=6,fun=mean)
  # }
  #raster to matrix to magpie
  f2 <- function(var1){
    return(t(raster::as.matrix(var1)))
  }
 x <-  lapply(x, f2)
  #x <- t(raster::as.matrix(x))
  mag <- array(NA,dim=c(59199,length(years),1),dimnames=list(cellNames,NULL,NULL))
  getYears(mag) <- years
  # coord <- getCoordinates(res = 0.5, degree=TRUE)
  vars <- c("lon","lat")
coord <- mapping[vars]
coord$lon <- as.numeric(coord$lon)
coord$lat <- as.numeric(coord$lat)
  #Optimization? mag <- x[coord[,1],coord[,2]]
  for(i in 1:length(years)){
  for (j in 1:59199) {
    mag[j,i,] <- x[[i]][which(coord[j, 1]==lon), which(coord[j,2]==lat)]
  }
  }
  y <- as.magpie(mag,spatial=1,temporal=2)
  getNames(y) <- paste0(subtype, " in 5 arcmin resolution")

return(y)
  }else
    return(x)
}