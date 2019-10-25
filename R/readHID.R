#' @title readHID
#' @description {Read Historic Irrigation Data
#' 
#' Read the Data from Siebert et.al on Historic Irrigation for each Country. Data contains Area equiped for Irrigation (AEI) from 1900 to 2005 in ten or five year timesteps
#' 
#' The Data is also available spatialy explicit with a resolution of 5 arcmin:
#' }
#' 
#' 
#' 
#' 
#' @param subtype : Available subtypes are:
#' \itemize{
#' \item national_1900_2005 : National Data on AEI
#' \item AEI_EARTHSTAT_CP : Data in 5armin gridded resolution based on Earthstat LU scenario and maximum consistency with pasture extent
#' \item AEI_EARTHSTAT_IR : Data in 5armin gridded resolution based on Earthstat LU scenario and maximum consistency with  AEI from subnationals regions
#' \item AEI_HYDE_LOWER_CP : Data in 5armin gridded resolution based on Hyde LU scenario from the lower end of the uncertainty band and maximum consistency with pasture extent
#' \item AEI_HYDE_LOWER_IR : Data in 5armin gridded resolution based on Hyde LU scenario from the lower end of the uncertainty band and maximum consistency with AEI from subnational regions
#' \item AEI_HYDE_FINAL_CP : Data in 5armin gridded resolution based on Hyde LU scenario final version and maximum consistency with pasture extent
#' \item AEI_HYDE_FINAL_IR : Data in 5armin gridded resolution based on Hyde LU scenario final version and maximum consistency with AEI from subnational regions
#' \item AEI_HYDE_UPPER_CP : Data in 5armin gridded resolution based on Hyde LU scenario from the upper end of the uncertainty band and maximum consistency with pasture extent
#' \item AEI_HYDE_UPPER_IR : Data in 5armin gridded resolution based on Hyde LU scenario from the upper end of the uncertainty band and maximum consistency with AEI from subnational regions
#' }
#' @details Further information:
#' \itemize{
#'  \item S. Siebert, M. Kummu, M. Porkka, P. D??ll, N. Ramankutty, and B.R. Scanlon. 2015.  A global data set of the extent of irrigated land from 1900 to 2005. Hydrology and Earth System Sciences. Vol. 19. p. 1521-1545. 
#'  \item K. K. Goldwijk, A. Beusen, G. van Drecht, and M. de Vos. 2011. The HYDE 3.1 spatially explicit database of human-induced global land-use change over the past 12,000 years. Global Ecology and Biogeography. Vol. 20. p. 73-86
#'  }
#' @return magpie object of the Irrigated Area  data
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{ 
#' a <- readSource("HID", "national_1900_2005")

#' }
#' @importFrom magclass getCells<-
#' @importFrom reshape2 melt

readHID <- function(subtype=NULL){
  
  files=c(national_1900_2005="Supplement_S3_AEI per_region_country_subnational.csv")
         # AEI_ES_CP="ASCII-Files\\AEI_EARTHSTAT_CP_1900.asc")
  
  
  #check wether data is gridded
if(subtype=="national_1900_2005")
{
  file <- toolSubtypeSelect(subtype, files)
  #read data an remove global value and empty rows
  a <- read.csv(file, sep = ";")
  a <- subset(a, a$COUNTRY != ("WORLD"))
  a <- subset(a,a$COUNTRY != "")
  a <- a[,c(2,4:length(colnames(a)))]
  #change year names so they match magpie standard
  colnames(a)[-1]  <- paste0("y",gsub("\\D", "",colnames(a)[-1]))
 
 
  
#melt object
   molten <- melt(a,id.vars=1)
   molten[,2] <- paste0("y",gsub("\\D", "",molten[,2]))
  #convert to magpieobject
   b <- as.magpie(molten, spatial=1, temporal=2)
   getSets(b)<-c("Regions", "Years", "AEI")
   getNames(b) <- "AEI in ha"
   return(b)
}
 # if object is gridded 
else if(subtype == "AEI_EARTHSTAT_CP" |subtype == "AEI_EARTHSTAT_IR" | subtype == "AEI_HYDE_FINAL_CP" | subtype == "AEI_HYDE_FINAL_IR" | subtype == "AEI_HYDE_LOWER_CP" |
        subtype == "AEI_HYDE_LOWER_IR" | subtype == "AEI_HYDE_UPPER_CP" | subtype == "AEI_HYDE_UPPER_IR"){
  #read asci file
  years <- c("1900","1910","1920", "1930", "1940", "1950", "1960", "1970", "1980", "1985", "1990", "1995", "2000", "2005")
 # a <- read.table(paste0(subtype, "_", years))
  a <- lapply(paste0(subtype, "_",years, ".asc"),read.table)
  f <- function(input){
b <- as.vector(t(input))
b <- as.numeric(b[-(1:12)])
b <- as.data.frame(b)
b <- as.magpie(b, spatial=1)
  }
  c <- lapply(a,f)
  #d <- new.magpie(cells_and_regions = getRegions(c[[1]]),years = years,names = getNames(c[[1]]), sets = getSets(c[[1]]), fill=0)
  d <- array(NA,dim= c(length(c[[1]]),length(years),1))
  d <- as.magpie(d, spatial=1 , temporal=2)
  getYears(d)<-years
  a <- NULL
  #TODO: Applying function with two parameters
  for(n in 1:length(getYears(d)))
    {
    d[,getYears(c[[n]]),] <- c[[n]]
    }
#  f2 <- function(x,y){
# y[,getYears(x),] <- x
# return(y)
# }
#d <- sapply(c, f2, y=d)
  getNames(d) <- paste0(subtype, " in 5 arcmin resolution")
  getCells(d) <- paste0("GLO.",1:dim(d)[1])
  gc()
  return(d)
}
  
}
