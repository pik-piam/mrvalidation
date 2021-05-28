#' @title readPardeyAgRD
#' @description Agricultural R&D investment data read from https://www.nature.com/news/agricultural-rd-is-on-the-move-1.20571
#' 3 tables are read in: AgRD_Pardey is public Ag expenditure in 1960 and 2011, extracted from the interactive figure in the article that has more complete countries
#' agGERD and agPERD are total and public expenditures respectively, for less countries but more years
#' @return magpie object containing expenditure in Ag R&D, 2009 USD PPP
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @examples
#'
#' \dontrun{
#' a <- readSource("PardeyAgRD")
#' }
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom madrat toolCountry2isocode
#' @importFrom GDPuc convertGDP


readPardeyAgRD <- function() {
  
  mapping <- toolGetMapping("regionmappingH12.csv","regional")
  
### perd data only public investment
  perd <- read.csv("agPERD.csv")
  colnames(perd)[1] <- "Region"
  colnames(perd) <- gsub("X", "",colnames(perd))
  
  perd <- pivot_longer(perd, cols=c(2:length(colnames(perd))), names_to = "Year", values_to = "PERD")
  #convert from billions to million USD
  perd$PERD <- perd$PERD * 1000
  #separate out the countries from the aggregates for using the aggregates later
  perdc <- perd
  perdc$Region <- toolCountry2isocode(perdc$Region)
  perdc <- perdc[!is.na(perdc$Region),]
  perdc <- as.magpie(perdc, spatial=1, temporal=2, tidy=TRUE)
  perd <- as.magpie(perd, spatial=1, temporal=2, tidy=TRUE)

### GERD data total public and private
  gerd <- read.csv("agGERD.csv")
  colnames(gerd)[1] <- "Region"
  colnames(gerd) <- gsub("X", "",colnames(gerd))
  gerd <- pivot_longer(gerd, cols=c(2:length(colnames(gerd))), names_to = "Year", values_to = "GERD")
  #convert from billions to million USD
  gerd$GERD <- gerd$GERD * 1000
  #separate out the countries from the aggregates for using the aggregates later
  gerdc <- gerd
  gerdc$Region <- toolCountry2isocode(gerdc$Region)
  gerdc <- gerdc[!is.na(gerdc$Region),]
  gerdc <- as.magpie(gerdc, spatial=1, temporal=2, tidy=TRUE)
  gerd <- as.magpie(gerd, spatial=1, temporal=2, tidy=TRUE)

### Get ratio of private/public investment
  getRegions(gerd) <- getRegions(perd)
  getRegions(gerdc) <- getRegions(perdc)
  ratio <- gerd/perd
  ratioc <- gerdc/perdc
  #countries for which GERD data exists
  countries <- getRegions(ratioc)

### Read data with more complete countries and bump PERD up to GERD based on regional ratios
  full <- read.csv("AgRD_Pardey.csv")
  colnames(full)[c(3,4)] <- substr(colnames(full)[c(3,4)],5,8)
  full <- pivot_longer(full[,-2], cols = c(2,3), names_to="Year", values_to = "PERD")
  full$id <- as.character(full$id)
  full$id[c(61,62)] <- "Sao Tome and Principe" ; full$id[c(27,28)] <- "Cote d Ivoire" ; full$id[c(71,72)] <- "Sudan"
  
  full$id <- toolCountry2isocode(full$id)
  full <- as.magpie(full, spatial=1, temporal=2)
  
# bind GERD for countries that have the data, assume ratio same as in 1960

  fullgerd1960 <- full[countries,1960,] * setYears(ratioc[,1980,],NULL) 
  fullgerd <- mbind(collapseNames(fullgerd1960, collapsedim=1), gerdc)    

#### multiply the PERD data by the 2011 and 1980 GERD/PERD ratios for the smaller countries, based on regional share
  
  #rename regions to match our mapping
  getRegions(ratio)[which(getRegions(ratio)=="EastSouth Asia and Pacific")] <- "OAS" 
  getRegions(ratio)[which(getRegions(ratio)=="LAC")] <- "LAM" 
  getRegions(ratio)[which(getRegions(ratio)=="MENA")] <- "MEA" 
  
  #make regional aggregations and apply other regions to EUR has available countries for mean, SPain 1980 looks like outlier
  missing_regions <- setdiff(unique(mapping$RegionCode),getRegions(ratio))

  ratio_missing <- new.magpie(cells_and_regions = missing_regions,
                          years = getYears(ratio),
                          names = getNames(ratio))
  ratio_missing["EUR",,] <- dimSums(ratio[c("Germany","France","Italy","Spain"),,],dim=1)/4  
  ratio_missing[c("NEU","REF"),,] <- ratio["Upper Middle",,]  
  ratio_missing["CAZ",,] <- ratio["High Income",,]
  
  ratio <- mbind(ratio, ratio_missing[where(is.na(ratio_missing))$true$regions,,inv=T])
  
  #map smaller countries to region
  y_countries <- as.data.frame(full[countries,,inv=T])[,-1]  
  y_countries <- merge(y_countries,mapping[,c(2,3)], by.x ="Region", by.y="CountryCode")
  # setdiff(getRegions(full[countries,,inv=T]),unique(y_countries$Region)) check that all countries got mapped
  

# multiply those countriesin OAS, MEA, LAM, SSA by regional share in 1960 and 1980
#NOTE assumption: share of  private investment in 1960  based on 1980 levels 
  aggr <- intersect(getRegions(ratio),unique(mapping$RegionCode))
  aggr_ratio <- as.data.frame(setYears(ratio[aggr,c(1980,2011),],c(1960,2011)))[,-c(1,4,5)]

  y_countries <- merge(y_countries, aggr_ratio,  by.x=c("RegionCode", "Year"), by.y=c("Region", "Year"))
  y_countries$GERD <- y_countries$Value.x * y_countries$Value.y

  gerdy <- as.magpie(y_countries[,c(2,3,7)],spatial=2,temporal=1,tidy=T)

  gerdy <- time_interpolate(gerdy, interpolated_year = getYears(fullgerd))

pardey <- mbind(fullgerd,gerdy)

#convert PARDEY from 2009 PPP to 2005 USD MER
 pardey1 <- convertGDP(pardey, unit_in="constant 2009 Int$PPP", unit_out = "constant 2005 US$MER")
 #for missing countries use USA rate for now
 pardey1[where(is.na(pardey1))$true$regions,,] <- pardey[where(is.na(pardey1))$true$regions,,] * setYears(pardey1["USA",2000,]/pardey["USA",2000,],NULL)
 pardey <- pardey1
### read in OECD data to get missing countries: REF missing from pardey data
  oecd <- read.csv("GERD_FORD_OECD.csv")
  oecd <- oecd[,-c(2,3,5,7,9,11,12,13,14,15,16,18,19)]

oecd <- as.magpie(oecd, spatial=1, temporal=5)

oecd <- oecd[,,"Agricultural and veterinary sciences"][,,"2015 Dollars - Constant prices and PPPs"]

#check whether total is equal to other categories combined when total exists
#nt <- dimSums(oecdm[,,"Total intramural", inv=T], na.rm=T)
#round(oecdm[,,"Total intramural"] - nt) #only polen and latvia has higher total

total <- dimSums(oecd[,,"Total intramural", inv=T],na.rm=T)
total["POL",,] <- oecd["POL",,"Total intramural"] 
#2015$ price and PPP

getNames(total) <- "GERD"

### note the currencies are different! this is in 2015 and pardey in 2009 PPP
# CONVERT TO 2009USD
#library(GDPuc)

oecd_rus<- setdiff(getRegions(total), getRegions(pardey))

#assume share of these countries as global total same in the past as data only intersects in 2010/2011
rus_shr <- magpply(total[oecd_rus,c(2010,2011),]/dimSums(pardey[,c(2010,2011),],dim=1), mean, MARGIN=1)

past_rus <- new.magpie(cells_and_regions=oecd_rus, years = getYears(pardey),names = getNames(pardey))
past_rus <- rus_shr * dimSums(pardey, dim=1)

#drop TWN which sneaked in there, fix later
past_rus <- past_rus["TWN",,inv=T]

past_rus <- convertGDP(past_rus, unit_in = "constant 2015 Int$PPP", unit_out = "constant 2005 US$MER")

out <- mbind(pardey, past_rus)

return(out)
}