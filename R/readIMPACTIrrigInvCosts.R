#' @title       readIMPACTIrrigInvCosts
#' @description Average annual baseline water-related investment cost data (2016-2030) read from Rosegrant et al. (2017) "Quantitative Foresight Modeling to Inform the CGIAR Research Portfolio"
#' 
#' @return magpie object containing Average annual baseline water-related investment cost on IMPACT-region level (in billion 2000 US$ per year)
#' 
#' @author Felicitas Beier
#' 
#' @seealso \code{\link{readSource}}
#' @examples
#'
#' \dontrun{
#' a <- readSource("IMPACTIrrigInvCosts", convert=TRUE)
#' }
#' 
#' @importFrom madrat toolCountry2isocode
#' @importFrom magclass new.magpie mbind getCells getNames getRegions

readIMPACTIrrigInvCosts <- function() {
  
  # time frame
  years <- seq(2016, 2030)
  
  # read in data
  a           <- read.csv("Rosegrant2017_TableK2.csv", header=F)
  names(a)    <- paste(a[1,],a[2,],sep=".")
  names(a)[1] <- "Region"
  a           <- a[-c(1,2),]

  # select relevant scenarios and transform to magpie object
  IPSL <- data.frame(Region=a$Region, BAU_IPSL=as.numeric(a$IPSL.Baseline_Expansion))
  IPSL <- as.magpie(IPSL, spatial=1, tidy=T)
  
  HGEM <- data.frame(Region=a$Region, BAU_HGEM=as.numeric(a$HGEM.Baseline_Expansion))
  HGEM <- as.magpie(HGEM, spatial=1, tidy=T)
  
  NoCC <- data.frame(Region=a$Region, BAU_NoCC=as.numeric(a$NoCC.Baseline_Expansion))
  NoCC <- as.magpie(NoCC, spatial=1, tidy=T)
  
  tmp  <- mbind(IPSL, HGEM, NoCC)
  x    <- new.magpie(cells_and_regions = getRegions(tmp), years = years, names = getNames(tmp))
  x[,years,] <- tmp
  
  names(dimnames(x))[3] <- "scenario"
  
  return(x)
}
