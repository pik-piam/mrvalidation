#' @title calcValidAgFFGDP
#' @description validation for agricultural, fisheries, forestry value added gdp (Million 05USD)
#' @param datasource datasource for validation (WDI)
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author David Chen
#' @importFrom magclass collapseNames
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' calcOutput("ValidAgFFGDP")
#' }

calcValidAgFFGDP<- function(datasource="WDI") {
  
  if (datasource=="WDI") {
    AgG <-  readSource("WDI",subtype="NV.AGR.TOTL.KD")
    #deflate from constant 2010 to constant 2005 $
    gdpC <- readSource("WDI",subtype="NY.GDP.MKTP.CD")
    gdpC <- collapseNames(gdpC)
    
    gdp2010 <- readSource("WDI",subtype="NY.GDP.MKTP.KD")
    gdp2010 <- collapseNames(gdp2010)
    
    infl <- gdpC[,2005,]/gdp2010[,2005,]
    infl[is.na(infl)] <- 0
    infl[is.infinite(infl)] <- 0
    
    AgG05 <- AgG*setYears(infl,NULL)
    
    out <- collapseNames(AgG05)
    #set na's to 0
    
  }else{
    stop("unknown datasource")}
  
  getNames(out) <- "Value|Agriculture, Forestry and Fisheries GDP (million US$05/yr)"
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  
  return(list(x=out,
              weight=NULL,
              unit="million US$05/yr",
              description="Agriculture Fisheries Forestry Value added GDP"))
}