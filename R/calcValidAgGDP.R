#' @title calcValidAgGDP
#' @description validation for agricultural, fisheries, forestry value added gdp (Million 05USD)
#' @param datasource datasource for validation, WDI and FAO
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author David Chen, Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' calcOutput("ValidAgGDP")
#' }

calcValidAgGDP<- function(datasource="WDI") {

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

  } else if (datasource == "FAO"){

    Prod_kcr<-collapseNames(calcOutput("Production",products="kcr",aggregate= FALSE,attributes="dm"))
    Prod_kli<-collapseNames(calcOutput("Production",products="kli",aggregate= FALSE,attributes="dm"))

    prices_kcr<-calcOutput("PricesProducer",products = "kcr",aggregate=FALSE)[,2005,]
    prices_kli<-calcOutput("PricesProducer",products = "kli",aggregate=FALSE)[,2005,]

    names_kcr<-intersect(getNames(Prod_kcr),getNames(prices_kcr))
    names_kli<-intersect(getNames(Prod_kli),getNames(prices_kli))

        Vprod_all<- dimSums(Prod_kcr[,,names_kcr]*setYears(prices_kcr[,,names_kcr],NULL),dim=3)+
      dimSums(Prod_kli[,,names_kli]*setYears(prices_kli[,,names_kli],NULL),dim=3)

    # Secondary seed and feed

    demand_feed<-collapseNames(calcOutput("FAOFodder_aggrFEED",aggregate=FALSE)[,,"production"])
    prices<-calcOutput("PriceAgriculture",datasource="FAO",aggregate=FALSE)
    prices_FF<-mbind(prices[,,"historical.FAO.maiz"],prices[,,"historical.FAO.maiz"],prices[,,"historical.FAO.others"])
    getNames(prices_FF)<-getNames(demand_feed)

    Vdemand<-dimSums(demand_feed[,,]*setYears(prices_FF[,2005,],NULL),dim=3)

    years<-intersect(getYears(Vdemand),getYears(Vprod_all))
    out<-Vprod_all[,years,]-Vdemand[,years,]


  }
  else {stop("unknown datasource")}

  getNames(out) <- "Value|Value added agriculture forestry fishery"
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm=datasource)

  return(list(x=out,
              weight=NULL,
              unit="Million US$05",
              description="Agriculture Fisheries Forestry Value added GDP"))
}
