#' @title calcValidAgGDP
#' @description Validation for agricultural value added gdp (Million 05USD)
#' @param datasource datasource for validation. Options FAO and FAO-consum
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @importFrom magclass collapseNames
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' calcOutput("ValidAgGDP")
#' }

calcValidAgGDP<- function(datasource="FAO") {
  
   if (datasource == "FAO"){
    
    Prod_kcr<-collapseNames(calcOutput("Production",products="kcr",aggregate= FALSE,attributes="dm"))
    Prod_kli<-collapseNames(calcOutput("Production",products="kli",aggregate= FALSE,attributes="dm"))
    
    prices_kcr<-setYears(calcOutput("PricesProducer",products = "kcr",aggregate=FALSE)[,2005,],NULL)
    prices_kli<-setYears(calcOutput("PricesProducer",products = "kli",aggregate=FALSE)[,2005,],NULL)
    
    names_kcr<-intersect(getNames(Prod_kcr),getNames(prices_kcr))
    names_kli<-intersect(getNames(Prod_kli),getNames(prices_kli))
    years<-intersect(getYears(Prod_kli),getYears(Prod_kcr))
    
    Vprod_all<- dimSums(Prod_kcr[,years,names_kcr]*prices_kcr[,,names_kcr],dim=3)+
      dimSums(Prod_kli[,years,names_kli]*prices_kli[,,names_kli],dim=3)
    
    ## Secondary seed and feed
    
    #Seed
    seed_kcr<-collapseNames(calcOutput("Seed",products="kcr",attributes="dm",aggregate = FALSE))
    seed_kli<-collapseNames(calcOutput("Seed",products="kli",attributes="dm",aggregate = FALSE))
    
    #feed
    feed<-collapseNames((calcOutput("FAOmassbalance",aggregate = FALSE)[,,"feed"])[,,"dm"])
    kcr<-findset("kcr")
    kli<-findset("kli")
    
    feed_kcr<-feed[,,kcr]
    feed_kli<-feed[,,kli]
    
    #Price consumers (World Prices)
    prices_kcr_con<-setYears(calcOutput("IniFoodPrice",products = "kcr",aggregate=FALSE),NULL)
    prices_kli_con<-setYears(calcOutput("IniFoodPrice",products = "kli",aggregate=FALSE),NULL)
    
    years<-intersect(getYears(seed_kcr),
                     intersect(getYears(feed_kcr),getYears(Vprod_all)))
    
    Vdemand<-dimSums((seed_kcr[,years,]+feed_kcr[,years,])*prices_kcr_con,dim=3)+
      dimSums((seed_kli[,years,]+feed_kli[,years,])*prices_kli_con,dim=3)
    
    out<-Vprod_all[,years,]-Vdemand[,years,]
    out[out<0]<-0
    
    
   }else if(datasource == "FAO-consum"){
     
     #Food and material demand
     kall<-findset("kall")
     food_mat<-collapseNames(dimSums((calcOutput("FAOmassbalance",aggregate = FALSE)[,,kall][,,c("food","other_util")])[,,"dm"],dim=3.2))
     
     #Price consumers (World Prices)
     prices_kall_con<-setYears(calcOutput("IniFoodPrice",products = "kall",aggregate=FALSE),NULL)
     
     #Consumption value and production value should be the same at global level
     out<-dimSums(dimSums(food_mat*prices_kall_con,dim=3),dim=1)
     
   }else{ 
    stop("unknown datasource")
     }
  
  getNames(out) <- "Value|Agriculture GDP (million US$05/yr)"
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  
  return(list(x=out,
              weight=NULL,
              unit="million US$05/yr",
              description="Agriculture Value added GDP"))
}