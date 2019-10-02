#' calcValidSDG12
#' 
#' Returns historical SDG12 Indicators_Sustainable Production and Consumption
#' 
#' @param datasource FAO 
#' @return List of magpie object with data and population
#' @author Edna J. Molina Bacca
#' @import magpiesets
#' @importFrom magclass getNames
#' 
calcValidSDG12 <- function(datasource="FAO") {
  x <- NULL  

  if(datasource=="FAO"){
    
    indicatorname<-"SDG|SDG12|Food loss"
    unit="Mt"
    #foodLoss <- readSource("FAO",subtype="Fbs")
    foodLoss <- readSource("FAO",subtype="CBCrop")
    aggregation <- toolGetMapping("FAOitems.rda", type = "sectoral", where="moinput")
    #standarized items _ magpie object
    a_agg<-speed_aggregate(foodLoss,rel=aggregation,from = "FAOaggregatedItem_fromWebsite", to="k",dim = 3.1, partrel = TRUE)
    #reading only waste data
    out<-collapseNames(a_agg[,,"waste"])*1000/1E6
    #Used to determine the total food loss in each country
    out<-dimSums(out,na.rm = TRUE,dim=3)
    getNames(out)<-paste0(indicatorname," (",unit,")")
    out<- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out<- add_dimension(out, dim=3.2, add="model", nm=datasource)
    x <- mbind(x,out)
    units_x<-unit
    
    indicatorname<-"SDG|SDG12|Material footprint"
    unit<-"tDM/capita/yr"
    matFoot<-foodLoss
    #standarized items _ magpie object
    matFoot_agg<-speed_aggregate(matFoot,rel=aggregation,from = "FAOaggregatedItem_fromWebsite", to="k",dim = 3.1, partrel = TRUE)
    #reading only domestic supply
    out<-collapseNames(matFoot_agg[,,"domestic_supply"])
    out<-dimSums(out,na.rm = TRUE,dim=3)
    popul<-readSource("FAO",subtype='Pop');
    popul<-popul[,,"population"]+10^-10
    com_years<-intersect(getYears(out),getYears(popul))
    popul<-popul[,com_years,]
    out<-out/popul
    getNames(out)<-paste0(indicatorname," (",unit,")")
    out<- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out<- add_dimension(out, dim=3.2, add="model", nm=datasource)
    x <- mbind(x,out)
    units_x<-c(units_x,unit)
    
    indicatorname<-"SDG|SDG12|Food waste"
    unit<-"kcal/cap/day"
    #Reads available food
    AvFood<-readSource("FAO",subtype="Fbs")
    AvFood<-AvFood[,,"food_supply_kcal/cap/day"]
    AvFood<-dimSums(AvFood,dim=3)
    #Calculate expected intake. Source is Lutz2014. Average for male,female,ages.ssp1 (historical trend)
    intake<-calcIntake()
    intake<-intake$x[,,"B.All.SSP1"]
    #intersect years
    com_years<-intersect(getYears(AvFood),getYears(intake))
    out<-AvFood[,com_years,]-intake
    #if the country doesn't have Supply data the value calculated would be less than 0
    out[out<0]<-0
    getNames(out)<-paste0(indicatorname," (",unit,")")
    out<- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out<- add_dimension(out, dim=3.2, add="model", nm=datasource)
    common_yrs = intersect(getYears(x),getYears(out))
    x <- mbind(x[,common_yrs,],out[,common_yrs,])
    units_x<-c(units_x,unit)
    
    popul<-popul[,com_years,]
    
    
    
    } else stop("No data exist for the given datasource!")
  
  return(list(x=x,weight=popul,unit=units_x,description="The present function calculates validation data for the indicators relevant to SDG12 "))
  
}

