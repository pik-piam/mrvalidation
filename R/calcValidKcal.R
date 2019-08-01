#' @title calcValidKcal
#' @description calculates the validation data for calorie food supply
#' 
#' @param datasource Datasource of validation data. If "FAO", we use FAO calories with FAO population data (slightly diverges from original data as the convert script for example splits up countries for the past). In the case of "PopulationPast", we also use FAO calorie values, but divide them by our standard population. "Bodirsky2015" uses projections from slightly altered method of the Bodirsky et al 2015 PLOS ONE paper.
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient kcal or protein
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' \code{\link{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidKcal")
#' }
#' 
#' @importFrom magpiesets reporthelper
#' @importFrom magclass getSets dimOrder
#' @importFrom moinput toolFAOcombine

calcValidKcal<-function(datasource="FAO", nutrient="kcal", detail=TRUE){
  if(length(nutrient)>1){stop("select only one nutrient")}
  if(datasource%in%c("FAO","FAOmassbalance")){
    
    if (datasource=="FAOmassbalance"){
      
      x      <- calcOutput("FoodSupplyPast",products="kall",per_capita=FALSE,aggregate = FALSE,attributes=nutrient)
      x2     <- calcOutput("FoodSupplyPast",products="kall",per_capita=TRUE,aggregate = FALSE,supplementary = TRUE,attributes=nutrient,populationweight="PopulationPast")
      value  <- x*1000000
      weight <- x2$weight 
      total=dimSums(value,dim=3)
      
    } else if (datasource=="FAO") {
      FSCrop <- readSource("FAO", "FSCrop")
      FSLive <- readSource("FAO", "FSLive")
      FS <- toolFAOcombine(FSLive,FSCrop, combine="Item")
      
      FS<-FS[,,c("food_supply_kcal","protein_supply")]
      getNames(FS,dim=2)<-c("kcal","protein")
      FS<-collapseNames(FS[,,nutrient])
      total=FS[,,"2901|Grand Total + (Total)"]
      
      relationmatrix <- toolGetMapping("FAOitems.rda","sectoral",where="moinput")
      relationmatrix <- relationmatrix[,which(names(relationmatrix)%in%c("FoodBalanceItem","k"))]
      relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[,1])==T),]
      
      
      FS<-toolAggregate(x = FS,rel =relationmatrix,dim = 3.1,from = "FoodBalanceItem",to = "k", partrel=TRUE)
      missing<-setdiff(findset("kall"),getNames(FS,dim=1))
      FS<-add_columns(FS,addnm = missing,dim = 3.1)
      FS[,,missing]<-0
      value=collapseNames(FS)
      weight <- collapseNames(readSource(type="FAO",subtype = "Pop",convert = T)[,getYears(FS),"population"])/1000000
    }
    
    if(nutrient=="kcal"){
      mainname="Nutrition|Calorie Supply"
    } else if (nutrient=="protein"){
      mainname="Nutrition|Protein Supply"
    }
    
    out    <- reporthelper(x=value,level_zero_name = mainname,detail = detail)
    #make sure sum is not neglecting products
    out[,,mainname]<-collapseNames(total)
    
    sumup  <- getNames(out[,,mainname,invert=TRUE],dim=1)
    out<-out[,,c(mainname,sumup)] # right order
    getNames(out,dim=1)  <- c(mainname,getNames(summationhelper(out[,,sumup], sep="+", dim=3.1),dim=1))

    out<-out/weight/365
    out[is.nan(out)]<-0
    out[is.infinite(out)]<-0
    weight[out[,,mainname]==0]<-0
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
    weight <- add_dimension(weight, dim=3.1, add="scenario", nm="historical")
    weight <- add_dimension(weight, dim=3.2, add="model", nm=datasource)
    
  } else if (datasource=="Bodirsky2015"){
        
    tmp    <- calcOutput(type = "Demand",aggregate = F)
    tmp    <- dimOrder(tmp,c(2,1))
    getSets(tmp)[3] <- "scenario"
    getSets(tmp)[4] <- "variable"
    selection <- c("SSP1","SSP2","SSP3","SSP4","SSP5",         
                   "SSP1_boundary","SSP2_boundary","SSP3_boundary","SSP4_boundary","SSP5_boundary",
                   "a1","a2","b1","b2")
    tmp <-  tmp[,,selection]
    x      <- tmp[,,c("kcal_pc","ls","vegfruit_share","pop","dem")]
    staples_kcal_structure<-calcOutput("Staples_kcal_structure",aggregate = FALSE)
    livestock_kcal_structure<-calcOutput("Livestock_kcal_structure",aggregate = FALSE)
    kap<-findset("kap")
    kst<-findset("kst")
    kcal<-mbind(
      collapseNames(x[,,"kcal_pc"]*x[,,"ls"])*livestock_kcal_structure,
      collapseNames(x[,,"kcal_pc"]*(1-x[,,"ls"]-x[,,"vegfruit_share"]))*staples_kcal_structure,
      add_dimension(collapseNames(x[,,"kcal_pc"]*x[,,"vegfruit_share"]),dim = 3.2,add ="data1",nm = "others")
    )
    
    missing<-setdiff(findset("kall"),getNames(kcal,dim=2))
    kcal<-add_columns(kcal,dim = 3.2,addnm = missing)
    kcal[,,missing]<-0
    kcal<-reporthelper(x = kcal,dim = 3.2,level_zero_name = "Nutrition|Calorie Supply",detail = T,sort = T)
    kcal<-summationhelper(kcal,dim = 3.2)
    #remove unnecessary entries
    kcal<-kcal[,,getNames(kcal,dim=2)[as.vector(dimSums(kcal,dim=c(1,2,3.1))>0)]]
    
    getNames(kcal)<-paste0(getNames(kcal), " (kcal/capita/day)")
    
    composition<-x[,,c("ls","vegfruit_share")]
    getNames(composition,dim = 2)<- c("Nutrition|Dietary Composition|Livestock Share (kcal/kcal)","Nutrition|Dietary Composition|Vegetables Fruits Nuts Share (kcal/kcal)")

    out<-mbind(kcal, composition)
    weight<-out
    weight[,,] <- collapseNames(x[,,"pop"])
    weight[,,getNames(composition)] <- collapseNames(x[,,"dem"])
    
    out  <- add_dimension(out, dim=3.1, add="model", nm=datasource)
    weight <- add_dimension(weight, dim=3.1, add="model", nm=datasource)
    
    if(any(is.nan(out))){out[is.nan(out)]=0}
    if(any(out==Inf)){out[out==Inf]=0}

  } else {stop("unknown data source")}

  return(list(x=out,
              weight=weight,
              unit="kcal/capita/day, protein/capita/day and kcal/kcal",
              description="FAO datasource contains slight alterations of original data, e.g. historical divison of countries. Bodirsky 2015 datasource contains projections for SSPs and SRES scenarios based on the (slightly adapted) methodology of Bodirsky 2015 PLOS ONE",
              min=0,
              max=7000)
  ) 
}

