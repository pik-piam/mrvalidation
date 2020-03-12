#' @title calcValidLivestockDemStructure
#' @description calculates the validation data for the share of different livestock products (excluding fish) in total livestock calorie food supply
#' 
#' @param datasource Datasource of validation data. If "FAO", we use FAO calories with FAO population data (slightly diverges from original data as the convert script for example splits up countries for the past).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl
#' @seealso
#' \code{\link{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidLivestockDemStructure")
#' }
#' 
#' @importFrom magpiesets findset

calcValidLivestockDemStructure<-function(datasource="FAO"){
  
  if(datasource%in%c("FAO")){
    group="kli"
    kli=findset("kli")
    x = calcOutput("FoodSupplyPast",products="kall",per_capita=FALSE,aggregate = FALSE)
    x = collapseNames(x[,,"kcal"])/365*1000000
    weight = dimSums(x[,,kli],dim=3.1) 
    
    out=x[,,kli]/weight
    
    level_zero_name <- "Nutrition|Dietary Composition|Livestock Demand Structure"
    out<-reporthelper(x=out,level_zero_name = level_zero_name,detail=T)
    
    nosum  <- out[,,paste(level_zero_name,reportingnames(group),sep="|"),invert=TRUE]
    out <- summationhelper(nosum)
    getNames(out) <- paste(getNames(out),"(kcal/kcal)",sep=" ")
    
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    if(any(is.nan(out))){out[is.nan(out)]=0}
    if(any(out==Inf)){out[out==Inf]=0}
  } else {stop("unknown data source")}
  
  return(list(x=out,
              weight=weight,
              unit="Kcal/Kcal",
              description="Share of livestock product calories within total calories of animal origin (excluding fish)",
              min=0,
              max=1)
  ) 
}
