#' @title calcValidVegfruitShare
#' @description calculates the validation data for the share of vegetables, fruit and nuts products (including fish) in total calorie food supply
#'
#' @param datasource Datasource of validation data. If "FAO", we use FAO calories with FAO population data (slightly diverges from original data as the convert script for example splits up countries for the past). In the case of "PopulationPast", we also use FAO calorie values, but divide them by our standard population
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link[mrcommons]{calcFoodSupplyPast}},
#' \code{\link{calcValidKcal}}
#' @examples
#'
#' \dontrun{
#' calcOutput("ValidVegfruitShare")
#' }
#'
#' @importFrom magclass getSets

calcValidVegfruitShare<-function(datasource="FAO"){

  if(datasource%in%c("FAO")){
    x = calcOutput("FoodSupplyPast",products="kall",per_capita=FALSE,aggregate = FALSE)
    x= collapseNames(x[,,"kcal"])/365*1000000
    l = dimSums(x[,,"others"],dim=3.1)
    weight = dimSums(x,dim=3.1)

    out=l/weight

    getNames(out) <- "Nutrition|Dietary Composition|Vegetables Fruits and Nuts Share (cal/cal)"
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    if(any(is.nan(out))){out[is.nan(out)]=0}
    if(any(out==Inf)){out[out==Inf]=0}
  } else {stop("unknown data source")}

  return(list(x=out,
              weight=weight,
              unit="kcal/kcal",
              description="Share of Food Supply Calories derived from vegetables, fruits and nuts",
              min=0,
              max=1)
  )
}

