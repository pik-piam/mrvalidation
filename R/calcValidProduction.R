#' @title calcValidProduction
#' @description calculates the validation data for production of agricultural products
#' 
#' @param datasource Datasource of validation data. 
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient The nutrient in which the results shall be reported.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFAOmassbalance}},
#' \code{\link{calcValidDemand}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidProduction")
#' }
#' 

#' @importFrom magpiesets reporthelper summationhelper
calcValidProduction<-function(datasource="FAO", detail=T, nutrient="dm"){
  
  if(datasource=="FAO"){
    mb<-collapseNames(calcOutput("FAOmassbalance",aggregate = F)[,,nutrient][,,"production"])
    mb[,,"woodfuel"] <- mb[,,"woodfuel"] * 0.5 ## For the moment MAgPIE is only modeling 50% of wood demand to be met by traditional woody biomass
    out<-reporthelper(x=mb,dim = 3.1,level_zero_name = "Production",detail = detail)  
  } else stop("No data exist for the given datasource!")
  
  out <- summationhelper(out)
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  names(dimnames(out))[3] <- "scenario.model.variable"
  if(nutrient=="dm"){unit="Mt DM/yr"
  } else if (nutrient=="nr"){unit="Mt Nr/yr"
  } else if (nutrient=="p"){unit="Mt P/yr"
  } else if (nutrient=="k"){unit="Mt K/yr"
  } else if (nutrient=="ge"){unit="PJ/yr"
  } else if (nutrient=="wm"){unit="Mt WM/yr"}
  getNames(out) <- sub("\\|$","",getNames(out)) 
  getNames(out) <- paste0(getNames(out)," (",unit,")")
  
  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Agricultural Production")
  )
}