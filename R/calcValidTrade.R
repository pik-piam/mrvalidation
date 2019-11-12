#' @title calcValidTrade
#' @description calculates the validation data for trade of agricultural products
#' 
#' @param datasource Datasource of validation data. 
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient The nutrient in which the results shall be reported.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Xiaoxi Wang
#' @seealso
#' \code{\link{calcFAOmassbalance}},
#' \code{\link{calcValidDemand}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidTrade")
#' }
#' 
#' @importFrom magpiesets reporthelper summationhelper

calcValidTrade<-function(datasource="FAO", detail=T,nutrient="dm"){
  
  if(datasource=="FAO"){
    k_trade<-findset("k_trade")
    mb<-collapseNames(calcOutput("FAOmassbalance",aggregate = F)[,,nutrient])
    
    # exports
    mb<-collapseNames(mb[,,c("production")])-collapseNames(mb[,,"domestic_supply"])
    
    .f <- function(x,weight=NULL){
      y <- dimSums(x,dim=1)
      if(is.null(weight)){
        z<- y/length(getRegions(x))
      }else if(weight=="net_trade"){
        z <- abs(x)/dimSums(abs(x),dim=1) * y
        z[is.na(z)] <- 0
      }
      else{stop("no weight exits")}
      return(x-z)
    }
    
    out<-reporthelper(x=.f(mb),dim = 3.1,level_zero_name = "Trade|Net-Trade",detail = detail)
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
              description="Agricultural Net-Trade")
         )
}