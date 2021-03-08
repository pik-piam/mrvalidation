#' @title calcValidTrade
#' @description calculates the validation data for trade of agricultural products
#' 
#' @param datasource Datasource of validation data. 
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient The nutrient in which the results shall be reported.
#' @param net_trade Net trade flows or total trade 
#' @param equalized numbers changed so that global production meets global demand (in reality different because of time-delay between exports and imports)
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

calcValidTrade<-function(datasource="FAO", detail=T,nutrient="dm", net_trade=TRUE, equalized = TRUE){
  
  if(net_trade){
    if(datasource=="FAO"){
      k_trade<-findset("k_trade")
      mb<-collapseNames(calcOutput("FAOmassbalance",aggregate = F)[,,nutrient])
      
      # exports
      mb<-collapseNames(mb[,,c("production")])-collapseNames(mb[,,"domestic_supply"])
      
      ### normalize trade for global production/demand mismatch
      .f <- function(x,weight=NULL){
        y <- dimSums(x,dim=1) # net global trade, so demand/supply mismatch
        if(is.null(weight)){ # absolute balanceflow for each country the same
          z <- y/length(getRegions(x))
        }else if(weight=="net_trade"){ # balanceflow proportional to countries im- or exports
          z <- abs(x)/dimSums(abs(x),dim=1) * y
          z[is.na(z)] <- 0
        }
        else{stop("no weight exits")}
        return(x-z)
      }
      
      if(equalized){
        normalized_trade = .f(mb)
      } else {
        normalized_trade = mb
      }
      
      out<-reporthelper(x=normalized_trade,dim = 3.1,level_zero_name = "Net-Trade",detail = detail)
      
    } else stop("No data exist for the given datasource!")
  } else {
    
    k_trade<-findset("k_trade")
    mb <- calcOutput("FAOmassbalance",aggregate = F)
    mb <- collapseNames(mb[,,nutrient])
    
    exports <- collapseNames(mb[,,c("export")])
    imports <- collapseNames(mb[,,c("import")])
    net_trade <- collapseNames(mb[,,c("production")])-collapseNames(mb[,,"domestic_supply"]-collapseNames(mb[,,"stock_variation"]))
    
    check <- exports-imports-net_trade
    message("inconsistencies in massbalances exist before year 2000")
    check_years=c("y2000", "y2005", "y2010")
    mismatch = unique(magclass::where(abs(check[,check_years,]) > 0.1)$true$individual[,3])
    if (length(mismatch>0)){
      message(paste(c("larger mismatch between absolute trade and net-trade for the products:",mismatch),collapse = " "))
    }

    if(equalized){ stop("no equalized absolute trade implemented yet. You should set equalized to FALSE")}
    
    out1<-reporthelper(exports,dim = 3.1,level_zero_name = "Exports",detail = detail)
    out2<-reporthelper(imports,dim = 3.1,level_zero_name = "Imports",detail = detail)
    out<-mbind(out1,out2)
  }
  
  out <- summationhelper(out,excludeLevels = 1)
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

  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="Agricultural Trade")
         )
}