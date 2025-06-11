#' @title calcValidFoodExpenditure
#' @description validation for foode expenditure
#'
#' @param detail if FALSE, only major food commoditiy groups are shown.
#' @param datasource Datasource for demand (FAO, FAOpre2010, FAOpost2010)
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("ValidFoodExpenditure")
#' }
#' @importFrom  magpiesets findset
calcValidFoodExpenditure <- function(detail=FALSE, datasource = "FAO") {
  
  price<-calcOutput("IniFoodPrice", datasource="FAO",aggregate = FALSE)
  pop <- collapseNames(calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[, , ])
  
    if (datasource == "FAO"){    
      demand<-dimSums(calcOutput("FAOmassbalance",aggregate = FALSE)[,,c("food","flour1")][,,"dm"],dim=c(3.2,3.3))
      demand<-demand[,,getNames(price)]   
    }else if(datasource == "FAOpre2010"){
      demand<-dimSums(calcOutput("FAOmassbalance",aggregate = FALSE,  version = "pre2010")[,,c("food","flour1")][,,"dm"],dim=c(3.2,3.3))
      demand<-demand[,,getNames(price)]  
     }else if(datasource == "FAOpost2010"){
      demand<-dimSums(calcOutput("FAOmassbalance",aggregate = FALSE,  version = "post2010")[,,c("food","flour1")][,,"dm"],dim=c(3.2,3.3))
      demand<-demand[,,getNames(price)]  
    } else {
      stop("unknown datasource")
      }

  demand <- demand[, , getNames(price)]
  pop <- pop[, getYears(demand), ]
  demandPc <- demand / pop[, getYears(demand), ]


  out <- demandPc * price
  out2 <- reporthelper(x = out,
                       level_zero_name = "Household Expenditure|Food|Expenditure",
                       detail = detail,
                       partly = TRUE)
  out2[is.nan(out2)] <- 0
  out2 <- add_dimension(out2, dim = 3.1, add = "scenario", nm = "historical")
  out2 <- add_dimension(out2, dim = 3.2, add = "model", nm = datasource)

  return(list(
    x = out2,
    weight = pop,
    unit = "US$2017/capita",
    description = "Per-capita expenditure for food"
  ))
}
