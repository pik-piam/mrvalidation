#' @title calcValidCostsTC
#' @description calculates the validation data for TC as Ag R&D investments
#'
#' @param datasource Datasource of validation data.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#' calcOutput("ValidCostsTC")
#' }
#'
#' @import mrmagpie

#' @importFrom magpiesets reporthelper summationhelper
calcValidCostsTC<-function(datasource="Pardey"){

  if(datasource=="Pardey"){

    out <- readSource("PardeyAgRD",convert=TRUE)
    getNames(out) <- "Costs|TC"
    getNames(out) <- paste(getNames(out), "(million US$2017/yr)", sep=" ")
    unit <- "(million US$MER2017/yr)"


    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)

  }
  else{ stop("Only Pardey R&D costs avilable currently!")}

  return(list(x=out,
              weight=NULL,
              unit=unit,
              description="TC Costs")
  )
}
