#' calcValidCroparea
#' 
#' Returns historical areas of individual crops. These are derived by
#' correcting harvested areas to match to physical cropland areas. Both these
#' datasets are from FAO. Output is meant to be used for model validation.
#' 
#' 
#' @param datasource Currently only "FAO" available
#' @param detail how much detail?
#' @return list of magpie object with data and weight
#' @author Ulrich Kreidenweis
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass getNames
calcValidCroparea <- function(datasource="FAO", detail=F) {

  if(datasource=="FAO"){
    data <- calcOutput("Croparea",sectoral="kcr",physical=TRUE,aggregate = FALSE)
    out <- reporthelper(x=data, dim=3.1, level_zero_name = "Resources|Land Cover|Cropland", detail = detail)
    out <- summationhelper(out)
    getNames(out) <- paste(getNames(out),"(million ha)",sep=" ")
    
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  } else stop("No data exist for the given datasource!")
  
  return(list(x=out,
              weight=NULL,
              unit="million ha",
              description="")
             )
}
