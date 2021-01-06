#' @title calcValidEmisLucGloGasser
#' 
#' @description Returns historical LUC emissions
#' 
#' @param subtype Available subtypes are:
#'  \itemize{
#'  \item historical:
#'  \itemize{
#'  \item Gasser_2020
#'  \item LUH2_GCB_2019
#'  \item FRA_2015
#'  }
#'  }
#' @return list of magpie object with data and weight
#' @author Abhijeet Mishra
calcValidEmisLucGloGasser <- function(subtype="Gasser_2020") {
  
  if (subtype %in% c("Gasser_2020","LUH2_GCB_2019","FRA_2015")) {
    out <- readSource("Gasser",convert=TRUE)[,,subtype] * 1000 * 44/12 ## Conversion from Pg to Mt and C to CO2
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model",nm=subtype)
    getNames(out, dim=3) <- "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
    out <- mbind(setYears(out,"y2010"),setYears(out,"y2015"))
    names(dimnames(out))[3] <- "scenario.model.variable"
    weight <- collapseNames(setYears(readSource("FRA2020",subtype = "forest_area",convert = TRUE)[,"y2010","landArea"],NULL))
  } else stop("Invalid subtype. See function description for valid subtypes.")
  
  return(list(x=out,
              weight=weight,
              unit="Mt CO2/yr",
              min=-1000,
              description="Historical land-use change CO2 emissions from different sources in Mt CO2/yr")
  )
}
