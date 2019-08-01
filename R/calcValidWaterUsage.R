#' @title calcValidWaterUsage
#' 
#' @description Returns historical and projected water usage from different sources
#' 
#' @param datasource Currently available: 
#' \itemize{
#' \item historical:
#' \itemize{
#' \item \code{"foley_2011"}
#' \item \code{"shiklomanov_2000"}
#' \item \code{"wada_2011"}
#' \item \code{"wisser_2008"}
#' }
#' \item projections:
#' \itemize{
#' \item \code{"fischer_IIASA"}
#' \item \code{"hejazi_2013"}
#'  \item \code{molden_IWMI}
#'  \item \code{seckler_IWMI}
#'  \item \code{shiklomanov}
#' }
#' }
#' @return list of magpie object with data and weight
#' @author Stephen Wirth, Anne Biewald
#' @importFrom magpiesets reportingnames
calcValidWaterUsage <- function(datasource="shiklomanov_2000"){
  #land<-getNames("land")
  
  if(datasource %in% c("foley_2011","shiklomanov_2000", "wada_2011", "wisser_2008","fischer_IIASA","hejazi_2013", "molden_IWMI", "seckler_IWMI", "shiklomanov"))
     {
    out <- readSource("WaterUsage", datasource, convert=F)
    if(datasource %in% c("wisser_2008","fischer_IIASA","hejazi_2013", "seckler_IWMI")){
      out <- out[,,"data"]
     # getNames(out) <- "Water|Withdrawal|Agriculture (million m3/yr)"
    }
      if(datasource %in% c("foley_2011","shiklomanov_2000", "wada_2011", "wisser_2008"))
      {
      out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
      out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
      }
      else if(datasource %in% c("fischer_IIASA","hejazi_2013", "molden_IWMI", "seckler_IWMI", "shiklomanov","aquastat_2008_12")){
      out <- add_dimension(out, dim=3.1, add="scenario", nm="projection")
      out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
      }
    }else {
    stop("Given datasource currently not supported!")
    }
  getNames(out, dim=3) <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"
  names(dimnames(out))[3] <- "scenario.model.variable"
  
  return(list(x=out,
              weight=NULL,
              unit="km^3",
              min=0,
              description="Agricultural waterusage from different sources in km^3"
                )
  )
}
