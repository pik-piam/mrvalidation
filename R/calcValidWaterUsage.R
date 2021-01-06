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
#' \item \code{"cwatm_ipsl-cm5a-lr"}
#' \item \code{"cwatm_gfdl-esm2m"}
#' \item \code{"cwatm_miroc5"}
#' \item \code{"cwatm_hadgem2-es"}
#' \item \code{"lpjml_ipsl-cm5a-lr"}    
#' \item \code{"lpjml_gfdl-esm2m"}
#' \item \code{"lpjml_miroc5"}
#' \item \code{"lpjml_hadgem2-es"}
#' \item \code{"h08_ipsl-cm5a-lr"}    
#' \item \code{"h08_gfdl-esm2m"}
#' \item \code{"h08_miroc5"}
#' \item \code{"h08_hadgem2-es"}
#' \item \code{"matsiro_ipsl-cm5a-lr"}
#' \item \code{"matsiro_gfdl-esm2m"}    
#' \item \code{"matsiro_miroc5"}
#' \item \code{"matsiro_hadgem2-es"}
#' \item \code{"mpi-hm_ipsl-cm5a-lr"}
#' \item \code{"mpi-hm_gfdl-esm2m"}
#' \item \code{"mpi-hm_miroc5"}    
#' \item \code{"pcr-globwb_ipsl-cm5a-lr"}
#' \item \code{"pcr-globwb_gfdl-esm2m"}
#' \item \code{"pcr-globwb_miroc5"}
#' \item \code{"pcr-globwb_hadgem2-es"}
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
#' @author Stephen Wirth, Anne Biewald, Felicitas Beier
#' @importFrom magpiesets reportingnames
#' @import mrcommons 

calcValidWaterUsage <- function(datasource="shiklomanov_2000"){
  #land<-getNames("land")
  
  if (datasource %in% c("foley_2011","shiklomanov_2000", "wada_2011", "wisser_2008","fischer_IIASA","hejazi_2013", "molden_IWMI", "seckler_IWMI", "shiklomanov")){
    out <- readSource("WaterUsage", datasource, convert=F)
    if (datasource %in% c("wisser_2008","fischer_IIASA","hejazi_2013", "seckler_IWMI")){
      out <- out[,,"data"]
     # getNames(out) <- "Water|Withdrawal|Agriculture (million m3/yr)"
    }
      if (datasource %in% c("foley_2011","shiklomanov_2000", "wada_2011", "wisser_2008")){
      out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
      out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
      }
      else if (datasource %in% c("fischer_IIASA","hejazi_2013", "molden_IWMI", "seckler_IWMI", "shiklomanov","aquastat_2008_12")){
      out <- add_dimension(out, dim=3.1, add="scenario", nm="projection")
      out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
      }
    } else if (datasource %in% c("cwatm_ipsl-cm5a-lr",      "cwatm_gfdl-esm2m",        "cwatm_miroc5",            "cwatm_hadgem2-es",       
                                 "h08_ipsl-cm5a-lr",        "h08_gfdl-esm2m",          "h08_miroc5",              "h08_hadgem2-es",         
                                 "lpjml_ipsl-cm5a-lr",      "lpjml_gfdl-esm2m",        "lpjml_miroc5",            "lpjml_hadgem2-es",       
                                 "matsiro_ipsl-cm5a-lr",    "matsiro_gfdl-esm2m",      "matsiro_miroc5",          "matsiro_hadgem2-es",     
                                 "mpi-hm_ipsl-cm5a-lr",     "mpi-hm_gfdl-esm2m",       "mpi-hm_miroc5",      
                                 "pcr-globwb_ipsl-cm5a-lr", "pcr-globwb_gfdl-esm2m",   "pcr-globwb_miroc5",       "pcr-globwb_hadgem2-es" )){ 
       folder <- "ISIMIP2b:water.histsoc_airrww"
       out <- readSource("ISIMIPoutputs", subtype=paste(folder,datasource,sep="_"), convert=TRUE) 
       out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
       out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    } else {
    stop("Given datasource currently not supported!")
    }
  getNames(out, dim=3) <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"
  names(dimnames(out))[3] <- "scenario.model.variable"
  
  return(list(x=out,
              weight=NULL,
              unit="km^3",
              min=0,
              description="Agricultural waterusage from different sources in km^3"))
}
