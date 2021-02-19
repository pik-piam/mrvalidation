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
#' \item \code{"CWatM:ipsl-cm5a-lr"}
#' \item \code{"CWatM:gfdl-esm2m"}
#' \item \code{"CWatM:miroc5"}
#' \item \code{"CWatM:hadgem2-es"}
#' \item \code{"LPJmL:ipsl-cm5a-lr"}    
#' \item \code{"LPJmL:gfdl-esm2m"}
#' \item \code{"LPJmL:miroc5"}
#' \item \code{"LPJmL:hadgem2-es"}
#' \item \code{"H08:ipsl-cm5a-lr"}    
#' \item \code{"H08:gfdl-esm2m"}
#' \item \code{"H08:miroc5"}
#' \item \code{"H08:hadgem2-es"}
#' \item \code{"MATSIRO:ipsl-cm5a-lr"}
#' \item \code{"MATSIRO:gfdl-esm2m"}    
#' \item \code{"MATSIRO:miroc5"}
#' \item \code{"MATSIRO:hadgem2-es"}
#' \item \code{"MPI-HM:ipsl-cm5a-lr"}
#' \item \code{"MPI-HM:gfdl-esm2m"}
#' \item \code{"MPI-HM:miroc5"}    
#' \item \code{"PCR-GLOBWB:ipsl-cm5a-lr"}
#' \item \code{"PCR-GLOBWB:gfdl-esm2m"}
#' \item \code{"PCR-GLOBWB:miroc5"}
#' \item \code{"PCR-GLOBWB:hadgem2-es"}
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
#' @importFrom magclass collapseDim dimSums
#' @importFrom madrat calcOutput readSource

calcValidWaterUsage <- function(datasource="shiklomanov_2000"){
  #land<-getNames("land")
  
  if (datasource %in% c("foley_2011","shiklomanov_2000", "wada_2011", "wisser_2008","fischer_IIASA","hejazi_2013", "molden_IWMI", "seckler_IWMI", "shiklomanov")){
    out <- readSource("WaterUsage", datasource, convert=F)
    if (datasource %in% c("wisser_2008","fischer_IIASA","hejazi_2013", "seckler_IWMI")){
      out <- out[,,"data"]
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
    } else if (datasource %in% c("CWatM:ipsl-cm5a-lr",      "CWatM:gfdl-esm2m",        "CWatM:miroc5",            "CWatM:hadgem2-es",       
                                 "H08:ipsl-cm5a-lr",        "H08:gfdl-esm2m",          "H08:miroc5",              "H08:hadgem2-es",         
                                 "LPJmL:ipsl-cm5a-lr",      "LPJmL:gfdl-esm2m",        "LPJmL:miroc5",            "LPJmL:hadgem2-es",       
                                 "MATSIRO:ipsl-cm5a-lr",    "MATSIRO:gfdl-esm2m",      "MATSIRO:miroc5",          "MATSIRO:hadgem2-es",     
                                 "MPI-HM:ipsl-cm5a-lr",     "MPI-HM:gfdl-esm2m",       "MPI-HM:miroc5",      
                                 "PCR-GLOBWB:ipsl-cm5a-lr", "PCR-GLOBWB:gfdl-esm2m",   "PCR-GLOBWB:miroc5",       "PCR-GLOBWB:hadgem2-es" )) {
      out <- readSource("ISIMIP", subtype=paste("airww",datasource,"2b",sep=":"), convert=TRUE) 
      
      # unit transformation: from: kg m-2 s-1 = mm/second, to: mm/month
      # (Note: 1 day = 60*60*24 = 86400 seconds)
      dayofmonths    <- as.magpie(c(jan=31,feb=28,mar=31,apr=30,may=31,jun=30,jul=31,aug=31,sep=30,oct=31,nov=30,dec=31), temporal = 1)
      out      <- out * dayofmonths * 86400
      # mm/month -> mm/year
      out      <- collapseDim(toolAggregate(out, data.frame(getItems(out,"month"),"year"), dim="month"))
      # mm/year = liter/m^2/year -> liter/year -> mio m^3/year
      landarea <- dimSums(calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="magpiecell", irrigation=FALSE, years="y1995"), dim=3)
      names(dimnames(landarea))[1] <- "iso.cell"
      landarea                     <- toolAggregate(landarea, data.frame("cell"=getCells(landarea), "iso"=gsub("\\..*","",getCells(landarea)), stringsAsFactors=F), dim=1)
      landarea                     <- toolCountryFill(landarea, fill=0)
      out      <- out * landarea * 10^-9
      
      out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
      out <- add_dimension(out, dim=3.2, add="model",    nm=datasource)
    } else {
      stop("Given datasource currently not supported!")
    }
  getNames(out, dim=3)    <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"
  names(dimnames(out))[3] <- "scenario.model.variable"
  
  return(list(x=out,
              weight=NULL,
              unit="km^3",
              min=0,
              description="Agricultural waterusage from different sources in km^3"))
}
