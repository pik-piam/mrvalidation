#' calcValidPopulation
#' 
#' Returns historical development of population and future projections of population dynamics
#' 
#' 
#' @param PopulationCalib to what should be calibrated? past, future or a transition?
#' @param PopulationPast population past data source
#' @param PopulationFuture population future data source
#' @param TimeFromFindSet boolean deciding something
#' @return list of magpie object with data and weight
#' @author Florian Humpenoeder, Jan Philipp Dietrich, David Chen
#' @importFrom magclass getRegions
calcValidPopulation <- function(PopulationCalib=c("past_grPEAP_grFuture", "Ariadne"), PopulationPast=c("WDI_completed", "Eurostat_WDI_completed"), PopulationFuture=c("SSP2018Update_completed_bezierOut", "SSP2Ariadne_completed_bezierOut"), TimeFromFindSet = TRUE ) {
 
  past   <- calcOutput("PopulationPast",PopulationPast=PopulationPast[1], aggregate = FALSE)
  getNames(past) <- paste("historical",paste0(PopulationPast[1]),"Population (million people)",sep=".")
  getSets(past)[3] <- "scenario.model.variable"
  
  future <- NULL
  for(i in 1:length(PopulationFuture)){
  tmp <- calcOutput("PopulationFuture",PopulationFuture=PopulationFuture[i], aggregate = FALSE)
  future <- mbind(future, tmp)
  }
  
  # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1
  if("pop_SSP1" %in% getNames(future) && !("pop_SDP" %in% getNames(future))){
    combined_SDP <- future[,, "pop_SSP1"]
    for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
      getNames(combined_SDP) <- gsub("SSP1", i, getNames(future[,, "pop_SSP1"]))
      future<- mbind(future, combined_SDP) 
    }
  }  
  
  
  getNames(future) <- paste(sub("^pop_","",getNames(future)),paste0(PopulationFuture, collapse = "_and_"),"Population (million people)",sep=".")
  getSets(future)[3] <- "scenario.model.variable"
  
  out <- new.magpie(getRegions(past),union(getYears(past),getYears(future)),c(getNames(past),getNames(future)),fill = NA)
  out[,getYears(past),getNames(past)] <- past
  out[,getYears(future),getNames(future)] <- future

  
  
  getSets(out,fulldim = FALSE)[3] <- "scenario.model.variable"
  return(list(x=out, weight=NULL, unit="million",
              description=paste0("Population data. Datasource for the Past: ",
                                 paste0(PopulationPast, collapse=", "),
                                 ". Datasource for the Future: ",
                                 paste0(PopulationFuture, collapse = ", "))))
}

