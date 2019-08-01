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
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @importFrom magclass getRegions
calcValidPopulation <- function(PopulationCalib="transition", PopulationPast="WDI", PopulationFuture="SSP", TimeFromFindSet = TRUE ) {
  past   <- calcOutput("PopulationPast",PopulationPast=PopulationPast, aggregate = FALSE)
  getNames(past) <- paste("historical",PopulationPast,"Population (million people)",sep=".")
  getSets(past)[3] <- "scenario.model.variable"
  
  future <- calcOutput("PopulationFuture",PopulationFuture=PopulationFuture, aggregate = FALSE)
  getNames(future) <- paste(sub("^pop_","",getNames(future)),PopulationFuture,"Population (million people)",sep=".")
  getSets(future)[3] <- "scenario.model.variable"
  
  out <- new.magpie(getRegions(past),union(getYears(past),getYears(future)),c(getNames(past),getNames(future)),fill = NA)
  out[,getYears(past),getNames(past)] <- past
  out[,getYears(future),getNames(future)] <- future
  getSets(out,fulldim = FALSE)[3] <- "scenario.model.variable"
  return(list(x=out, weight=NULL, unit="million",
              description=paste0("Population data. Datasource for the Past: ",
                                 PopulationPast,
                                 ". Datasource for the Future: ",
                                 PopulationFuture)))
}

