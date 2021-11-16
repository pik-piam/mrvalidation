#' calcValidPopulation
#'
#' Returns historical development of population and future projections of population dynamics
#'
#'
#' @param PopulationPast population past data source
#' @param PopulationFuture population future data source
#' @param TimeFromFindSet boolean deciding something
#' @return list of magpie object with data and weight
#' @author Florian Humpenoeder, Jan Philipp Dietrich, David Chen
#' @importFrom magclass getRegions
#' @import mrdrivers
calcValidPopulation <- function(PopulationPast   = "WDI-UN_PopDiv-MI",
                                PopulationFuture = c("SSPs-UN_PopDiv-MI",
                                                     "SDPs-UN_PopDiv-MI",
                                                     "SSP2EU-UN_PopDiv-MI"),
                                TimeFromFindSet = TRUE) {

  past   <- calcOutput("PopulationPast",
                       PopulationPast = PopulationPast,
                       aggregate = FALSE)
  getNames(past) <- paste("historical", paste0(PopulationPast), "Population (million people)", sep = ".")
  getSets(past)[3] <- "scenario.model.variable"

  future <- NULL
  for (i in 1:length(PopulationFuture)) {
  tmp <- calcOutput("PopulationFuture",
                    PopulationFuture = PopulationFuture[i],
                    extension2150 = "bezier",
                    aggregate = FALSE)
  future <- mbind(future, tmp)
  }


  getNames(future) <- paste(sub("^pop_", "", getNames(future)),
                            paste0(PopulationFuture, collapse = "_and_"),
                            "Population (million people)",
                            sep = ".")
  getSets(future)[3] <- "scenario.model.variable"

  out <- new.magpie(getRegions(past),
                    union(getYears(past), getYears(future)),
                    c(getNames(past), getNames(future)),
                    fill = NA)
  out[, getYears(past), getNames(past)] <- past
  out[, getYears(future), getNames(future)] <- future

  getSets(out, fulldim = FALSE)[3] <- "scenario.model.variable"
  return(list(x = out, weight = NULL, unit = "million",
              description = paste0("Population data. Datasource for the Past: ",
                                 PopulationPast,
                                 ". Datasource for the Future: ",
                                 paste0(PopulationFuture, collapse = ", "))))
}
