#' calcValidPopulation
#'
#' Returns historical development of population and future projections of population dynamics
#'
#' @param pastData population past data source
#' @param futureData population future data source
#' @return list of magpie object with data and weight
#' @author Florian Humpenoeder, Jan Philipp Dietrich, David Chen
calcValidPopulation <- function(pastData = "WDI-UN_PopDiv-MI", futureData = "SSPs-UN_PopDiv") {

  past   <- calcOutput("PopulationPast", pastData = pastData, aggregate = FALSE)
  future <- calcOutput("PopulationFuture", futureData = futureData, aggregate = FALSE)

  getNames(past) <- paste("historical", pastData, "Population (million people)", sep = ".")
  getNames(future) <- paste(getNames(future), futureData, "Population (million people)", sep = ".")
  getSets(past)[3] <- "scenario.model.variable"
  getSets(future)[3] <- "scenario.model.variable"

  out <- new.magpie(cells_and_regions = getItems(past, 1),
                    years = union(getYears(past), getYears(future)),
                    names = c(getNames(past), getNames(future)),
                    sets = c("region", "year", "scenario.model.variable"),
                    fill = NA)
  out[, getYears(past), getNames(past)] <- past
  out[, getYears(future), getNames(future)] <- future

  list(x = out,
       weight = NULL,
       unit = "million",
       description = paste0("Population data. Datasource for the Past: ", pastData, ". Datasource for the Future: ",
                            futureData))
}
