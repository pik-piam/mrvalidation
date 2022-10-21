#' @title calcValidGlobalSurfaceTemp
#' @description validation for the global surface temperature
#' @author Michael Crawford
#'
#' @return Global MAgPIE object
#'
#' @examples
#'
#' \dontrun{
#'    calcOutput("calcValidGlobalSurfaceTemp")
#' }
#'

calcValidGlobalSurfaceTemp <- function() {

  surfaceTemp <- readSource("GISTEMP")
  getNames(surfaceTemp, dim = "variable") <- "Global Surface Temperature"

  return(list(x = surfaceTemp,
              unit = "C",
              description = "Estimate of global surface temperature change, with 1951-1980 as the base period"))

}
