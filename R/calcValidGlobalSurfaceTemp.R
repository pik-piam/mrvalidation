#' @title calcValidGlobalSurfaceTemp
#' @description validation for the global surface temperature anomaly
#' @author Michael Crawford
#'
#' @return global MAgPIE object
#'
#' @examples
#'
#' \dontrun{
#'    calcOutput("ValidGlobalSurfaceTemp", aggregate = FALSE)
#' }
#'

calcValidGlobalSurfaceTemp <- function() {

    surfaceTemp <- readSource("AR6SPMFig1")

    getNames(surfaceTemp, dim = "variable") <- "Global Surface Temperature"

    return(list(x           = surfaceTemp,
                unit        = "C",
                description = "Estimate of global surface temperature anomaly, with 1850-1900 
                used as the base period."))

}
