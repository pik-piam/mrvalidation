#' @title calcValidAAI
#'
#' @description Returns historical area actually irrigated.
#'
#' @param datasource Currently available: \code{"LUH2v2"} and \code{"GMIA"}
#' @return list of magpie object with data and weight
#' @author Stephen Wirth, Anne Biewald
#' @importFrom magpiesets reportingnames
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass dimSums add_dimension

calcValidAAI <- function(datasource = "LUH2v2") {

  if (datasource == "LUH2v2") {

    out <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = TRUE,
                      cellular = FALSE, selectyears = "past", aggregate = FALSE)
    out <- collapseNames(out[, , c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")][, , "irrigated"])

    out <- dimSums(out, dim = 3)

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)
  } else if (datasource == "GMIA") {
    out <- calcOutput("GMIA", aggregate = FALSE)[, , "AAI_ha_"] / 10^6

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)
  } else {
    stop("Given datasource currently not supported!")
  }
  getNames(out, dim = 3)  <- paste("Resources|Land Cover|Cropland|Area actually irrigated",
                                   "(million ha)", sep = " ")
  names(dimnames(out))[3] <- "scenario.model.variable"

  return(list(x = out,
              weight = NULL,
              unit = "million ha",
              min = 0,
              description = "Area actually irrigated in Mha")
  )
}
