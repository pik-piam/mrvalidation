#' @title       calcValidCostsAEI
#' @description calculates the validation data for irrigation investment costs
#'
#' @param datasource Datasource of validation data
#'
#' @return magpie object on country level, unit and description
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidCostsAEI")
#' }
#'
calcValidCostsAEI <- function(datasource = "IMPACT") {

  if (datasource == "IMPACT") {

    out <- readSource("IMPACTIrrigInvCosts", convert = TRUE)
    out <- add_dimension(out, dim = 3.2, add = "variable", nm = "Costs|AEI")
    out <- add_dimension(out, dim = 3.1, add = "model", nm = datasource)

    getNames(out) <- paste(getNames(out), "(million US$05/yr)", sep = " ")
    unit          <- "million US$05/yr"

  } else {
    stop("So far only IMPACT irrigation investment costs available!")
  }

  return(list(x           = out,
              weight      = NULL,
              unit        = unit,
              description = "AEI Costs"))
}
