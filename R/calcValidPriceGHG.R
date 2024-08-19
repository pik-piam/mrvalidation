#' @title calcValidPriceGHG
#' @description Validates MAgPIE GHG emission price input against SSP GHG emission price projections
#' @param datasource Currently available: \code{"SSPResults"}
#'
#' @author Amsalu W. Yalew, Benjamin Leon Bodirsky, Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("ValidPriceGHG")
#' }
#'
#' @importFrom magclass fulldim

calcValidPriceGHG <- function(datasource = "SSPResults") {

  if (datasource == "SSPResults") {

    # read the file from SSPResults
    sspr <- calcOutput("ValidSSPResults", warnNA = FALSE, aggregate = FALSE)

    # extract the carbon price
    co2p <- sspr[, , "Price|Carbon (US$2017/t CO2)"]
    getNames(co2p, dim = 3) <- "Prices|GHG Emission|CO2 (US$2017/tCO2)"


    # Calculate N20 price based on Carbon price (* by GWP factor 265)
    n2op <- co2p * 265
    getNames(n2op, dim = 3) <- "Prices|GHG Emission|N2O (US$2017/tN2O)"


    # Calcualte CH4 price based on Carbon price (* by GWP factor 265)
    ch4p <- co2p * 28
    getNames(ch4p, dim = 3) <- "Prices|GHG Emission|CH4 (US$2017/tCH4)"


    # merging
    out  <- mbind(co2p, n2op, ch4p)

    # weights
    co2Weight <- setNames(readSource(type = "EDGAR_LU", subtype = "CO2"), "Prices|GHG Emission|CO2 (US$2017/tCO2)")
    n2oWeight <- setNames(readSource(type = "EDGAR_LU", subtype = "N2O"), "Prices|GHG Emission|N2O (US$2017/tN2O)")
    ch4Weight <- setNames(readSource(type = "EDGAR_LU", subtype =  "CH4"), "Prices|GHG Emission|CH4 (US$2017/tCH4)")
    edgar     <- mbind(co2Weight, n2oWeight, ch4Weight)


    weight <- out
    weight[, , ] <- 10^-10
    # loop - for each model (of the SSPS) set the weights.This avoids the mismatch problem
    for (model in getItems(out, dim = "model")) {
      weight[, , model] <- weight[, , model] + setYears(edgar[, "y2005", ], NULL)
    }


  } else {
stop("unknown data source")
}

  return(list(x = out,
              weight = weight,
              unit = "US$ 2017 per ton CO2, N2O, CH4",
              description = "CO2, N2O, and CH4 GHG emission price validation based on SSP results"))

}
