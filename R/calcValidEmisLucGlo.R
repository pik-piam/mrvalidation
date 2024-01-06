#' @title calcValidEmisLucGlo
#'
#' @description Returns historical and projected water usage from different sources
#'
#' @param subtype Available subtypes are:
#'  \itemize{
#'  \item historical:
#'  \itemize{
#'  \item Canadell_2007
#'  \item Friedlingstein_2010
#'  \item Harris_2013
#'  \item Houghton_2012
#'  \item RCP
#'  }
#'  }
#' @return list of magpie object with data and weight
#' @author Florian Humpenoeder
calcValidEmisLucGlo <- function(subtype = "Canadell_2007") {

  if (subtype %in% c("Canadell_2007", "Friedlingstein_2010", "Harris_2013", "Houghton_2012", "RCP")) {
    out <- readSource("EmisLucGlo", subtype, convert = FALSE)
    if (subtype %in% c("Canadell_2007", "Friedlingstein_2010", "Harris_2013", "Houghton_2012", "RCP")) {
      out <- out[, , "data"] * (44 / 12) ## Original Data in C, We validate as CO2
    }
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = subtype)
    getNames(out, dim = 3) <- "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
    names(dimnames(out))[3] <- "scenario.model.variable"
  } else {
    stop("Given subtype currently not supported!")
  }

  return(list(x = out,
              weight = NULL,
              unit = "Mt CO2/yr",
              min = 0,
              description = "Historical land-use change CO2 emissions from different sources in Mt CO2/yr")
  )
}
