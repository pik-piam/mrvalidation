#' @title calcValidCostsOverall
#' @description Returns historical values of the overall value of production for the Agricultural, 
#' Forestry sectors, and fisheries (mio.05USD)
#' @param datasource datasource for validation. FAO Value of Production dataset.
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' calcOutput("calcValidCostsOverall")
#' }
#'
calcValidCostsOverall <- function(datasource = "FAO") {

  #### Value of Production for the agriculture, forestry and fisheries sector

  if (datasource == "FAO") {
  
  # Original costs in 2020 values. Conversion factor for US$D05
  x <- dimSums(calcOutput("VoPAFF", aggregate = FALSE), dim = 3.1) / (1 + 0.04)^15

  # Fraction of revenue

  rev <- readSource("TFPUSDA")[, , "revenue"]
  rev <- time_interpolate(rev, interpolated_year = c((getYears(rev, as.integer = TRUE) + 5)),
                    extrapolation_type = "constant", integrate_interpolated_years = TRUE)

  years <- intersect(getYears(x), getYears(rev))

  x <- x[, years, ] * (1 - rev[, years, ])

  getNames(x) <- paste0("Costs|Gross value of production (million US$05/yr)")
  x <- add_dimension(x, dim = 3.1, add = "scenario", nm = "historical")
  x <- add_dimension(x, dim = 3.2, add = "model", nm = datasource)

  weight <- NULL
  units <- "(million US$05/yr)"

  } else {
    warning("Only FAO datasource available")
  }

  return(list(x = x, weight = weight,
              unit = units,
              description = "Validation for overall costs indicators(million US$05/yr)"))

}
