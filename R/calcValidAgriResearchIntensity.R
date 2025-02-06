#' @title calcValidAgriResearchIntensity
#' @description calculates the validation data for TC as Ag R&D investments
#'
#' @param datasource Datasource of validation data.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David Chen
#' @examples
#' \dontrun{
#' calcOutput("calcValidAgriResearchIntensity")
#' }
#'
#' @import mrmagpie

#' @importFrom magclass time_interpolate

calcValidAgriResearchIntensity <- function(datasource = "Pardey") {

  if (datasource == "Pardey") {

    agRD <- readSource("PardeyAgRD", convert = TRUE)


    gdp <- collapseNames(calcOutput("GDP", unit = "constant 2005 US$MER", scenario = "SSP2", aggregate = FALSE))

    cyears <- intersect(getYears(agRD), getYears(gdp))
    out <- agRD[, cyears, ] / gdp[, cyears, ] * 100 # nolint


    # interpolate 1995 value
    out <- time_interpolate(out, interpolated_year = 1995, integrate_interpolated_years = TRUE)

    unit <- "(% of Total GDP)"

    getNames(out) <- paste("Agricultural Research Intensity", "(% of Total GDP)", sep = " ")

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    weight <- gdp[, getYears(out), ]
  } else {
    stop("Only Pardey R&D costs avilable currently!")
  }

  return(list(x = out,
              weight = weight,
              unit = unit,
              description = "Agricultural Research Intensity")
  )
}
