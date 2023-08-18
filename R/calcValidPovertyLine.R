#' @title calcValidPovertyLine
#' @description Validation of poverty (head-count)
#' @param datasource datasource for validation. Option: "WBPoverty" (world bank).
#' @param subtype Poverty line criteria. Options:"320PovertyLine" (default), "550PovertyLine", "1p90 USDppp11/day"
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @importFrom magclass collapseNames
#' @examples
#' \dontrun{
#' calcOutput("ValidPovertyLine")
#' }
#'
calcValidPovertyLine <- function(datasource = "WBPoverty", subtype = "320PovertyLine") {

  rawPer <- readSource("WBPoverty", subtype = subtype)
  rawPer <- rawPer[, getYears(rawPer, as.integer = TRUE) >= 1995, ]
  rawPer[!is.finite(rawPer) | rawPer == 9999] <- 0

  mapping <- toolGetMapping("regionmappingFSEC.csv", "regional", where = "mrvalidation")[, 2:3]

  colnames(mapping) <- c("Country", "Region")
  population <- calcOutput(type = "PopulationPast", aggregate = FALSE)
  years <- intersect(getYears(population, as.integer = TRUE), getYears(rawPer, as.integer = TRUE))
  rawPer <- rawPer[, years, ]

  # Fill interpolating values when there are 2 or more points reported
  for (i in getCells(rawPer)) {
    if (length(where(rawPer[i, , ] != 0)$true$years) >= 2 &&
        length(where(rawPer[i, , ] != 0)$true$years) < length(getYears(rawPer))) {
      yrInter <- where(rawPer[i, , ] == 0)$true$years
      rawPer[i, yrInter, ] <- time_interpolate(rawPer[i, , ][, yrInter, , invert = TRUE], yrInter)
    }
  }

  # Fill with regional averages
  past <- population
  past[!is.finite(rawPer) | rawPer == 0] <- 0
  regAvg <- toolAggregate(rawPer, rel = mapping, weight = past[, years, ], from = "Country", to = "Region")

  regAvg <- as.data.frame(regAvg)
  regAvg <- merge(regAvg, mapping, by = "Region")


  for (i in getCells(rawPer)) {
    for (y in years) {
      rawPer[i, y, ] <- if (rawPer[i, y, ] == 0) regAvg[regAvg$Year == y &
                                                          regAvg$Country == i, "Value"] else rawPer[i, y, ]
    }
  }



  out <- magpiesort(setNames((round(rawPer * population[, years, ] * 1e6, digits = 0)) / 1e6, NULL))
  nameScenario <- if (subtype == "190PovertyLine") "1p90 USDppp11/day" else if (subtype == "320PovertyLine")
    "3p20 USDppp11/day" else if (subtype == "550PovertyLine")
      "5p50 USDppp11/day"

  getNames(out) <- paste0("Income|Number of People Below ", nameScenario)
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = NULL,
              unit = "million people",
              description = paste0("Head-count of people living with income below ", nameScenario)))
}
