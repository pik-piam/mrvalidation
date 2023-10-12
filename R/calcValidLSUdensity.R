#' @title calcValidLSUdensity
#' @description Calculates historical rangelands intensity use.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' calcOutput("ValidLSUdensity")
#' }
#'
calcValidLSUdensity <- function() {

  past <- findset("past")
  prod <- calcOutput("GrasslandBiomass", aggregate = FALSE)[, past, "range"]
  prod <- toolCountryFill(prod, fill = 0)

  # pasture areas
  area <- calcOutput("LUH2v2", landuse_types = "LUH2v2", cellular = FALSE, aggregate = FALSE)[, past, "range"]
  area <- toolCountryFill(area, fill = 0)

  yref <- calcOutput("GrasslandsYields", lpjml = "lpjml5p2_pasture", climatetype = "MRI-ESM2-0:ssp245",
                     subtype = "/co2/Nreturn0p5", # nolint
                     lsu_levels = c(seq(0, 2.2, 0.2), 2.5), past_mngmt = "mdef",
                     aggregate = FALSE)[, past, "range.rainfed"]
  yref <- collapseNames(yref)

  yrefWeights <- calcOutput("LUH2v2", landuse_types = "LUH2v2", cellular = TRUE, aggregate = FALSE)[, past, "range"]

  # aggregate cells to iso country level
  cell2iso <- data.frame(celliso = getItems(yref, 1, full = TRUE),
                         iso = getItems(yref, if (magclass::dimExists(1.3, yref)) 1.3 else 1.1, full = TRUE))
  yref <- toolAggregate(yref, rel = cell2iso, from = "celliso", to = "iso", weight = yrefWeights)

  yref <- toolCountryFill(yref, fill = 0)

  # Actual yields
  yact <- prod[, past, ] / area[, past, ]
  yact[is.nan(yact) | is.infinite(yact)] <- 0

  t <- yact[, past, ] / yref[, past, ]
  t[is.nan(t) | is.infinite(t)] <- 0
  t <- collapseNames(t)

  names(dimnames(t))[3] <- "scenario.model.variable"
  getNames(t) <- paste0("historical.MAgPIEOwn.Productivity|Livestock eq density|+|",
                        reportingnames("range"), " (LSU eq/ha)")

  return(list(x = t,
              weight = yref * area, # Xref
              unit = "-",
              description = paste("Historic Trends in managed pastures Land Use Intensity Tau",
                                  "based on FAO yield trends")))
}
