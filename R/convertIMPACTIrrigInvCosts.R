#' @title       convertIMPACTIrrigInvCosts
#' @description converts units and dimensions of average annual baseline
#'              water-related investment cost data from readIMPACTIrrigInvCosts
#'
#' @param x MAgPIE object containing irrigation investment cost data on region level
#'
#' @return magpie object containing average annual baseline water-related
#'         investment cost on country-level
#'         (in million 2017 USD per year)
#'
#' @author Felicitas Beier
#'
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("IMPACTIrrigInvCosts", convert = TRUE)
#' }
#'
#' @importFrom madrat toolAggregate toolCountryFill
#' @importFrom magclass getYears getNames getRegions setYears new.magpie mbind where
#' @importFrom GDPuc convertGDP

convertIMPACTIrrigInvCosts <- function(x) {

  # convert from bio. to mio. USD
  x <- x * 1000

  # Disaggregate to iso-countries
  # weight for disaggregation: GDP
  w <- collapseNames(setYears(calcOutput("GDP", aggregate = FALSE)[, "y2020", "gdp_SSP2"], NULL))

  # IMPACT country-region-mapping
  mapping <- read.csv("regionmapping_IMPACT.csv")
  mapping <- data.frame(region = mapping$Region_Code, iso = mapping$ISO_code)
  mapping <- rbind(mapping, data.frame(region = "EUR", iso = "ATA"))
  mapping <- rbind(mapping, data.frame(region = "EUR", iso = "JEY"))
  mapping <- mapping[which(mapping$iso != setdiff(mapping$iso, getCells(w))), ]

  tmp <- new.magpie(cells_and_regions = setdiff(mapping$region, getCells(x)),
                    years = getYears(x),
                    names = getNames(x),
                    fill = 0)
  x <- mbind(x, tmp)
  x <- x[intersect(mapping$region, getCells(x)), , ]

  x <- toolAggregate(x, rel = mapping, weight = w, from = "region", to = "iso", dim = 1)

  # convert 2000 to 2017 USD
  tmp <- convertGDP(x,
                    unit_in = "constant 2000 US$MER",
                    unit_out = "constant 2017 US$MER",
                    replace_NAs = "no_conversion")

  x <- tmp
  ### for missing countries use DEU rate for now ###

  # fill missing countries with 0
  x <- toolCountryFill(x, fill = 0)

  return(x)
}
