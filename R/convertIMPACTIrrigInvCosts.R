#' @title       convertIMPACTIrrigInvCosts
#' @description converts units and dimensions of average annual baseline
#'              water-related investment cost data from readIMPACTIrrigInvCosts
#'
#' @param x MAgPIE object containing irrigation investment cost data on region level
#'
#' @return magpie object containing average annual baseline water-related
#'         investment cost on country-level
#'         (in million 2005 USD per year)
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
  w <- collapseNames(setYears(calcOutput("GDPppp", GDPpppFuture = "SRES_SSP_completed",
                                          naming = "indicator.scenario", aggregate = FALSE)[, "y2020", "SSP2"],
                              NULL))

  # IMPACT country-region-mapping
  mapping <- read.csv("regionmapping_IMPACT.csv")
  mapping <- data.frame(region = mapping$Region_Code, iso = mapping$ISO_code)
  mapping <- rbind(mapping, data.frame(region = "EUR", iso = "ATA"))
  mapping <- rbind(mapping, data.frame(region = "EUR", iso = "JEY"))
  mapping <- mapping[which(mapping$iso != setdiff(mapping$iso, getRegions(w))), ]

  tmp <- new.magpie(cells_and_regions = setdiff(mapping$region, getRegions(x)),
                    years = getYears(x),
                    names = getNames(x),
                    fill = 0)
  x <- mbind(x, tmp)
  x <- x[intersect(mapping$region, getCells(x)), , ]

  x <- toolAggregate(x, rel = mapping, weight = w, from = "region", to = "iso", dim = 1)

  # convert 2000 to 2005 USD
  tmp <- convertGDP(x, unit_in = "constant 2000 US$MER", unit_out = "constant 2005 US$MER")

  ### for missing countries use DEU rate for now ###
  tmp[where(is.na(tmp))$true$regions, , ] <- x[where(is.na(tmp))$true$regions, , ] *
                                             setYears(tmp["DEU", "y2020", ] / x["DEU", "y2020", ],
                                                      NULL)
  x <- tmp
  ### for missing countries use DEU rate for now ###

  # fill missing countries with 0
  x <- toolCountryFill(x, fill = 0)

  return(x)
}
