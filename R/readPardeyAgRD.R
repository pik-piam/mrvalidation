#' @title readPardeyAgRD
#' @description Agricultural R&D investment data read from
#' https://www.nature.com/news/agricultural-rd-is-on-the-move-1.20571 3 tables are
#' read in: AgRD_Pardey is public Ag expenditure in 1960 and 2011, extracted from
#' the interactive figure in the article that has more complete countries agGERD
#' and agPERD are total and public expenditures respectively, for less countries
#' but more years
#' @return magpie object containing expenditure in Ag R&D, 2009 USD PPP
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("PardeyAgRD")
#' }
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom madrat toolCountry2isocode
#' @importFrom GDPuc convertGDP
#' @importFrom magclass magpply as.data.frame


readPardeyAgRD <- function() {

  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "madrat")

  ### perd data only public investment
  perd <- read.csv("agPERD.csv")
  colnames(perd)[1] <- "Region"
  colnames(perd) <- gsub("X", "", colnames(perd))

  perd <- pivot_longer(perd, cols = c(2:length(colnames(perd))), names_to = "Year", values_to = "PERD")
  # convert from billions to million USD
  perd$PERD <- perd$PERD * 1000 # nolint
  # separate out the countries from the aggregates for using the aggregates later
  perdc <- perd
  perdc$Region <- toolCountry2isocode(perdc$Region, ignoreCountries = c("EastSouth Asia and Pacific", "High Income", "LAC", "Low Income", "Lower Middle Income", "MENA", "SSA", "Lower Middle", "Upper Middle", "Upper Middle Income", "World Total")) # nolint
  perdc <- perdc[!is.na(perdc$Region), ]
  perdc <- as.magpie(perdc, spatial = 1, temporal = 2, tidy = TRUE)
  perd <- as.magpie(perd, spatial = 1, temporal = 2, tidy = TRUE)

  ### GERD data total public and private
  gerd <- read.csv("agGERD.csv")
  colnames(gerd)[1] <- "Region"
  colnames(gerd) <- gsub("X", "", colnames(gerd))
  gerd <- pivot_longer(gerd, cols = c(2:length(colnames(gerd))), names_to = "Year", values_to = "GERD")
  # convert from billions to million USD
  gerd$GERD <- gerd$GERD * 1000 # nolint
  # separate out the countries from the aggregates for using the aggregates later
  gerdc <- gerd
  gerdc$Region <- toolCountry2isocode(gerdc$Region, ignoreCountries = c("East/South Asia and Pacific", "High Income", "LAC", "Low Income", "Lower Middle Income", "MENA", "SSA", "Lower Middle", "Upper Middle", "Upper Middle Income", "World Total"))  # nolint
  gerdc <- gerdc[!is.na(gerdc$Region), ]
  gerdc <- as.magpie(gerdc, spatial = 1, temporal = 2, tidy = TRUE)
  gerd <- as.magpie(gerd, spatial = 1, temporal = 2, tidy = TRUE)

  ### Get ratio of private/public investment
  getItems(gerd, dim = 1) <- getItems(perd, dim = 1)
  getItems(gerdc, dim = 1) <- getItems(perdc, dim = 1)
  ratio <- gerd / perd
  ratioc <- gerdc / perdc
  # countries for which GERD data exists
  countries <- getItems(ratioc, dim = 1)

  ### Read data with more complete countries and bump PERD up to GERD based on regional ratios
  full <- read.csv("AgRD_Pardey.csv")
  colnames(full)[c(3, 4)] <- substr(colnames(full)[c(3, 4)], 5, 8)
  full <- pivot_longer(full[, -2], cols = c(2, 3), names_to = "Year", values_to = "PERD")
  full$id <- as.character(full$id)
  full$id[c(61, 62)] <- "Sao Tome and Principe"
  full$id[c(27, 28)] <- "Cote d Ivoire"
  full$id[c(71, 72)] <- "Sudan"

  full$id <- toolCountry2isocode(full$id)
  full <- as.magpie(full, spatial = 1, temporal = 2)

  # bind GERD for countries that have the data, assume ratio same as in 1960

  fullgerd1960 <- full[countries, 1960, ] * setYears(ratioc[, 1980, ], NULL)
  fullgerd <- mbind(collapseNames(fullgerd1960, collapsedim = 1), gerdc)

  #### multiply the PERD data by the 2011 and 1980 GERD/PERD ratios for the smaller countries, based on regional share

  # rename regions to match our mapping
  getItems(ratio, dim = 1)[which(getItems(ratio, dim = 1) == "EastSouth Asia and Pacific")] <- "OAS"
  getItems(ratio, dim = 1)[which(getItems(ratio, dim = 1) == "LAC")] <- "LAM"
  getItems(ratio, dim = 1)[which(getItems(ratio, dim = 1) == "MENA")] <- "MEA"

  # make regional aggregations and apply other regions to EUR has available countries for mean,
  # SPain 1980 looks like outlier
  missingRegions <- setdiff(unique(mapping$RegionCode), getItems(ratio, 1))

  ratioMissing <- new.magpie(cells_and_regions = missingRegions,
                             years = getYears(ratio),
                             names = getNames(ratio))
  ratioMissing["EUR", , ] <- dimSums(ratio[c("Germany", "France", "Italy", "Spain"), , ], dim = 1) / 4
  ratioMissing[c("NEU", "REF"), , ] <- ratio["Upper Middle", , ]
  ratioMissing["CAZ", , ] <- ratio["High Income", , ]

  ratio <- mbind(ratio, ratioMissing[where(is.na(ratioMissing))$true$regions, , invert = TRUE])

  # map smaller countries to region
  yCountries <- as.data.frame(full[countries, , invert = TRUE])[, -1]
  yCountries <- merge(yCountries, mapping[, c(2, 3)], by.x = "Region", by.y = "CountryCode")

  # multiply those countriesin OAS, MEA, LAM, SSA by regional share in 1960 and 1980
  # NOTE assumption: share of  private investment in 1960  based on 1980 levels
  aggr <- intersect(getItems(ratio, dim = 1), unique(mapping$RegionCode))
  aggrRatio <- as.data.frame(setYears(ratio[aggr, c(1980, 2011), ], c(1960, 2011)))[, -c(1, 4, 5)]

  yCountries <- merge(yCountries, aggrRatio,  by.x = c("RegionCode", "Year"), by.y = c("Region", "Year"))
  yCountries$GERD <- yCountries$Value.x * yCountries$Value.y # nolint

  gerdy <- as.magpie(yCountries[, c(2, 3, 7)], spatial = 2, temporal = 1, tidy = TRUE)

  gerdy <- time_interpolate(gerdy, interpolated_year = getYears(fullgerd))

  pardey <- mbind(fullgerd, gerdy)

  # convert PARDEY from 2009 PPP to 2017 USD MER
  pardey <- convertGDP(pardey,
                       unit_in = "constant 2009 Int$PPP",
                       unit_out = "constant 2017 US$MER",
                       replace_NAs = "no_conversion")

  ### read in OECD data to get missing countries: REF missing from pardey data
  oecd <- read.csv("GERD_FORD_OECD.csv")
  oecd <- oecd[, -c(2, 3, 5, 7, 9, 11, 12, 13, 14, 15, 16, 18, 19)]

  oecd <- as.magpie(oecd, spatial = 1, temporal = 5)

  oecd <- oecd[, , "Agricultural and veterinary sciences"][, , "2015 Dollars - Constant prices and PPPs"]

  total <- dimSums(oecd[, , "Total intramural", invert = TRUE], na.rm = TRUE)
  total["POL", , ] <- oecd["POL", , "Total intramural"]
  # 2015$ price and PPP

  getNames(total) <- "GERD"

  oecdRus <- setdiff(getItems(total, dim =  1), getItems(pardey, dim =  1))

  # assume share of these countries as global total same in the past as data only intersects in 2010/2011
  rusShr <- magpply(total[oecdRus, c(2010, 2011), ] / dimSums(pardey[, c(2010, 2011), ], dim = 1), mean, MARGIN = 1)

  pastRus <- rusShr * dimSums(pardey, dim = 1)

  # drop TWN which sneaked in there
  pastRus <- pastRus["TWN", , invert = TRUE]

  pastRus <- convertGDP(pastRus,
                        unit_in = "constant 2015 Int$PPP",
                        unit_out = "constant 2017 US$MER",
                        replace_NAs = "no_conversion")

  out <- mbind(pardey, pastRus)

  return(out)
}
