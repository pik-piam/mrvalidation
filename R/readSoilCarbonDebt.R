#' @title readSoilCarbonDebt
#' @description Read data from Soil Carbon Debt Paper (https://github.com/whrc/Soil-Carbon-Debt/)
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#'
#' \dontrun{
#'   readSource("SoilCarbonDebt", subtype = "SOCS_2010")
#' }
#'
#' @importFrom magclass as.magpie getNames getSets getCells
#' @importFrom raster raster projectRaster area aggregate extract
#' @importFrom mrcommons toolGetMappingCoord2Country

readSoilCarbonDebt <- function() {

  files <- c(SOCS_900      = "SOCS_0_30cm_year_900AD_10km.tif",
             SOCS_1800     = "SOCS_0_30cm_year_1800AD_10km.tif",
             SOCS_1910     = "SOCS_0_30cm_year_1910AD_10km.tif",
             SOCS_1960     = "SOCS_0_30cm_year_1960AD_10km.tif",
             SOCS_1990     = "SOCS_0_30cm_year_1990AD_10km.tif",
             SOCS_2010     = "SOCS_0_30cm_year_2010AD_10km.tif",
             SOCS_noLU     = "SOCS_0_30cm_year_NoLU_10km.tif")

  map        <- toolGetMappingCoord2Country(pretty = TRUE)
  area       <- raster::raster("landmask_10km.tif")
  r5min      <- raster::raster(res = 0.5/6)
  area5min   <- raster::area(r5min)/10^4 * area
  area30min  <- raster::aggregate(area5min, fact = 6, fun = sum)

  .convertToMag <- function(f, file) {

    data           <- raster::raster(file)
    data           <- raster::projectRaster(data, r5min, over = TRUE)
    data30min      <- raster::aggregate(data * area5min, fact = 6, fun = mean) / area30min
    mag            <- as.magpie(raster::extract(data30min, map[c("lon", "lat")]), spatial = 1)

    getNames(mag)  <- f
    getCells(mag)  <- paste(map$coords, map$iso, sep = ".")
    getSets(mag)   <- c("x.y.iso", "t", "data")
    return(mag)
  }

  mag <- NULL

  for (f in names(files)) {

    file <- toolSubtypeSelect(f, files)
    mag  <- mbind(mag, .convertToMag(f, file))
  }

  return(mag)
}
