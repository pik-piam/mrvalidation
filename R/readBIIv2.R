#' @title readBIIv2
#' @description Read the De Palma et al. 2024 NHM BII v2.1.1 rasters and return
#' a cellular magclass on the 67420-cell lpjcell grid. Country-aggregation is
#' left to the consumer (calcValidBII).
#' @seealso \code{\link{readBII}}, \code{\link{downloadBIIv2}}, \code{\link{calcValidBII}}
#' @author Michael Crawford, Florian Humpenoeder
#'
#' @return magclass: x.y.iso x year x "bii", fraction (0-1). Source is percent
#'   (0-100); divided by 100 here to match readBII (Phillips et al. 2021).
#'
#' @details
#' 5 arc-minute -> 30 arc-minute via terra::aggregate(fact = 6, fun = "mean")
#' -- unweighted mean is fine within a 6x6 block (<1% cell-area variation at
#' 80 deg lat, BII is intensive). terra::project pins the result to the
#' canonical 0.5-deg grid. terra::extract samples the 67420 lpjcell
#' coordinates from mstools::toolGetMappingCoord2Country.
#'
#' Data sub-dim is named "variable" (not the cellular-read default "data")
#' so calcValidBII can use \code{getNames(x, dim = "variable") <- ...} on
#' both v1 and v2 inputs.
#'
#' @examples
#'
#' \dontrun{
#'   readSource("BIIv2")
#' }
#'
#' @importFrom magclass as.magpie mbind getCells getNames getSets getYears
#' @importFrom terra rast aggregate project extract
#' @importFrom mstools toolGetMappingCoord2Country

readBIIv2 <- function() {

  years <- c(2000, 2005, 2010, 2015, 2020)
  files <- setNames(sprintf("bii-%d_v2-1-1.tif", years), as.character(years))
  missingFiles <- files[!file.exists(files)]
  if (length(missingFiles)) {
    stop("Missing BII v2.1.1 GeoTIFF(s): ", paste(missingFiles, collapse = ", "),
         ". Run downloadSource(\"BIIv2\") first.")
  }

  map        <- toolGetMappingCoord2Country(pretty = TRUE)
  resolution <- terra::rast(resolution = 0.5)

  .read <- function(year, file) {
    r    <- terra::rast(file)
    r    <- terra::aggregate(r, fact = 6, fun = "mean", na.rm = TRUE)
    r    <- terra::project(r, resolution)
    vals <- terra::extract(r, map[c("lon", "lat")])[, 2]   # col 1 = ID
    m    <- as.magpie(vals, spatial = 1)
    getYears(m) <- year
    getNames(m) <- "bii"
    m
  }

  out <- do.call(mbind, Map(.read, years, files))
  getCells(out) <- paste(map$coords, map$iso, sep = ".")
  getSets(out)  <- c("x.y.iso", "t", "variable")
  out <- out / 100   # percent -> fraction (parity with readBII)

  return(out)
}
