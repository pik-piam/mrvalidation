#' @title readGSOC
#' @description This function reads the raw GSOC data (available at http://54.229.242.119/GSOCmap/)
#' or if available the preprocessed raster layers.
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' readSource("GSOC")
#' }
#'
#' @importFrom raster raster aggregate res projectRaster writeRaster brick
#' @importFrom magclass as.magpie mbind


readGSOC <- function() {

  if (!file.exists("GSOC_30cm.grd")) {

    tmp     <- raster("GSOCmapV1.2.0.tif")
    toMag   <- aggregate(tmp, fact = 60, fun = mean, na.rm = TRUE)
    writeRaster(toMag, filename = "GSOC_30cm.grd")

  } else {

    toMag <- raster("GSOC_30cm.grd")
  }

  # Change longitude and latitude
  r50     <- raster(resolution = 0.5)
  toMag   <- raster::brick(raster::projectRaster(toMag, r50, over = TRUE)) # re-project to regular grid

  # Convert to magpie object and rename set
  x       <- clean_magpie(as.magpie(toMag))
  getSets(x) <- c("cell", "t", "data")

  return(x)
}
