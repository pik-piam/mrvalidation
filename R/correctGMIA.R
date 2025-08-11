#' @title correctGMIA
#' @description Correct Irrigated Area
#'
#' Correct Irrigated Area to 0.5 Degree x 0.5 Degree Grid.
#' Change resolution from 5 arcmin to 0.5 Degree by aggregating. Values in ha are summed up,
#' Values in percent are calculated using mean.
#'
#' @param x MAgPIE object containing Global Map on Irrigaiton data data at 0.5 Degree resolution
#' @param subtype : subtypes are the same as in readGMIA
#' @return Global Map on Irrigation data as MAgPIE object at a 0.5 Degree resolution.
#' @author Stephen Wirth
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("IrrigatedArea")
#' }
#' @importFrom raster raster extent<- aggregate brick
#'
#'

correctGMIA <- function(x, subtype) {

  files <- c(all_data_national = "HESS_2010_159_Supplement_S2.csv",
             aei_pct = "gmia_v5_aei_pct.asc",
             aei_ha = "gmia_v5_aei_ha.asc",
             aai_pct_aei = "gmia_v5_aai_pct_aei.asc",
             aeigw_pct_aei = "gmia_v5_aeigw_pct_aei.asc",
             aeisw_pct_aei = "gmia_v5_aeisw_pct_aei.asc",
             aeinc_pct_aei = "gmia_v5_aeinc_pct_aei.asc")

  file <- toolSubtypeSelect(subtype, files)
  if (subtype == "all_data_national") {
    return(x)
  }

  # read file
  x         <- raster::raster(file)
  extent(x) <- c(-180, 180, -90, 90)

  # check for subtype and aggregate according to it
  if (grepl("ha", subtype)) {
    x <- raster::aggregate(x, fact = 6, fun = sum, na.rm = TRUE)
  } else {
    x <- raster::aggregate(x, fact = 6, fun = mean, na.rm = TRUE)
  }

  # raster to magpie
  y <- as.magpie(raster::brick(x))
  z <- new.magpie(cells_and_regions = getCells(y), years = 2000:2008,
                  names = subtype, sets = getSets(y))
  z[, 2000:2008, ] <- y

  return(z)
}
