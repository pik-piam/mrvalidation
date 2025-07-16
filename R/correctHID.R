#' @title {correctHID}
#' @description  Correct Irrigated Area
#'
#' Correct Irrigated Area to 0.5 Degree x 0.5 Degree Grid.
#' Change resolution from 5 arcmin to 0.5 Degree by aggregating.
#' Values in ha are summed up, Values in percent are calculated using mean.
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
#' @importFrom raster raster extent<- as.matrix aggregate brick
#' @importFrom utils read.table
#' @importFrom magclass getRegions getYears<-

correctHID <- function(x, subtype) { # nolint: cyclocomp_linter

  if (subtype == "AEI_EARTHSTAT_CP" || subtype == "AEI_EARTHSTAT_IR" || subtype == "AEI_HYDE_FINAL_CP" ||
        subtype == "AEI_HYDE_FINAL_IR" || subtype == "AEI_HYDE_LOWER_CP" ||
        subtype == "AEI_HYDE_LOWER_IR" || subtype == "AEI_HYDE_UPPER_CP" || subtype == "AEI_UPPER_FINAL_IR") {
    # read asci file
    years <- c("1900", "1910", "1920", "1930", "1940", "1950", "1960",
               "1970", "1980", "1985", "1990", "1995", "2000", "2005")
    x     <- lapply(paste0(subtype, "_", years, ".asc"), raster)

    # read file
    f1 <- function(var1, var2) {
      extent(var1) <- var2
      return(var1)
    }
    x <- lapply(x, f1, var2 = c(-180, 180, -90, 90))
    x <- lapply(x, aggregate, fact = 6, fun = sum, na.rm = TRUE)

    f2 <- function(var1) {
      return(raster::brick(as.magpie(var1)))
    }
    x <-  lapply(x, f2)

    mag <- NULL
    for (i in seq_along(years)) {

      mag <- mbind(mag, setYears(x[[i]], years[i]))
    }
    getNames(mag) <- paste0(subtype, " in 5 arcmin resolution")
    return(mag)
  } else {
    return(x)
  }
}
