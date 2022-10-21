#' @title downloadGISTEMP
#' @description download GISTEMP historical global surface temperature data from the Goddard Institute for Space Studies
#' @seealso https://data.giss.nasa.gov/gistemp/
#' @author Michael Crawford
#'
#' @return Metadata on downloaded dataset
#'
#' @examples
#'
#' \dontrun{
#'   downloadSource("BII")
#' }
#'
#' @importFrom utils download.file

downloadGISTEMP <- function() {

    # nolint start

    csvURL  <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"
    download.file(csvURL, destfile = "GISTEMP_GlobalSurfaceTemperature.csv")

    return(list(url           = "https://data.giss.nasa.gov/gistemp/",
                title         = "GISS Surface Temperature Analysis (GISTEMP)",
                unit          = "C",
                author        = "GISTEMP Team",
                version       = "v4",
                release_date  = "2022",
                description   = "The GISS Surface Temperature Analysis ver. 4 (GISTEMP v4) is an estimate of global surface
                                temperature change. Graphs and tables are updated around the middle of every month using current
                                data files from NOAA GHCN v4 (meteorological stations) and ERSST v5 (ocean areas), combined as
                                described in our publications Hansen et al. (2010) and Lenssen et al. (2019). These updated files
                                incorporate reports for the previous month and also late reports and corrections for earlier
                                months.",
                license       = "Open Data Commons Public Domain Dedication and License (PDDL)",
                reference     = "GISTEMP Team, 2022: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies."
                )
            )

    # nolint end
}
