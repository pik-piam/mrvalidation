#' @title readGISTEMP
#' @description read GISTEMP historical global surface temperature data from the Goddard Institute for Space Studies
#' @seealso https://data.giss.nasa.gov/gistemp/
#' @author Michael Crawford
#'
#' @return magclass object
#'
#' @examples
#'
#' \dontrun{
#'   readSource("readGISTEMP")
#' }
#'
#' @importFrom dplyr %>% mutate select rename filter
#' @importFrom rlang .data

readGISTEMP <- function() {

    d <- read.csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv", header = TRUE, skip = 1)

    d <- d %>%
        mutate(Region = "GLO") %>%
        select(.data$Region, .data$Year, .data$J.D)

    d <- d %>%
        rename(surfaceTemp = .data$J.D) %>%
        filter(.data$surfaceTemp != "***") %>%
        mutate(surfaceTemp = as.numeric(.data$surfaceTemp))

    d <- d %>%
        mutate(Year = as.numeric(.data$Year)) %>%
        filter(.data$Year >= 1995)

    dMag <- as.magpie(d)
    dMag <- add_dimension(dMag, dim = 3.1, add = "scenario", nm = "historical")
    dMag <- add_dimension(dMag, dim = 3.2, add = "model", nm = "GISTEMP")
    getSets(dMag)["d3.3"] <- "variable"

    return(dMag)
}
