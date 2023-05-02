#' @title readAR6SPMFig1
#' @description download IPCC AR6 Summary for Policymakers Fig 1, global surface temperature change
#' relative to 1850-1900 from observations. This dataset also includes the CMIP6 models (human and
#' natural forcings simulations), but are so far excluded here.
#' @seealso https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_SPM.pdf
#' @author Michael Crawford
#'
#' @return magclass object
#'
#' @examples
#'
#' \dontrun{
#'   readSource("AR6SPMFig1")
#' }
#'
#' @importFrom dplyr %>% select rename mutate across
#' @importFrom rlang .data

readAR6SPMFig1 <- function() {

    d <- read.csv("AR6SPMFig1_GlobalSurfaceTemperatureAnomaly.csv", header = TRUE, skip = 35) %>%
        select("X1", "X8") %>%
        rename(Year = "X1", Anomaly = "X8")

    d <- d[-nrow(d), ] # remove "end_data" signifier

    d <- d %>%
        mutate(Region = "GLO") %>%
        select("Region", "Year", "Anomaly") %>%
        mutate(dplyr::across(c("Year", "Anomaly"), as.numeric))

    d <- d %>% filter(.data$Year >= 1995)

    dMag <- as.magpie(d)
    dMag <- add_dimension(dMag, dim = 3.1, add = "scenario", nm = "historical")
    dMag <- add_dimension(dMag, dim = 3.2, add = "model", nm = "IPCC AR6")
    getSets(dMag)["d3.3"] <- "variable"

    return(dMag)
}
