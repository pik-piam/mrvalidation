#' @title readBII
#' @description Read and select scenarios and variables from the "The Biodiversity Intactness Index -
#' country, region and global-level summaries for the year 1970 to 2050 under various scenarios"
#' dataset.
#' Default behavior is to return BII for the historical period.
#' @seealso https://data.nhm.ac.uk/dataset/bii-bte
#' @author Michael Crawford
#'
#' @param subtype scenario selection, with potential values: "historical", "ssp1rcp2p6image",
#' "ssp2rcp4p5messageglobiom", "ssp3rcp7p0aim", "ssp4rcp6p0gcam", "ssp5rcp8p5remindmagpie".
#' Lists are accepted.
#' @param subset variable selection, with potential values: "bii", "crops", "highintensityag",
#' "hpd", "pastureland", "qualitynatural", "urbanextent". Lists are accepted.
#'
#' @return magclass object containing the desired subtypes and subsets
#'
#' @examples
#'
#' \dontrun{
#'   readSource("BII")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>% mutate filter select
#' @importFrom rlang .data
#' @importFrom stringr str_match
#' @importFrom magclass as.magpie

readBII <- function(subtype = "historical", subset = "bii") {

    bii <- readRDS("BII_historical_and_SSPs.rds")

    bii <- bii %>%
        filter(.data$scenario %in% subtype & .data$variable %in% subset)

    bii <- bii %>%
        mutate(ISO = str_match(string = .data$area_code, pattern = "[A-Z]{3}")) %>% # identify the country aggregation
        filter(!is.na(.data$ISO)) %>% # remove all non-country aggregations (e.g. global)
        select(.data$ISO, .data$scenario, .data$variable, .data$year, .data$value)

    biiMag <- as.magpie(bii)

    biiMag <- toolCountryFill(biiMag) # Taiwan and many islands are missing from this dataset
    # For each NA, set BII to this year's mean value
    for (y in getYears(biiMag)) {
        biiMag[, y, ][which(is.na(biiMag[, y, ]))] <- mean(biiMag[, y, ], na.rm = TRUE)
    }

    biiMag <- magpiesort(biiMag)

    return(biiMag)

}
