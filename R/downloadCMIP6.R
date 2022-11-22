#' @title downloadCMIP6
#' @description Download near-surface air temperature projection from CMIP6 for the following scenarios:
#' SSP1-1.9, SSP1-2.6, SSP2-4.5, SSP3-7.0, SSP4-3.4, SSP4-6.0, SSP5-8.5, SSP5-3.4-over
#' @seealso https://cmip6.science.unimelb.edu.au/
#' @author Michael Crawford
#'
#' @return Metadata on downloaded CMIP near-surface air temperature data
#'
#' @examples
#'
#' \dontrun{
#'   downloadSource("CMIP6")
#' }
#'
#' @importFrom utils download.file unzip person
#' @importFrom dplyr %>% mutate
#' @importFrom purrr map walk2
#' @importFrom rlang .data
#' @importFrom withr with_options

downloadCMIP6 <- function() {

    # future function revisions could parameterize these settings and retrieve different data
    # than only near-surface air temperature for the CMIP6 scenarios
    .buildDownloadRequest <- function(.rcp) {

        rcps            <- paste("experiment_id",   .rcp,                    sep = "=")
        normalised      <- paste("normalised",      "",                      sep = "=") # purposefully blank
        mipera          <- paste("mip_era",         "all",                   sep = "=")
        timeseriestype  <- paste("timeseriestype",  "average-year-mid-year", sep = "=")
        variableID      <- paste("variable_id",     "tas",                   sep = "=") # near-surface air temperature
        region          <- paste("region",          "World",                 sep = "=")

        root <- "https://cmip6.science.unimelb.edu.au/api/v1/download_zip?"
        request <- paste(rcps, normalised, mipera, timeseriestype, variableID, region, sep = "&")

        return(paste0(root, request))

    }

    .downloadData <- function(.rcp, .request) {

        with_options(new = list("timeout" = 1000),
                     code = {
            zipPath <- paste0(.rcp, ".zip")
            rcpPath <- paste0(.rcp)

            download.file(url = .request, destfile = zipPath)
            unzip(zipPath, exdir = rcpPath)
            file.remove(zipPath)
        })

    }

    rcps <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", "ssp460", "ssp534-over", "ssp585")

    requests <- data.frame(rcp = rcps)
    requests <- requests %>%
        mutate(request = map(.x = .data$rcp, .f = .buildDownloadRequest))

    walk2(.x = requests$rcp, .y = requests$request, .f = .downloadData)

    # nolint start

    return(list(url           = "https://cmip6.science.unimelb.edu.au/",
                doi           = "https://doi.org/10.1002/gdj3.113",
                title         = "Regionally aggregated, stitched and de-drifted CMIP-climate data, processed with netCDF-SCM v2.0.0",
                unit          = "C",
                author        = list(person("Zebedee", "Nicholls"),
                                     person("Jared", "Lewis"),
                                     person("Melissa", "Makin"),
                                     person("Usha", "Nattala"),
                                     person("Geordie", "Zhang"),
                                     person("Simon", "Mutch"),
                                     person("Edoardo", "Tescari"),
                                     person("Malte", "Meinshausen")),
                version       = NULL,
                release_date  = "23 February, 2021",
                description   = "The world's most complex climate models are currently running a range of experiments as part of the Sixth Coupled Model Intercomparison Project (CMIP6). Added to the output from the Fifth Coupled Model Intercomparison Project (CMIP5), the total data volume will be in the order of 20PB. Here, we present a dataset of annual, monthly, global, hemispheric and land/ocean means derived from a selection of experiments of key interest to climate data analysts and reduced complexity climate modellers. The derived dataset is a key part of validating, calibrating and developing reduced complexity climate models against the behaviour of more physically complete models. In addition to its use for reduced complexity climate modellers, we aim to make our data accessible to other research communities. We facilitate this in a number of ways. Firstly, given the focus on annual, monthly, global, hemispheric and land/ocean mean quantities, our dataset is orders of magnitude smaller than the source data and hence does not require specialized `big data` expertise. Secondly, again because of its smaller size, we are able to offer our dataset in a text-based format, greatly reducing the computational expertise required to work with CMIP output. Thirdly, we enable data provenance and integrity control by tracking all source metadata and providing tools which check whether a dataset has been retracted, that is identified as erroneous. The resulting dataset is updated as new CMIP6 results become available and we provide a stable access point to allow automated downloads. Along with our accompanying website (cmip6.science.unimelb.edu.au), we believe this dataset provides a unique community resource, as well as allowing non-specialists to access CMIP data in a new, user-friendly way.",
                license       = "Creative Commons Attribution 4.0 International",
                reference     = "Nicholls Z, Lewis J, Makin M, Nattala U, Zhang GZ, Mutch SJ, Tescari E and Meinshausen, M. Regionally aggregated, stitched and de-drifted CMIP-climate data, processed with netCDF-SCM v2.0.0. Geosci Data J. 2020; 00:000-000. https://doi.org/10.1002/gdj3.113")
    )

    # nolint end
}
