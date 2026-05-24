#' @title downloadBIIv2
#' @description Download the Biodiversity Intactness Index v2.1.1 (Open Access,
#' Limited Release) from the Natural History Museum (De Palma et al. 2024,
#' \doi{10.5519/k33reyb6}). Global 5 arc-minute GeoTIFFs for 2000, 2005, 2010,
#' 2015, 2020.
#' @seealso \url{https://data.nhm.ac.uk/dataset/bii-developed-by-nhm-v2-1-1-limited-release},
#'   \code{\link{downloadBII}}, \code{\link{readBIIv2}}
#' @author Michael Crawford, Florian Humpenoeder
#'
#' @return Metadata on downloaded BII v2.1.1 data.
#'
#' @details
#' The NHM CKAN resource URL returns HTTP 403 to non-browser clients
#' (Cloudflare). The portal's content-addressable
#' \code{/downloads/direct/<sha>.zip} endpoint bypasses the challenge.
#' If the URL becomes unreachable, download the ZIP manually via a browser
#' from the landing page and place it as 'bii-v2-1-1-nhm-data-portal.zip'
#' in this source folder.
#'
#' @examples
#'
#' \dontrun{
#'   downloadSource("BIIv2")
#' }
#'
#' @importFrom utils download.file person unzip

downloadBIIv2 <- function() {

  # nolint start

  url     <- "https://data.nhm.ac.uk/downloads/direct/8161226fda7a36ca0b94f2becee9afe1b318d0a8.zip"
  zipFile <- "bii-v2-1-1-nhm-data-portal.zip"

  if (!file.exists(zipFile)) {
    tryCatch(
      utils::download.file(url, destfile = zipFile, mode = "wb"),
      error = function(e) {
        stop("Automated NHM download failed (Cloudflare likely). Manually download from ",
             "https://data.nhm.ac.uk/dataset/bii-developed-by-nhm-v2-1-1-limited-release ",
             "and place '", zipFile, "' in this source folder. Original error: ", conditionMessage(e))
      }
    )
  }

  # Cloudflare challenge stub is ~6 KB; real ZIP is ~45 MB.
  if (file.size(zipFile) < 1e7) {
    stop("'", zipFile, "' is too small (", file.size(zipFile),
         " bytes) -- likely a Cloudflare challenge page. Manually download via browser.")
  }

  # Outer ZIP holds an inner ZIP (resource-ID-named) + manifest.json; inner ZIP has the 5 GeoTIFFs.
  utils::unzip(zipFile, exdir = ".")
  innerZip <- "c4c281c4-befa-4e1b-a162-ba2f25e5ae82.zip"
  if (!file.exists(innerZip)) {
    stop("Inner archive '", innerZip, "' missing -- NHM ZIP layout may have changed.")
  }
  utils::unzip(innerZip, exdir = ".")

  return(list(url          = "https://data.nhm.ac.uk/dataset/bii-developed-by-nhm-v2-1-1-limited-release",
              doi          = "https://doi.org/10.5519/k33reyb6",
              title        = "The Biodiversity Intactness Index developed by The Natural History Museum, London, v2.1.1 (Open Access, Limited Release)",
              unit         = "Percentage (0-100)",
              author       = list(person("Adriana", "De Palma"),
                                  person("Sara", "Contu"),
                                  person("Gareth E", "Thomas"),
                                  person("Connor", "Duffin"),
                                  person("Sabine", "Nix"),
                                  person("Andy", "Purvis")),
              version      = "v2.1.1",
              release_date = "2024-10-09",
              description  = paste("Global gridded BII at 5 arc-minute resolution for",
                                   "2000, 2005, 2010, 2015, 2020.",
                                   "Values 0-100 (100 = fully intact)."),
              license      = "CC-BY-NC-SA 4.0",
              reference    = paste("Adriana De Palma; Sara Contu; Gareth E Thomas; Connor Duffin; Sabine Nix; Andy Purvis (2024).",
                                   "The Biodiversity Intactness Index developed by The Natural History Museum, London,",
                                   "v2.1.1 (Open Access, Limited Release) [Data set]. Natural History Museum.",
                                   "https://doi.org/10.5519/k33reyb6"))
  )

  # nolint end
}
