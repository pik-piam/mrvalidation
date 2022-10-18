#' @title downloadBII
#' @description Download historical and projected BII dataset from Phillips et al. 2021
#' (https://doi.org/10.5519/he1eqmg1).
#' @seealso https://data.nhm.ac.uk/dataset/bii-bte
#' @author Michael Crawford
#'
#' @return Metadata on downloaded BII data
#'
#' @examples
#'
#' \dontrun{
#'   downloadSource("BII")
#' }
#'
#' @importFrom utils download.file person

downloadBII <- function() {

    # nolint start

    rdsURL  <- "https://data.nhm.ac.uk/dataset/07f701d4-9a26-403c-a9d6-248c895fa412/resource/94be0af6-ec90-4b83-8f02-64a4983e1ca1/download/long_data.rds"
    download.file(rdsURL, destfile = "BII_historical_and_SSPs.rds")

    return(list(url           = "https://data.nhm.ac.uk/dataset/bii-bte",
                doi           = "https://doi.org/10.5519/he1eqmg1",
                title         = "The Biodiversity Intactness Index - country, region and global-level summaries for the year
                                    1970 to 2050 under various scenarios",
                unit          = "Unitless",
                author        = list(person("Helen", "Phillips"),
                                     person("Adriana", "De Palma"),
                                     person("Ricardo E", "Gonzalez"),
                                     person("Sara", "Contu"),
                                     person("Samantha L L", "Hill"),
                                     person("Andres", "Baselga"),
                                     person("Luca", "Borger"),
                                     person("Andy", "Purvis")),
                version       = NULL,
                release_date  = "October 4, 2021",
                description   = "Using the PREDICTS database of local biodiversity measures at thousands of sites around the world,
                                    we statistically modelled how total abundance of organisms and compositional similarity responded
                                    to land use and related pressures. We combined these models with spatio-temporal projections of
                                    explanatory variables (at 0.25 degrees spatial resolution) from the year 1970 to 2050 under five
                                    Shared Socioeconomic Pathways (SSPs) to project the Biodiversity Intactness Index (BII). Mean BII
                                    (weighted by cell area) was calculated at the country, subregion, interregion and global level. We
                                    used cross-validation (leaving one biome out in turn) to produce decadal upper and lower uncertainty
                                    margins for 1970-2050. These summary data were uploaded to the Natural History Museum's Biodiversity
                                    Trends Explorer on 2021-10-27. We have also provided mean values of some of the pressures, as changes
                                    in these contribute to changes in BII.",
                license       = "Creative Commons Non-Commercial",
                reference     = "Helen Phillips; Adriana De Palma; Ricardo E Gonzalez; Sara Contu et al. (2021). The Biodiversity
                                    Intactness Index - country, region and global-level summaries for the year 1970 to 2050 under
                                    various scenarios [Data set]. Natural History Museum. ")
    )

    # nolint end
}
