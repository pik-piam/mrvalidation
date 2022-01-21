#' @title downloadSanderman
#' @description Download data from Soil Carbon Debt Paper (https://github.com/whrc/Soil-Carbon-Debt/)
#'
#' @return Meta information on downloaded data
#' @author Kristine Karstens
#'
#' @examples
#'
#' \dontrun{
#'   readSource("SoilCarbonDebt", subtype = "SOCS_2010")
#' }
#' 
#' @importFrom utils download.file person

downloadSoilCarbonDebt <- function() {

  files <- c(SOCS_900      = "SOCS_0_30cm_year_900AD_10km.tif",
             SOCS_1800     = "SOCS_0_30cm_year_1800AD_10km.tif",
             SOCS_1910     = "SOCS_0_30cm_year_1910AD_10km.tif",
             SOCS_1960     = "SOCS_0_30cm_year_1960AD_10km.tif",
             SOCS_1990     = "SOCS_0_30cm_year_1990AD_10km.tif",
             SOCS_2010     = "SOCS_0_30cm_year_2010AD_10km.tif",
             SOCS_noLU     = "SOCS_0_30cm_year_NoLU_10km.tif")

  url  <- "https://github.com/whrc/Soil-Carbon-Debt/raw/master/"

  for (f in names(files)) {

    file <- toolSubtypeSelect(f, files)
    download.file(paste0(url, "SOCS/", file), destfile = file, mode = "wb")
  }

  download.file(paste0(url, "OCD/landmask_10km.tif"), destfile = "landmask_10km.tif", mode = "wb")

  # Compose meta data
  return(list(url           = url,
              doi           = "doi:10.1073/pnas.1706103114",
              title         = "Soil organic carbon stocks (Mg C / ha) for 0-30 depths predicted at various years",
              author        = list(person("Jonathan", "Sanderman", email = "jsanderman@woodwellclimate.org")),
              version       = NULL,
              release_date  = NULL,
              description   = NULL,
              license       = "MIT License",
              reference     = NULL)
  )
}
