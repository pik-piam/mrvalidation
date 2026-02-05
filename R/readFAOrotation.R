#' @title FAOrotation
#' @description Read Rotation lengths reported by Global planted forests thematic study: Results and analysis (2006)
#'
#' @param subtype subtype - only FAO supported
#' @return magpie object containing rotation length data
#' @author Abhijeet Mishra
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("FAOrotation")
#' }
#' @importFrom madrat toolSubtypeSelect
#' @importFrom openxlsx read.xlsx

readFAOrotation <- function(subtype = "FAO") {
  if (subtype == "FAO") {
    ## Set file name
    file <- "rotation_length_validation.xlsx"

    ## Read source data
    rlData <- read.xlsx(file)

    ## Change colnames and little cleanup
    colnames(rlData) <- gsub(pattern = "\\.", replacement = "_", x = tolower(colnames(rlData)))

    ## Convert to MAgPIE object
    rlMag <- as.magpie(unique(rlData[, c(-1, -7)]), spatial = "sub_region", temporal = NULL)

    ## Set mapping
    mapping <- read.csv("subreg_mapping.csv", sep = ";", strip.white = TRUE)

    ## Convert to ISO level data
    out <- toolAggregate(rlMag, rel = mapping, from = "sub_region", to = "CountryCode", dim = 1, weight = NULL)

    return(out)
  } else {
    ## Stop if subtype is not set correctly
    stop("Invalid subtype. Only FAO as subytpe is supported.")
  }
}
