#' @title FAOrotation
#' @description Read Rotation lengths reported by Global planted forests thematic study: Results and analysis (2006)
#'
#' @param subtype subtype - only FAO supported
#' @return magpie object containing rotation length data
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("FAOrotation")
#' }
#' @importFrom madrat toolSubtypeSelect getConfig
#' @importFrom openxlsx read.xlsx

readFAOrotation <- function(subtype = "FAO") {
  if (subtype == "FAO") {
    ## Set file name
    file <- "rotation_length_validation.xlsx"

    ## Read source data
    rl_data <- read.xlsx(file)

    ## Change colnames and little cleanup
    colnames(rl_data) <- gsub(pattern = "\\.", replacement = "_", x = tolower(colnames(rl_data)))

    ## Convert to MAgPIE object
    rl_mag <- as.magpie(unique(rl_data[, c(-1, -7)]), spatial = "sub_region", temporal = NULL)

    ## Set mapping
    mapping <- read.csv("subreg_mapping.csv", sep = ";", strip.white = TRUE)

    ## Convert to ISO level data
    rl_iso <- toolAggregate(rl_mag, rel = mapping, from = "sub_region", to = "CountryCode", dim = 1, weight = NULL)

    ## Pass info to out
    out <- rl_iso

    return(out)
  } else {
    ## Stop if subtype is not set correctly
    stop("Invalid subtype. Only FAO as subytpe is supported.")
  }
}
