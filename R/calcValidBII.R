#' @title calcValidBII
#' @description validation for the Biodiversity Intactness Index (BII)
#' @author Michael Crawford
#'
#' @return Country-level MAgPIE object
#'
#' @examples
#'
#' \dontrun{
#'    calcOutput("ValidBII")
#' }
#'

calcValidBII <- function() {

  bii <- readSource("BII", subtype = "historical", subset = "bii")
  bii <- add_dimension(bii, dim = 3.2, add = "model", nm = "Phillips et al")
  getNames(bii, dim = "variable") <- "Biodiversity|BII"

  land <- calcOutput("FAOLand", aggregate = FALSE)
  landArea <- land[, "y2015", "6601|Land area"] # total land is stable through time, so I arbitrarily use y2015

  return(list(x = bii,
              weight = landArea,
              unit = "unitless",
              description = "Historical BII for 1970-2014"))

}
