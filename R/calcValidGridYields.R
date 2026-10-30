#' @title calcValidGridYields
#' @description reports Yields on 0.5 degree grid
#'
#' @param datasource downscaledFAO
#' @param physical if true (default) physical area (croparea) used for yield calculation;
#'                 if false harvested area used for yield calculation
#'
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATTOLPJML}}
#' @examples
#' \dontrun{
#' calcOutput("ValidGridYields")
#' }
#'
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<- clean_magpie
#' @importFrom madrat toolConditionalReplace
#' @importFrom stringr str_split

calcValidGridYields <- function(datasource = "downscaledFAO", physical = TRUE) {

  if (datasource == "downscaledFAO") {

    out <- calcOutput("FAOYield", cellular = TRUE, aggregate = FALSE, irrigation = TRUE, physical = physical)
    getNames(out, dim = 1) <- reportingnames(getNames(out, dim = 1))
    getNames(out, dim = 2) <- reportingnames(getNames(out, dim = 2))
    out <- clean_magpie(out)
    out <- dimOrder(out, perm = c(2, 1))
    getComment(out) <- NULL

  } else {
    stop("Source not available.")
  }

  return(list(x = out,
              weight = NULL,
              unit = "t DM per ha physical area",
              description = "Crop yields by plant type and irrigation",
              isocountries = FALSE)
  )
}
