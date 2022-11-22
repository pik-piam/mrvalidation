#' @title calcValidCMIP6
#' @description validation for the MAGICC warming output, showing average temperature for the different RCPs
#' @author Michael Crawford
#'
#' @return GLO MAgPIE object containing average warming per RCP
#'
#' @examples
#'
#' \dontrun{
#'    calcOutput("calcValidCMIP6")
#' }
#'

calcValidCMIP6 <- function() {

  cmip <- readSource("CMIP6")
  cmip <- cmip[, , c("UKESM1-0-LL", "MRI-ESM2-0", "IPSL-CM6A-LR")] # filter to the included ISIMIP core ESMs

  getSets(cmip)["d3.1"] <- "model"
  getSets(cmip)["d3.2"] <- "scenario"
  cmip <- add_dimension(cmip, dim = 3.3, add = "variable", nm = "Global Surface Temperature")

  getComment(cmip) <- NULL

  return(list(x           = cmip,
              weight      = NULL,
              unit        = "C",
              description = "Mean projected near-surface temperature warming from CMIP6 database of Nicholls
                             et al 2021"))

}
