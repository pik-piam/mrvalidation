#' @title convertPardeyAgRD
#' @description Agricultural R&D investment data read from https://www.nature.com/news/agricultural-rd-is-on-the-move-1.20571
#' 3 tables are read in: AgRD_Pardey is public Ag expenditure in 1960 and 2011, extracted from the interactive figure in the article that has more complete countries
#' agGERD and agPERD are total and public expenditures respectively, for less countries but more years
#' @param x MAgPIE object containing PardeyRD data
#' @return magpie object containing expenditure in Ag R&D, 2009 USD PPP
#' @author David Chen
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#'
#' \dontrun{
#' a <- convertSource("PardeyAgRD")
#' }
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate

convertPardeyAgRD <- function(x) {
  x <- toolCountryFill(x, fill=0)
  #x <- toolFillWithRegionAvg(x, callToolCountryFill = TRUE)
  
  
  return(x)
}