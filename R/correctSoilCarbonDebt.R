#' @title correctSoilCarbonDebt
#' @description Correct data from Soil Carbon Debt Paper (https://github.com/whrc/Soil-Carbon-Debt/)
#'
#' @param x magpie object provided by the read function

#' @return List of magpie objects
#' @author Kristine Karstens
#'
#' @examples
#'
#' \dontrun{
#'   readSource("SoilCarbonDebt", convert="onlycorrect")
#' }
#' 
#' @importFrom madrat toolConditionalReplace

correctSoilCarbonDebt <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)
  
  return(x)
}
