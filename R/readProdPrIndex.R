#' Read FAO producer price index
#' 
#' Read-in world producer price index as magclass object
#' 
#' 
#' @return magpie object of the world producer index from FAO
#' @author Mishko Stevanovic
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="ProdPrIndex")
#' }
#' 
#' @importFrom readxl read_excel
#' @importFrom magclass as.magpie

readProdPrIndex <- function(){
  data <- as.data.frame(read_excel("food_price_index_nominal_real.xlsx",skip=4,col_names = FALSE))
  data <- data[!is.na(data[[1]]),]
  colnames(data) <- c("Year","nominal","real")
  data <- as.magpie(data)
  return(data)
}