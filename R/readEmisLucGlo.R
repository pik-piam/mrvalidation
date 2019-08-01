#' @title EmisLucGlo
#' @description Read historic land-use change CO2 emissions
#' 
#' 
#' @param subtype Available subtypes are:
#'  \itemize{
#'  \item historical:
#'  \itemize{
#'  \item Canadell_2007
#'  \item Friedlingstein_2010
#'  \item Harris_2013
#'  \item Houghton_2012
#'  \item RCP
#'  }
#'  }
#' @return magpie object containing data land-use change CO2 emissions
#' @author Florian Humpenoeder
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("EmisLucGlo",subtype="Canadell_2007")
#' a <- readSource("EmisLucGlo",subtype="Friedlingstein_2010")
#' }
#' #'@importFrom reshape2 dcast
#' @importFrom madrat toolSubtypeSelect

readEmisLucGlo <- function(subtype=NULL){
  
  files <- c(Canadell_2007= "Canadell_2007.csv",
             Friedlingstein_2010="Friedlingstein_2010.csv",
             Harris_2013="Harris_2013.csv",
             Houghton_2012="Houghton_2012.csv",
             RCP="RCP.csv")
  
  file <- toolSubtypeSelect(subtype, files)
  
  x <- read.magpie(file_name = file)
  getSets(x) <- c("region", "year", "data")
  return(x)
  
}
