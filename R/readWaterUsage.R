#' @title readWaterUsage
#' @description Read Historic and projcted Agricultural water consumption
#'
#'
#' @param subtype Available subtypes are:
#'  \itemize{
#'  \item historical:
#'  \itemize{
#'  \item foley_2011
#'  \item shiklomanov_2000
#'  \item wada_2011
#'  \item wisser_2008
#'  }
#'  \item projections
#'  \itemize{
#'  \item fischer_IIASA
#'  \item hejazi_2013
#'  \item molden_IWMI
#'  \item seckler_IWMI
#'  \item shiklomanov
#'  \item aquastat_2008_12
#'  }
#'  }
#' @return magpie object containing data on water usage
#' @author Stephen Wirth
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("foley_2011")
#' a <- readSource("aquastat_2008_12")
#' }
#' #' @importFrom reshape2 dcast
#' @importFrom magclass read.magpie getSets

readWaterUsage <- function(subtype = NULL) {

  files <- c(foley_2011 = "Foley_2011.csv",
             shiklomanov_2000 = "shiklomanov_2000.csv",
             wada_2011 = "Wada_2011.csv",
             wisser_2008 = "Wisser_2008.csv",
             fischer_IIASA = "Fischer_IIASA.csv",
             hejazi_2013 = "Hejazi_2013.csv",
             molden_IWMI = "Molden_IWMI.csv",
             seckler_IWMI = "Seckler_IWMI.csv",
             shiklomanov = "Shiklomanov.csv",
             aquastat_2008_12 = "aquastat_use_2008_12.csv")

  file <- toolSubtypeSelect(subtype, files)

  if (subtype == "aquastat_2008_12") {
    a <- read.csv(file, header = FALSE, skip = 2, stringsAsFactors = FALSE)
    colnames(a) <- as.character(a[1, ])
    a <- a[c(2:372), c(1, 3, 5, 6)]
    a$Value <- as.numeric(a$Value)
    colnames(a)[2] <- "VariableName"
    b <- reshape2::dcast(a, Area + Year ~ a$VariableName)
    return(as.magpie(b, spatial = 1, temporal = 2))
  } else {
    x          <- read.magpie(file_name = file)
    getSets(x) <- c("region", "year", "data")
    return(x)
  }
}
