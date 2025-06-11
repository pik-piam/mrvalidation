#' @title calcValidFeed
#' @description calculates the validation data for feed demand by animal type
#'
#' @param datasource Datasource of validation data.
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient The nutrient in which the results shall be reported.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' \dontrun{
#' calcOutput("ValidFeed")
#' }
#'
#' @importFrom magpiesets reporthelper
#' @importFrom magclass dimOrder
calcValidFeed <- function(datasource = "FAO", detail = TRUE, nutrient = "dm") {

  if (datasource == "FAO") {
    mb <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , nutrient])
    } else if (datasource == "FAOpre2010") {
      mb <- collapseNames(calcOutput("FAOmassbalance", version = "pre2010", aggregate = FALSE)[, , nutrient])
    } else if (datasource == "FAOpost2010") {
      mb <- collapseNames(calcOutput("FAOmassbalance", version = "post2010", aggregate = FALSE)[, , nutrient])
    } else {
      stop("No data exist for the given datasource!")
    }


    mb2 <- mb[, , c("feed_fish", "feed_livst_chick", "feed_livst_egg",
                    "feed_livst_milk", "feed_livst_pig", "feed_livst_rum")]

    getNames(mb2, dim = 2) <- reportingnames(getNames(mb2, dim = 2))


    mb3 <- dimOrder(mb2, c(2, 1))
    out <- reporthelper(x = mb3, dim = 3.2, level_zero_name = "_", detail = detail)
    dimnames(out)[[3]] <- gsub("[^[:alnum:][:blank:]\\|]", "", getNames(out))

    out <- summationhelper(out, sep = "+")

    lvl1 <- c("+|Feed for Aquaculture",
              "+|Feed for Poultry meat",
              "+|Feed for Eggs",
              "+|Feed for Dairy",
              "+|Feed for Pig meat",
              "+|Feed for Ruminant meat")
    tmp <- out[, , lvl1]
    getNames(tmp) <- paste0("+", getNames(tmp))
    out <- mbind(tmp, out[, , lvl1, invert = TRUE])

    dimnames(out)[[3]] <- paste0("Demand|Feed|", dimnames(out)[[3]])

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  } else {
    stop("No data exist for the given datasource!")
  }

  names(dimnames(out))[3] <- "scenario.model.variable"

  if (nutrient == "dm") {
    unit <- "Mt DM/yr"
  } else if (nutrient == "nr") {
    unit <- "Mt Nr/yr"
  } else if (nutrient == "p") {
    unit <- "Mt P/yr"
  } else if (nutrient == "k") {
    unit <- "Mt K/yr"
  } else if (nutrient == "ge") {
    unit <- "PJ/yr"
  } else if (nutrient == "wm") {
    unit <- "Mt WM/yr"
  }

  getNames(out) <- sub("\\|$", "", getNames(out))
  getNames(out) <- paste0(getNames(out), " (", unit, ")")

  return(list(x = out,
              weight = NULL,
              unit = unit,
              description = "Agricultural Demand")
  )
}
