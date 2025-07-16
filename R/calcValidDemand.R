#' @title calcValidDemand
#' @description calculates the validation data for the utilization of agricultural products
#' for food, feed, bioenergy, seed, material, processing, or waste
#'
#' @param datasource Datasource of validation data.
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient The nutrient in which the results shall be reported.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Isabelle Weindl
#' @seealso
#' \code{\link[mrcommons]{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#'
#' \dontrun{
#' calcOutput("ValidDemand")
#' }
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass add_columns mbind getNames
calcValidDemand <-
  function(datasource = "FAO",
           detail = TRUE,
           nutrient = "dm") {

    if (datasource == "FAO") {
      mb <-
        collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , nutrient])


      processing <-
        setdiff(c(findset("processing20")), c("milling", "ginning", "breeding"))

      mb2 <-
        mb[, , c("food", "feed", "seed", "waste", "other_util", "bioenergy")]
      # for cereals, flour, brans and branoil are counted as fooduse,
      # even though later being processed (FAO method)
      mb2[, , "food"] <-
        mb2[, , "food"] + dimSums(mb[, , c("flour1", "brans1", "branoil1")],
                                  dim = 3.2)
      mb2 <- add_columns(mb2, addnm = "processed", dim = 3.2)
      mb2[, , "processed"] <- dimSums(mb[, , processing], dim = 3.2)

      balanceflow <-
        mb[, , "domestic_supply"] - dimSums(mb2[, , ], dim = c(3.2))
      getNames(balanceflow, dim = 2) <- "dom_balanceflow"
      mb2 <- mbind(mb2, balanceflow)

      getNames(mb2, dim = 2) <- reportingnames(getNames(mb2, dim = 2))

      mb3 <- dimOrder(mb2, c(2, 1))
      sum <- dimSums(mb3, dim = 3.1)
      sum <-
        reporthelper(
          x = sum,
          dim = 3.1,
          level_zero_name = "Demand",
          detail = detail
        )
      sum <- summationhelper(sum)
      getNames(sum)[1] <- "Demand"
      getNames(sum) <-
        gsub(pattern = "Demand\\|\\+",
             replacement = "Demand|++",
             x = getNames(sum))

      out <- NULL
      for (type in getNames(mb3, dim = 1)) {
        tmp <- collapseNames(mb3[, , type], collapsedim = 1)
        # demand.R renamed dim=3.1
        tmp <-
          reporthelper(
            x = tmp,
            level_zero_name = paste0("Demand|", type),
            detail = detail,
            dim = 3.1
          )
        out <- mbind(out, tmp)
      }
      out <- summationhelper(out)

      out <- mbind(out, sum)
      out <-
        add_dimension(out,
                      dim = 3.1,
                      add = "scenario",
                      nm = "historical")
      out <-
        add_dimension(out,
                      dim = 3.2,
                      add = "model",
                      nm = "FAOSTAT CBS 2016")

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

    getNames(out) <- paste0(getNames(out), " (", unit, ")")

    return(list(
      x = out,
      weight = NULL,
      unit = unit,
      description = "Agricultural Demand"
    ))
  }
