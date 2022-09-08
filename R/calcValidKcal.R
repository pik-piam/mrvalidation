#' @title calcValidKcal
#' @description calculates the validation data for calorie food supply
#'
#' @param datasource Datasource of validation data. If "FAO", we use FAO calories with FAO population data (slightly
#' diverges from original data as the convert script for example splits up countries for the past). If "FAOmassbalance"
#' we use calories from the FAO massbalance calculations, and divide them by our standard population.
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient kcal or protein
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' \code{\link{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' \dontrun{
#' calcOutput("ValidKcal")
#' }
#'
#' @importFrom magpiesets reporthelper
#' @importFrom magclass getSets dimOrder
#' @importFrom mrcommons toolFAOcombine

calcValidKcal <- function(datasource = "FAO", nutrient = "kcal", detail = TRUE) {
  if (length(nutrient) > 1) {
    stop("select only one nutrient")
  }
  if (datasource %in% c("FAO", "FAOmassbalance")) {
    if (datasource == "FAOmassbalance") {
      x <- calcOutput("FoodSupplyPast", products = "kall", per_capita = FALSE, aggregate = FALSE, attributes = nutrient)
      x2 <- calcOutput("FoodSupplyPast",
        products = "kall", per_capita = TRUE, aggregate = FALSE, supplementary = TRUE,
        attributes = nutrient, populationweight = "PopulationPast"
      )
      value <- x * 1000000
      weight <- x2$weight
      total <- dimSums(value, dim = 3)
    } else if (datasource == "FAO") {
      FSCrop <- readSource("FAO_online", "FSCrop")
      FSLive <- readSource("FAO_online", "FSLive")
      FS <- toolFAOcombine(FSLive, FSCrop, combine = "Item")

      FS <- FS[, , c("food_supply_kcal", "protein_supply")]
      getNames(FS, dim = 2) <- c("kcal", "protein")
      FS <- collapseNames(FS[, , nutrient])
      total <- FS[, , "2901|Grand Total"]

      relationmatrix <- toolGetMapping("FAOitems_online.rda", "sectoral", where = "mrvalidation")
      relationmatrix <- relationmatrix[, which(names(relationmatrix) %in% c("FoodBalanceItem", "k"))]
      relationmatrix <- relationmatrix[-which(duplicated(relationmatrix[, 1]) == TRUE), ]

      ## Note:
      ## FAO_online now has sub-aggregate and processed equivalent reporting: rice (milled equivalent), sugar (raw eq)
      ## These get dropped automatically by the mapping
      FS <- toolAggregate(x = FS, rel = relationmatrix, dim = 3.1, from = "FoodBalanceItem", to = "k", partrel = TRUE)
      missing <- setdiff(findset("kall"), getNames(FS, dim = 1))
      FS <- add_columns(FS, addnm = missing, dim = 3.1)
      FS[, , missing] <- 0
      value <- collapseNames(FS)
      population <- readSource(type = "FAO", subtype = "Pop", convert = TRUE)
      weight <- collapseNames(population[, getYears(FS), "population"]) / 1000000
    }

    if (nutrient == "kcal") {
      mainname <- "Nutrition|Calorie Supply"
      unit <- "kcal/capita/day"
    } else if (nutrient == "protein") {
      mainname <- "Nutrition|Protein Supply"
      unit <- "g protein/capita/day"
    }

    out <- reporthelper(x = value, level_zero_name = mainname, detail = detail)
    # make sure sum is not neglecting products
    out[, , mainname] <- collapseNames(total)

    sumup <- getNames(out[, , mainname, invert = TRUE], dim = 1)
    out <- out[, , c(mainname, sumup)] # right order
    getNames(out, dim = 1) <- c(mainname, getNames(summationhelper(out[, , sumup], sep = "+", dim = 3.1), dim = 1))

    out <- out / weight / 365
    out[is.nan(out)] <- 0
    out[is.infinite(out)] <- 0
    weight[out[, , mainname] == 0] <- 0

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    weight <- add_dimension(weight, dim = 3.1, add = "scenario", nm = "historical")
    weight <- add_dimension(weight, dim = 3.2, add = "model", nm = datasource)
  } else {
    stop("unknown data source")
  }

  getNames(out) <- paste0(getNames(out), " (", unit, ")")

  return(list(x = out,
              weight = weight,
              unit = unit,
              description = "FAO datasource contains slight alterations of original data, e.g. historical divison of countries.",
              min = 0,
              max = 7000))
}
