#' @title calcValidNitrogenBudgetCropland
#' @description Validation Script for Nitrogen Budgets on Croplands
#'
#' @param datasource Bodirsky for own calculations, Lassaletta2014 for a country dataset from
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014.
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems: The Relationship
#' between Yield and Nitrogen Input to Cropland.
#' Environmental Research Letters.
#' FAO for some N related parameters published in FAOSTAT.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' \dontrun{
#' calcOutput("ValidNitrogenBudgetCropland")
#' }
#'
#' @importFrom magpiesets reportingnames
calcValidNitrogenBudgetCropland <- function(datasource = "Bodirsky") {
  if (datasource == "Bodirsky") {
    budget <- calcOutput("NitrogenBudgetCropland", aggregate = FALSE)
    budget[, , "som"] <- -budget[, , "som"]
    all <- getNames(budget)


    withdrawaltypes <- c("harvest", "ag", "bg")
    balancetypes <- c("surplus", "som", "balanceflow")
    inputtypes <- setdiff(setdiff(all, withdrawaltypes), balancetypes)

    tmp <- budget[, , inputtypes]
    getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|", reportingnames(getNames(tmp)))
    inputs <- mbind(
      setNames(dimSums(tmp, dim = 3), "Resources|Nitrogen|Cropland Budget|Inputs"),
      tmp
    )

    tmp <- budget[, , withdrawaltypes]
    getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|+|", reportingnames(getNames(tmp)))
    withdrawals <- mbind(
      setNames(dimSums(tmp, dim = 3), "Resources|Nitrogen|Cropland Budget|Withdrawals"),
      tmp
    )

    tmp <- budget[, , balancetypes]
    getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Balance|+|", reportingnames(getNames(tmp)))
    balance <- mbind(
      setNames(dimSums(tmp, dim = 3), "Resources|Nitrogen|Cropland Budget|Balance"),
      tmp
    )

    out <- mbind(
      inputs,
      withdrawals,
      balance
    )

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  } else if (datasource == "ACCMIP") {
    dep <- calcOutput("AtmosphericDeposition", datasource = "ACCMIP",
                      scenario = c("rcp26", "rcp45", "rcp85"), aggregate = FALSE)
    out <- dep
    out <- dimSums(out, dim = c(3.1, 3.3, 3.4))
    out <- add_dimension(out, dim = 3.2, add = "indicator",
                         nm = "Resources|Nitrogen|Cropland Budget|Inputs|+|Atmospheric Deposition")
    getSets(out)[3] <- "scenario"
  } else if (datasource == "Lassaletta2014") {
    budget <- readSource("Lassaletta2014", subtype = "budget")
    out <- mbind(
      setNames(budget[, , "harvest"], paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|+|",
                                             reportingnames(getNames(budget[, , "harvest"])))),
      setNames(budget[, , "harvest", invert = TRUE],
               paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|",
                      reportingnames(getNames(budget[, , "harvest", invert = TRUE]))))
    )
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  } else if (datasource == "FAO") {
    residues <- readSource("FAO_online", "EmisAgCropResid")
    ## New units are provided in kg by FAO so kg to Mt should be 1e9
    residues <- setNames(residues[, , "Residues_(Crop_residues)_(kg_of_nutrients)"][, , "1712|All Crops"]
                         / 1e9, "ag_recycling")

    manure <- readSource("FAO_online", "EmisAgManureSoil")
    selection <- c(
      "1053|Chickens, broilers",
      "1068|Ducks",
      "1079|Turkeys",
      "1052|Chickens, layers",
      "946|Buffaloes",
      "1760|Camels and Llamas",
      "960|Cattle, dairy",
      "1016|Goats",
      "976|Sheep",
      "1759|Mules and Asses",
      "1051|Swine, breeding",
      "1096|Horses",
      "1049|Swine, market",
      "961|Cattle, non-dairy"
    )
    tmp2 <- collapseNames(manure[, , selection][, , "Manure_applied_to_soils_(N_content)_(kg)"])
    mapping <- toolGetMapping(type = "sectoral", name = "IPCCitems_fao_online.csv", where = "mappingfolder")
    tmp2 <- toolAggregate(tmp2, rel = mapping, from = "fao", to = "magpie", dim = 3.1)
    tmp2 <- tmp2 / 1e9
    manure <- setNames(dimSums(tmp2, dim = 3.1), "manure_conf")

    # fertilizer
    fertilizer <- (readSource("FAO_online", "EmisAgSynthFerti")
    )[, , c("3102|Nutrient nitrogen N (total).Agricultural_Use_in_nutrients_(kg_of_nutrients)")]
    fertilizer <- dimSums(fertilizer, dim = c(3)) / 1e9
    fertilizer <- setNames(fertilizer, "fertilizer")
    commonyears <- intersect(intersect(getYears(fertilizer), getYears(residues)), getYears(manure))

    out <- mbind(
      residues[, commonyears, ],
      manure[, commonyears, ],
      fertilizer[, commonyears, ]
    )

    vcat(2, "instead of removing years with no data, each object should rather be blown up to full dimensionality")

    namesX <- reportingnames(getNames(out))
    names(namesX) <- NULL
    getNames(out) <- paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|", namesX)
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  } else {
    stop("No data exist for the given datasource!")
  }
  out <- add_dimension(out, dim = 3.1, add = "model", nm = datasource)
  getNames(out) <- paste0(getNames(out), " (Mt Nr/yr)")

  return(list(x = out,
              weight = NULL,
              unit = "Mt Nr/yr",
              description = "Cropland Nitrogen Budget")
  )
}
