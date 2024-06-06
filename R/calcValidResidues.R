#' @title calcValidResidues
#' @description calculates the validation data for residues (biomass, field balance, demands)
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidResidues")
#' }
#'
#' @importFrom magpiesets reportingnames summationhelper

calcValidResidues <- function() {

  ### getting all relevant data
  selectAttr <- c("c")
  kcr2kres   <- toolGetMapping("mappingCrop2Residue.csv", type = "sectoral", where = "mrcommons")

  biomass      <- calcOutput("ResBiomass", plantparts = "both", attributes = "all", aggregate = FALSE)
  biomass      <- toolAggregate(collapseDim(biomass[, , selectAttr]),
                                from = "kcr", to = "kres", rel = kcr2kres, dim = 3.2)
  fieldBalance <- calcOutput("ResFieldBalancePast", products = "kres", aggregate = FALSE)
  fieldBalance <- collapseDim(fieldBalance[, , selectAttr][, , c("removal", "recycle", "burned")])
  resDemand    <- calcOutput("ResDemand", aggregate = FALSE)
  resDemand    <- collapseDim(resDemand[, , selectAttr][, , c("feed", "other_util", "bioenergy")])

  ### helper function
  .reportNicely <- function(x, levelZeroName = NULL, unit = NULL, summation = TRUE, sep = "+") {
    x <- setNames(x, paste0(levelZeroName, "|", gsub("\\.", "|", getItems(x, dim = 3)), " ", unit))
    if (summation == TRUE) x <- magpiesets::summationhelper(x, sep = sep)
    return(x)
  }

  prefix     <- "Resources|Carbon|Cropland|Residues"
  agPre      <- paste0("|", magpiesets::reportingnames("ag"))
  removalPre <- paste0("|", magpiesets::reportingnames("removal"))
  unit       <- "(Mt C/yr)"

  .getCommonYears <- function(listOfYearVectors) {
    nullIndex         <- which(vapply(listOfYearVectors, is.null, logical(1)))
    if (length(nullIndex) != 0) {
      vcat(1, "There are objects with no years (NULL) provided.")
      listOfYearVectors <- listOfYearVectors[-nullIndex]
    }
    commonYears       <- Reduce(intersect, listOfYearVectors)
    if (length(commonYears) == 0) vcat(0, "There are no common years objects provided.")
    return(commonYears)
  }

  ### Biomass reporting
  getItems(biomass, dim = 3.1) <- magpiesets::reportingnames(getItems(biomass, dim = 3.1))
  getItems(biomass, dim = 3.2) <- magpiesets::reportingnames(getItems(biomass, dim = 3.2))
  biomassPoolTotal <- dimSums(biomass, dim = 3.2)
  biomassTotal     <- dimSums(biomass, dim = 3)

  biomass          <- .reportNicely(biomass,          prefix, unit)
  biomassPoolTotal <- .reportNicely(biomassPoolTotal, prefix, unit)
  biomassTotal     <- setNames(biomassTotal, paste(prefix, unit))

  ### Field Balance
  getItems(fieldBalance, dim = 3.1) <- magpiesets::reportingnames(getItems(fieldBalance, dim = 3.1))
  getItems(fieldBalance, dim = 3.2) <- magpiesets::reportingnames(getItems(fieldBalance, dim = 3.2))
  fieldBalanceUseTotal <- dimSums(fieldBalance, dim = 3.2)

  fieldBalance         <- .reportNicely(fieldBalance,         paste0(prefix, agPre), unit)
  fieldBalanceUseTotal <- .reportNicely(fieldBalanceUseTotal, paste0(prefix, agPre), unit, sep = "++")

  ### Residue Demand
  getItems(resDemand, dim = 3.1) <- magpiesets::reportingnames(getItems(resDemand, dim = 3.1))
  getItems(resDemand, dim = 3.2) <- magpiesets::reportingnames(getItems(resDemand, dim = 3.2))
  resDemandCatTotal <- dimSums(resDemand, dim = 3.2)

  resDemand         <- .reportNicely(resDemand,         paste0(prefix, agPre, removalPre), unit)
  resDemandCatTotal <- .reportNicely(resDemandCatTotal, paste0(prefix, agPre, removalPre), unit, sep = "++")

  ### Create one output validation object

  years <- .getCommonYears(list(getYears(biomass),
                                getYears(fieldBalance),
                                getYears(resDemand)))

  out <- mbind(biomassTotal[, years, ],
               biomassPoolTotal[, years, ], fieldBalanceUseTotal[, years, ],
               biomass[, years, ],
               resDemandCatTotal[, years, ], fieldBalance[, years, ],
               resDemand[, years, ])

  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = "FAO-based-estimates")

  return(list(x           = out,
              weight      = NULL,
              unit        = "MtC/yr",
              description = "Residue biomass, usage and removal categories",
              min         = 0)
  )
}
