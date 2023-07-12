#' @title calcValidCostsTransport
#' @description calculates the validation data for transport costs
#'
#' @param datasource Datasource of validation data.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David Chen
#' @examples
#' \dontrun{
#' calcOutput("ValidCostsTransport")
#' }
#'
#' @import mrmagpie

#' @importFrom magpiesets reporthelper summationhelper
calcValidCostsTransport <- function(datasource = "GTAP") {

  if (datasource == "GTAP") {

  costs <- readSource("TransportCostsGTAP", convert = FALSE)
  getYears(costs) <- 2005
 missingProducts <- setdiff(findset("kall"), getNames(costs))

  out <- add_columns(costs, addnm = missingProducts, dim = 3.1)
  out[, , missingProducts] <- 0

 out <- reporthelper(out, dim = 3.1, level_zero_name = "Costs|Transport", detail = TRUE)
 out <- summationhelper(out)

 # take away + from total
 getNames(out[, , "Costs|+|Transport"]) <- "Costs|Transport"

 getNames(out) <- paste(getNames(out), "(million US$05/yr)", sep = " ")
  unit <- "million US$05/yr"
 unit <- "million US$05/yr"

 out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
 out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

} else if (datasource == "MAgPIEcalc") {

  distance <- calcOutput("TransportTime", aggregate = FALSE)
  productionKcr <- calcOutput("Production", cellular = TRUE, products = "kcr", attributes = "dm", aggregate = FALSE)
  productionKli <- calcOutput("Production", cellular = TRUE, products = "kli", attributes = "dm", aggregate = FALSE)
  productionPasture <- calcOutput("Production", cellular = TRUE,
                                  products = "pasture", attributes = "dm", aggregate = FALSE)
  productionPasture <- add_dimension(productionPasture, add = "pasture",
                                     nm = "pasture", dim = 3.1)
  production <- mbind(productionKcr, productionKli, productionPasture)

  productionDistance <- collapseNames(distance * production, collapsedim = 2)


  # costs per unit per distance
  costs <- readSource("TransportCostsGTAP", convert = FALSE)

  products <- intersect(getNames(productionDistance), getNames(costs))

  out <- costs[, , products] * productionDistance[, , products]

  mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell", where = "mappingfolder")
  out   <- toolAggregate(out, rel = mapping, from = "celliso", to = "iso", dim = 1)
  out   <- toolCountryFill(out, fill = 0)

# add missing product groups, so that report and summation helper work properly.
  # Note that forest, secondary, fish, bioenergy and  residues set to 0 currently
 missingProducts <- setdiff(findset("kall"), products)
  out <- add_columns(out, addnm = missingProducts, dim = 3.1)
  out[, , missingProducts] <- 0

 out <- reporthelper(out, dim = 3.1, level_zero_name = "Costs|Transport", detail = FALSE)
 out <- summationhelper(out)

 getNames(out[, , "Costs|+|Transport"]) <- "Costs|Transport"

 getNames(out) <- paste(getNames(out), "(million US$05/yr)", sep = " ")
 unit <- "million US$05/yr"

 out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
 out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  } else {
 stop("Only own calculation and GTAP transport costs avilable currently!")
}

  return(list(x = out,
              weight = NULL,
              unit = unit,
              description = "Transport Costs")
  )
}
