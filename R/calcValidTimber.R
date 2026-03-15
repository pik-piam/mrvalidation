#' calcValidTimber
#'
#' Returns historical timber demand and production from FAO in both
#' volumetric (Mm3/yr) and mass (mio tDM) units.
#' Variable names match reportTimber (Mm3) and reportTimberDemand (tDM).
#'
#' @param datasource Currently only available for the "FAO" source
#' @return List of magpie object with data and population
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @import magpiesets
#' @importFrom magclass getNames mbind collapseNames add_dimension dimSums setNames
#'
calcValidTimber <- function(datasource = "FAO") {
  if (datasource == "FAO") {
    timberDemand <- calcOutput("TimberDemand", aggregate = FALSE)
    dem <- collapseNames(timberDemand[, , "domestic_supply"])
    prod <- collapseNames(timberDemand[, , "production"])

    ## --- Volumetric (Mm3/yr) --- matches reportTimber variable names
    unit <- "Mm3/yr"

    # Demand: product-level
    demVol <- dem
    getNames(demVol) <- paste0("Timber|Volumetric|Demand|Roundwood|+|", getNames(demVol))
    # Demand: total
    demVolTotal <- setNames(dimSums(demVol, dim = 3),
                            "Timber|Volumetric|Demand|+|Roundwood")
    demVol <- mbind(demVolTotal, demVol)
    getNames(demVol) <- paste0(getNames(demVol), " (", unit, ")")

    # Production: product-level
    prodVol <- prod
    getNames(prodVol) <- paste0("Timber|Volumetric|Production|Roundwood|+|", getNames(prodVol))
    # Production: total
    prodVolTotal <- setNames(dimSums(prodVol, dim = 3),
                             "Timber|Volumetric|Production|+|Roundwood")
    prodVol <- mbind(prodVolTotal, prodVol)
    getNames(prodVol) <- paste0(getNames(prodVol), " (", unit, ")")

    ## --- Mass (mio tDM) --- matches reportTimberDemand variable names
    # Apply IPCC climate-zone wood density and stacking factor (same as MAgPIE module 73)
    woodDensity <- calcOutput("WoodDensity", aggregate = FALSE)
    stackingFactor <- 0.65

    demTdm <- dem
    demTdm[, , "Industrial roundwood"] <- demTdm[, , "Industrial roundwood"] * woodDensity
    demTdm[, , "Wood fuel"] <- demTdm[, , "Wood fuel"] * stackingFactor * woodDensity
    getNames(demTdm) <- paste0("Timber demand|", getNames(demTdm))
    demTdmTotal <- setNames(dimSums(demTdm, dim = 3), "Timber demand|Roundwood")
    demTdm <- mbind(demTdmTotal, demTdm)
    getNames(demTdm) <- paste0(getNames(demTdm), " (mio tDM)")

    out <- mbind(demVol, prodVol, demTdm)

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = "FAOSTAT FO")

  } else {
    stop("No data exist for the given datasource!")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mm3/yr, mio tDM",
    description = "Timber demand and production from FAO data (volumetric and mass)"
  ))
}
