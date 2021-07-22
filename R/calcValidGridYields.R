#' @title calcValidGridYields
#' @description reports Yields on 0.5 degree grid
#' 
#' @param datasource downscaledFAO or calibratedFAO
#' @param future if NULL no future values are returned (default).
#'               specify climate scenario (gcm:rcp), if future is needed 
#' 
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullMADRATTOLPJML}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGridYields")
#' }
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<- clean_magpie
#' @importFrom madrat toolConditionalReplace

calcValidGridYields <- function(datasource = "downscaledFAO", future = NULL) {
  
  if (datasource == "downscaledFAO") {

    if (!is.null(future)) stop("Future options is not available for source type 'downscaledFAO'.")
    
    out <- calcOutput("FAOYield", cellular = TRUE, aggregate = FALSE, irrigation = TRUE)
    getNames(out, dim = 1) <- reportingnames(getNames(out, dim = 1))
    getNames(out, dim = 2) <- reportingnames(getNames(out, dim = 2))
    out <- clean_magpie(out)
    out <- dimOrder(out, perm = c(2, 1))
    getComment(out) <- NULL

  } else if (datasource == "calibratedLPJmL") {
    
    sizelimit <- getOption("magclass_sizeLimit")
    options(magclass_sizeLimit=1e+12)
    on.exit(options(magclass_sizeLimit=sizelimit))
    
    if (is.null(future)) climatetype <- "GSWP3-W5E5:historical" else climatetype <- future
    
    ref_year        <- "y2010"
    
    yieldFAO_iso    <- calcOutput("FAOYield", cut = 0.98, aggregate = FALSE)
    yieldLPJmL_grid <- calcOutput("Yields", source = c(lpjml = "ggcmi_phase3_nchecks_9ca735cb+oldGSWP3"),
                                  climatetype = climatetype, aggregate = FALSE)[, , findset("kcr")]
    
    areaMAG_grid    <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE, cellular = TRUE, 
                                  irrigation = TRUE, aggregate = FALSE)[, ref_year, ]
    CountryToCell   <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
    
    areaMAG_iso     <- toolAggregate(dimSums(areaMAG_grid, dim = 3.1), rel = CountryToCell, 
                                     from = "celliso", to = "iso", dim = 1)
    cropMAG_grid    <- dimSums(areaMAG_grid, dim = 3.2)
    
    yieldLPJmL_iso  <- toolAggregate(dimSums(yieldLPJmL_grid[, ref_year, ] * areaMAG_grid, dim = 3.2), 
                                             rel = CountryToCell, from = "celliso", to = "iso", dim = 1) /
                                                                                                   areaMAG_iso

    yieldLPJmL_iso[areaMAG_iso == 0] <- (toolAggregate(dimSums(yieldLPJmL_grid[, ref_year, ] * cropMAG_grid, dim = 3.2), 
                                                       rel = CountryToCell, from = "celliso", to = "iso", dim = 1) / 
                                                     dimSums(areaMAG_iso, dim = 3))[areaMAG_iso == 0]
    
    yieldLPJmL_iso <- toolConditionalReplace(yieldLPJmL_iso, "is.na()", 0)
    yieldLPJmL_iso <- toolIso2CellCountries(yieldLPJmL_iso)
    yieldFAO_iso   <- toolIso2CellCountries(yieldFAO_iso)
    
    out <- toolPatternScaling(yieldLPJmL_grid, yieldLPJmL_iso, yieldFAO_iso, ref_year = ref_year)
    
    getNames(out, dim = 1) <- reportingnames(getNames(out, dim = 1))
    getNames(out, dim = 2) <- reportingnames(getNames(out, dim = 2))
    out <- clean_magpie(out)
    getComment(out) <- NULL

  } else stop("Source not available.")
  
  return(list(x=out,
              weight=NULL,
              unit="t DM per ha physical area",
              description="Crop yields by plant type and irrigation",
              isocountries=FALSE)
  ) 
}

