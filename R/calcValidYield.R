#' @title calcValidYield
#' @description
#' Calculates a dataset of agricultural production out of the combined
#' data of calcFAOharmonized(). Covers dry matter (DM) production.
#' Also returns areas of individual crops from FAOSTAT.
#' Total area can be lower or higher than arable land because of multicropping or fallow land.
#' Returns yield as calculated from area area and production.
#'
#' @param datasource Specify which datasource needs to be used.
#'                   Currently only "FAO" and "calibratedLPJmL" is available.
#' @param future if NULL no future values are returned (default).
#'               specify climate scenario (gcm:rcp), if future is needed
#'
#' @return List of magpie objects with results on country level, weight on country level, unit, Max.&Min. values alongwith description.
#' @author Abhijeet Mishra, Isabelle Weindl
#' @seealso
#' \code{\link{calcFAOmassbalance}},
#' \code{\link{calcCroparea}}
#'
#' @examples
#'
#' \dontrun{
#' calcOutput("ValidYield")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @importFrom magpiesets reporthelper summationhelper findset

calcValidYield  <-  function(datasource="FAO", future = NULL){


  if (datasource=="FAO") {

    if (!is.null(future)) stop("Future options is not available for source type 'FAO'.")

    past <- findset("past")

    #Calculate areas of individual crops and pasture
    croparea  <-  collapseNames(calcOutput("Croparea", sectoral="kcr", physical=TRUE, aggregate = FALSE)[,past,])
    pastarea  <-  setNames(calcOutput("LanduseInitialisation", aggregate=FALSE)[,,"past"],"pasture")
    area <- mbind(croparea,pastarea)
    area <- summationhelper(reporthelper(area,level_zero_name = "Productivity|Yield"))

    #Calculate production
    histproduction <- calcOutput("FAOmassbalance",aggregate = FALSE)
    #extract DryMatter(dm) from production data
    #Extract Production from subsetted production data containing only DryMatter(dm)
    histproduction  <-  collapseNames(histproduction[,,"dm"][,,"production"])

    #Figure out which commodities are common and make sure that production and area has same commodity names
    kcr <- findset("kcr")
    cropproduction  <-  histproduction[,,kcr]
    pastproduction  <-  histproduction[,,"pasture"]
    production <- mbind(cropproduction,pastproduction)

    #Calculate Yields
    production <- summationhelper(reporthelper(production, level_zero_name = "Productivity|Yield"))
    yield      <-  production / area

    #Check for NaN values
    NaNindex  <-  which(is.nan(yield))
    #Change NaN to 0
    yield[is.nan(yield)] = 0
    if (length(NaNindex)>0) {
      vcat(verbosity = 2, "NaN values were found in the calculated yields ---> NaN values set to 0")
    } else if(length(NaNindex) == 0){
      vcat(verbosity = 2, "No NaN values detected in calculated yields. Yields for area reported as 0 are converted to 0.")
    }

    #Check for infinite values - Production reported but on 0 area
    infindex  <-  which(is.infinite(yield)) #Lots of inf values. Being too diplomatic FAO aren't we!!
    #Change inf to 0
    yield[is.infinite(yield)] = 0
    if (length(infindex)>0) {
      vcat(verbosity = 2, "inf values were found in the calculated yields ---> inf values set to 0")
    } else if (length(infindex)==0) {
      vcat(verbosity = 2, "No inf values found in the calculated yields")
    }

    ## Additional checks required because we are reporting insanely high yields
    ## which(yield>200,arr.ind = TRUE)
    ## looks like Vatican are the best producers of oil

    ## We decided to set these outliers to 0
    R <- rownames( which(yield>200,arr.ind = TRUE))
    Y <- which(yield>200,arr.ind = TRUE)[,2]
    V <- which(yield>200,arr.ind = TRUE)[,3]

    ## In the above expression
    ## rownames( which(yield>200,arr.ind = TRUE)) looks for the (R)egion names which are outliers
    ## which(yield>200,arr.ind = TRUE)[,2]        looks for the (Y)ears where reporting is flawed
    ## which(yield>200,arr.ind = TRUE)[,3]        looks for the scenario.model.(V)ariable where reporting is flawed

    ## Now do the operation of our magpie object
    yield[R,Y,V]  <-  0
    vcat(verbosity = 2, "Yields>200 ton/ha were converted to 0 when area and production values were ambiguous")

    scenario    <- "historical"
    description <- "CalcValidYield calculates the historical yield from FAO database."

  } else if (datasource == "calibratedLPJmL") {

    irrigation=FALSE # can be made function argument if needed
    yieldLPJmL_grid <- calcOutput("ValidGridYields", datasource = "calibratedLPJmL", future = future, aggregate = FALSE)
    areaMAG_grid    <- setYears(calcOutput("ValidGridCroparea", aggregate = FALSE)[, "y2010", ], NULL)
    CountryToCell   <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")

    yield <- toolAggregate(yieldLPJmL_grid, weight = areaMAG_grid, rel = CountryToCell,
                           from = "celliso", to = "iso", dim = 1)
    area <- toolAggregate(areaMAG_grid, weight = NULL, rel = CountryToCell,
                           from = "celliso", to = "iso", dim = 1)


    rm(yieldLPJmL_grid, areaMAG_grid)

    if(irrigation==FALSE){
      yield <- dimSums(yield*area,dim="data1")/(dimSums(area,dim="irrigation")+10^-10)
      area <- dimSums(area,dim="irrigation")
    }

    yield <- toolCountryFill(yield, 0)
    area <- toolCountryFill(area, 0)
    yield <- setNames(yield, paste0("Productivity|Yield|", gsub("\\.", "|", getNames(yield))))
    area <- setNames(area, paste0("Productivity|Yield|", gsub("\\.", "|", getNames(area))))

    scenario    <- "projection"
    description <- "CalcValidYield calculates the projected yields from LPJmL calibrated to FAO."

  } else{
    stop("specified datasource doesn't exist")
  }

  #Change the name of calculated element to YIELD
  getNames(yield)  <-  paste0(getNames(yield)," (t DM/ha)" ) #Units added to be consistent with getReport from gdx file.

  #Set areas as weight
  weight  <-  area + 10^-10
  getNames(weight)  <-  paste0(getNames(weight)," (t DM/ha)" )    #Important to change the names again so that ".dimextract" doesn't get confused

  #rename dimensions to be consistent with MAgPIE coding etiquette
  yield  <-  add_dimension(yield, dim=3.1, add="scenario", nm=scenario)
  yield  <-  add_dimension(yield, dim=3.2, add="model", nm=datasource)

  names(dimnames(yield))[3]  <-  "scenario.model.variable"

  return(list(x=yield,
              weight=weight,
              unit="t DM/ha",
              max = 200,
              min = 0,
              description=description) )
}
