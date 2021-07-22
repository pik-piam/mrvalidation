#' @title calcValidLand
#'
#' @description Returns historical cropland, pasture and forest area from FAOSTAT that can
#' be used for model validation.
#'
#' @param datasource Currently available: \code{"FAO"}, \code{"LUH2v2"}, \code{"MAgPIEown"} and \code{"SSPResults"}
#' @return list of magpie object with data and weight
#' @author Ulrich Kreidenweis, Benjamin Bodirsky, Abhijeet Mishra, Mishko Stevanovic, Kristine Karstens
#' @importFrom utils read.csv
#' @importFrom magpiesets reportingnames
#' @importFrom magpiesets findset
#' @importFrom magclass time_interpolate
#'
calcValidLand <- function(datasource="MAgPIEown"){

  if(datasource=="FAO_crop_past") {

    FAOLand <- calcOutput("FAOLand",aggregate = FALSE)
    data    <- collapseNames(FAOLand[,,c("6620", "6655"), pmatch=TRUE])
    getNames(data) <- c("crop", "past")

    out <- data[,,c("crop","past")]
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    getNames(out,dim=3)<-paste0("Resources|Land Cover|+|",reportingnames(getNames(out,dim=3))," (million ha)")
  } else if(datasource=="FAO_forest") {

    FRAForest   <- readSource("FAO_FRA2015","fac")[,,c("Forest")]
    y_past <- magpiesets::findset("past",noset = "original")
    y_past <- as.integer(substring(y_past,2,5))
    y_data <- getYears(FRAForest, as.integer=TRUE)
    y_interpolate <- union(y_past[y_past >= min(y_data)], y_data)
    FRAForest <- time_interpolate(FRAForest,interpolated_year=y_interpolate,integrate_interpolated_years = TRUE,extrapolation_type = "constant")

    # FAOLand <- calcOutput("FAOLand",aggregate = FALSE)
    # data <- collapseNames(FAOLand[,,c("6661|Forest area")])
    # getNames(data) <- c("forest")

    out <- setNames(FRAForest,"forest")
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    getNames(out,dim=3)<-paste0("Resources|Land Cover|+|",reportingnames(getNames(out,dim=3))," (million ha)")
  } else if(datasource=="FRA2020") {

    FRAForest2020   <- readSource("FRA2020","forest_area")[,,c("plantedForest","plantationForest","otherPlantedForest")]
    getNames(FRAForest2020,dim=1) <- c("Resources|Land Cover|Forest|Managed Forest","Resources|Land Cover|Forest|Managed Forest|+|Plantations","Resources|Land Cover|Forest|Managed Forest|+|NPI/NDC")
    y_past <- magpiesets::findset("past",noset = "original")
    y_past <- as.integer(substring(y_past,2,5))
    y_data <- getYears(FRAForest2020, as.integer=TRUE)
    y_interpolate <- union(y_past[y_past >= min(y_data)], y_data)
    FRAForest2020 <- time_interpolate(FRAForest2020,interpolated_year=y_interpolate,integrate_interpolated_years = TRUE,extrapolation_type = "constant")
    out <- FRAForest2020
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    getNames(out,dim=3)<-paste0(getNames(out,dim=3)," (million ha)")
  } else if(datasource=="LUH2v2") {

    data <- calcOutput("LUH2v2",landuse_types="magpie",irrigation=FALSE,cellular=FALSE,selectyears="past",aggregate = FALSE)
    out <- data[,,c("crop","past","urban","other","forest")]
    getNames(out,dim=1)<-paste0("Resources|Land Cover|+|",reportingnames(getNames(out,dim=1))," (million ha)")
    out <- mbind(out,setNames(dimSums(out,dim=3),"Resources|Land Cover (million ha)"))
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)

  } else if(datasource=="MAgPIEown") {

    x <- calcOutput("LanduseInitialisation", nclasses="seven", fao_corr=TRUE,aggregate = FALSE)
    x <- mbind(x,setNames(dimSums(x[,,c("primforest","secdforest","forestry")],dim=3),"forest"))
    
    main_cat <- c("crop","past","urban","other","forest")
    main <- x[,,main_cat]
    getNames(main,dim=1)<-paste0("Resources|Land Cover|+|",reportingnames(getNames(main[,,main_cat],dim=1))," (million ha)")
    main <- mbind(main,setNames(dimSums(main,dim=3),"Resources|Land Cover (million ha)"))
    
    grassland <- setNames(calcOutput("LanduseInitialisation", nclasses="nine", fao_corr=TRUE,aggregate = FALSE)[,,c("past", "range")], c("pastr", "range"))
    grassland <- setNames(grassland, paste0("Grassland Management|+|",reportingnames(getNames(grassland,dim=1))," (million ha)"))

    forest <- c("primforest","secdforest","forestry")
    forest <- x[,,forest]
    forest <- mbind(forest,setNames(dimSums(x[,,c("primforest","secdforest")],dim=3),"natrforest"))
    natrforest <- forest[,,c("primforest","secdforest")]
    forest <- forest[,,c("forestry","natrforest")]
    getNames(forest,dim=1)<-paste0("Resources|Land Cover|Forest|+|",reportingnames(getNames(forest,dim=1))," (million ha)")
    getNames(natrforest,dim=1)<-paste0("Resources|Land Cover|Forest|Natural Forest|+|",reportingnames(getNames(natrforest,dim=1))," (million ha)")

    out <- mbind(main, forest,natrforest, grassland)
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
   } else if(datasource=="SSPResults") {

     # Pick out Land Cover categories in SSPResults
     out <- calcOutput("ValidSSPResults", aggregate = FALSE)
     selection <- c(
       "Land Cover (million ha)",
       "Land Cover|Built-up Area (million ha)",
       "Land Cover|Cropland (million ha)",
       "Land Cover|Cropland|Energy Crops (million ha)",
       "Land Cover|Forest (million ha)",
       "Land Cover|Forest|Forestry (million ha)",
       "Land Cover|Forest|Forestry|Harvested Area (million ha)",
       "Land Cover|Forest|Natural Forest (million ha)",
       "Land Cover|Other Arable Land (million ha)",
       "Land Cover|Other Land (million ha)",
       "Land Cover|Other Natural Land (million ha)",
       "Land Cover|Pasture (million ha)"
     )

     out <- out[,,selection]

     # # Pick out just ref-Runs for SSP markermodel results
     # SSPscenario <- "-Ref-SPA0-V15"
     # SSPmarkermodel <- list(c("SSP1","IMAGE"),c("SSP2","MESSAGE-GLOBIOM"),c("SSP3","AIM/CGE"),c("SSP4","GCAM4"),c("SSP5","REMIND-MAGPIE"))
     #
     # a <- NULL
     #
     # for(i in (1:length(SSPmarkermodel))){
     #   a <- mbind(a, out[,,paste0(SSPmarkermodel[[i]][1],SSPscenario)][,,SSPmarkermodel[[i]][2]])
     # }
     # out <- a

     # Renaming reporting categories from SSPResults to MAgPie validation names (not all SSP-categories have MAgPIE-equivalents)

     mapping <- toolGetMapping(type = "sectoral", name = "mappingSSPResultsToMAgPIEValid.csv")
     mapping_from <- as.vector(mapping[,"SSPResults"])
     mapping_to <- as.vector(mapping[,"MAgPIEValid"])
     names(mapping_to) <- mapping_from
     aNames <- getNames(out,dim=3)
     getNames(out,dim=3) <- mapping_to[aNames]
     getNames(out,dim=3) <- paste0("Resources|",getNames(out,dim=3))



  } else {

    stop("Given datasource currently not supported!")
  }

  names(dimnames(out))[3] <- "scenario.model.variable"
  names(dimnames(out))[1] <- "ISO"

  return(list(x=out,
              weight=NULL,
              unit="million ha",
              min=0,
              description="Cropland, pasture, urban, other land and forest area from FAO, LUH2v2, MAgPIE-Input and SSPresults.
              Cropland: is the land under temporary agricultural crops (multiple-cropped areas are counted only once), temporary meadows for mowing or pasture, land under market and kitchen gardens and land temporarily fallow, and cultivated with long-term crops which do not have to be replanted for several years (such as cocoa and coffee); land under trees and shrubs producing flowers, such as roses and jasmine;
              Pasture: is the land used permanently (for a period of five years or more) for herbaceous forage crops, either cultivated or naturally growing.
              Forest: is the land spanning more than 0.5 hectares with trees higher than 5 metres and a canopy cover of more than 10 percent (includes temporarily unstocked areas)")
  )
}
