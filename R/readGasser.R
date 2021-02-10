#' @title Gasser
#' @description Read historic land-use change CARBON emissions
#'
#' @param subtype subtype
#' @return magpie object containing data land-use change CARBON emissions
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#'
#' \dontrun{
#' a <- readSource("Gasser")
#' }
#' @importFrom madrat toolSubtypeSelect toolMappingFile
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom magclass time_interpolate

readGasser <- function(subtype = "regional") {
  if (subtype == "regional") {
    # C:/PIK/data_processing/inputdata/sources/Gasser/
    file <- "Bookkeeping LULCC emissions (ELUC).csv"

    dataset <- read.csv(file = file, header = TRUE, skip = 7, sep = ";")
    x <- as.magpie(dataset, spatial = "Region")
    x <- x[, , grep(pattern = "U_", x = getNames(x), value = TRUE, invert = TRUE)]

    mapping_file_gasser <- "GasserMapping.csv"
    mapping_gasser <- read.csv(file = mapping_file_gasser, header = TRUE, sep = ";")
    mapping_magpie <- toolMappingFile(type = "regional", name = "h12.csv", readcsv = TRUE)
    colnames(mapping_magpie)[1] <- "ISOName"
    mapping <- merge(mapping_gasser, mapping_magpie, all = FALSE)

    weight <- collapseNames(setYears(readSource("FRA2020", subtype = "forest_area", convert = TRUE)[, "y2010", "landArea"], NULL))

    out <- toolAggregate(x = x, rel = mapping, weight = weight[unique(mapping$CountryCode), , ], from = "Region", to = "CountryCode", dim = 1)

    getSets(out) <- c("region", "year", "source")
    getNames(out) <- c("Gasser_2020", "LUH2_GCB_2019", "FRA_2015")
    return(out)
  } else if (subtype == "global") {
    # C:/PIK/data_processing/inputdata/sources/Gasser/
    file <- "Bookkeeping LULCC emissions (ELUC) Disaggregated.csv"

    dataset <- read.csv(file = file, header = TRUE, skip = 7, sep = ";")
    x <- as.magpie(dataset)
    x <- x[, , grep(pattern = "U_", x = getNames(x, dim = 2), value = TRUE, invert = TRUE)]
    out <- x
    getSets(out) <- c("region", "year", "variable", "source")
    getNames(out, dim = 2) <- c("Gasser_2020", "LUH2_GCB_2019", "FRA_2015")
    return(out)
  } else if (subtype == "bookkeeping") {
    # C:/PIK/data_processing/inputdata/sources/Gasser/
    file <- "Gasser_et_al_2020_best_guess.nc"
    dataset <- nc_open(file)
    eluc <- as.data.frame.table(ncvar_get(dataset, "Eluc"))
    colnames(eluc) <- c("pool", "from", "to", "region", "year", "value")
    levels(eluc$pool) <- dataset$var$Eluc$dim[[1]]$vals
    levels(eluc$from) <- dataset$var$Eluc$dim[[2]]$vals
    levels(eluc$to) <- dataset$var$Eluc$dim[[3]]$vals
    levels(eluc$region) <- ncvar_get(dataset, attributes(dataset$var)$names[1])
    levels(eluc$year) <- dataset$var$Eluc$dim[[5]]$vals

    eluc <- as.magpie(eluc, temporal = "year", spatial = "region")
    
    eluc <- mbind(setNames(dimSums(eluc[, , c("HWP.Forest.Non-Forest")],dim=3),"hwp"), 
                  setNames(dimSums(eluc[, , c("Veg.Forest.Forest")],dim=3),"regrowth"),
                  setNames(dimSums(eluc[, , ],dim = 3),"overall"))

    mapping_file_gasser <- "gasser_mapping_git.csv"
    mapping_gasser <- read.csv(file = mapping_file_gasser, header = TRUE, sep = ";")

    weight <- collapseNames(calcOutput("EndUseTimber", aggregate = FALSE)[, , "industrial_roundwood"])
    missing_weight_years <- setdiff(getYears(eluc), getYears(weight))
    weight <- time_interpolate(dataset = weight, interpolated_year = missing_weight_years, extrapolation_type = "constant", integrate_interpolated_years = T)
    iso_list <- intersect(getRegions(weight), unique(mapping_gasser$ISO.Alpha3))
    mapping_gasser <- mapping_gasser[mapping_gasser$ISO.Alpha3 %in% iso_list, ]
    hwp <- toolAggregate(x = eluc[,,"hwp"], rel = mapping_gasser, weight = weight[iso_list, getYears(eluc), ], from = "GasserReg", to = "ISO.Alpha3")
    
    weight <- collapseNames(readSource("FRA2020","forest_area")[,,"forestArea"])
    missing_weight_years <- setdiff(getYears(eluc), getYears(weight))
    weight <- time_interpolate(dataset = weight, interpolated_year = missing_weight_years, extrapolation_type = "constant", integrate_interpolated_years = T)
    iso_list <- intersect(getRegions(weight), unique(mapping_gasser$ISO.Alpha3))
    mapping_gasser <- mapping_gasser[mapping_gasser$ISO.Alpha3 %in% iso_list, ]
    regrowth <- toolAggregate(x = eluc[,,"regrowth"], rel = mapping_gasser, weight = weight[iso_list, getYears(eluc), ], from = "GasserReg", to = "ISO.Alpha3")
    
    weight <- collapseNames(readSource("FRA2020","forest_area")[,,"landArea"])
    missing_weight_years <- setdiff(getYears(eluc), getYears(weight))
    weight <- time_interpolate(dataset = weight, interpolated_year = missing_weight_years, extrapolation_type = "constant", integrate_interpolated_years = T)
    iso_list <- intersect(getRegions(weight), unique(mapping_gasser$ISO.Alpha3))
    mapping_gasser <- mapping_gasser[mapping_gasser$ISO.Alpha3 %in% iso_list, ]
    overall <- toolAggregate(x = eluc[,,"overall"], rel = mapping_gasser, weight = weight[iso_list, getYears(eluc), ], from = "GasserReg", to = "ISO.Alpha3")
    
    out <- mbind(hwp,regrowth,overall)

    return(out)
  } else {
    stop("Invalid subtype.")
  }
}
