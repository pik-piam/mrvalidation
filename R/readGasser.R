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
#' @importFrom madrat toolSubtypeSelect toolGetMapping
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom magclass time_interpolate

readGasser <- function(subtype = "bookkeeping") {
  if (subtype == "regional") {
    # C:/PIK/data_processing/inputdata/sources/Gasser/
    file <- "Bookkeeping LULCC emissions (ELUC).csv"

    dataset <- read.csv(file = file, header = TRUE, skip = 7, sep = ";")
    x <- as.magpie(dataset, spatial = "Region")
    x <- x[, , grep(pattern = "U_", x = getNames(x), value = TRUE, invert = TRUE)]

    mapping_file_gasser <- "GasserMapping.csv"
    mapping_gasser <- read.csv(file = mapping_file_gasser, header = TRUE, sep = ";")
    mapping_magpie <- toolGetMapping(type = "regional", name = "h12.csv")
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
    eluc_raw <- ncvar_get(dataset, "Eluc")
    dimnames(eluc_raw)[[1]] <- dataset$var$Eluc$dim[[1]]$vals
    dimnames(eluc_raw)[[2]] <- dataset$var$Eluc$dim[[2]]$vals
    dimnames(eluc_raw)[[3]] <- dataset$var$Eluc$dim[[3]]$vals
    dimnames(eluc_raw)[[4]] <- ncvar_get(dataset, attributes(dataset$var)$names[1])
    dimnames(eluc_raw)[[5]] <- dataset$var$Eluc$dim[[5]]$vals
    
    names(dimnames(eluc_raw))[1] <- "bio_pool"
    names(dimnames(eluc_raw))[2] <- "bio_to"       ### Strangely this is bio to and not bio from
    names(dimnames(eluc_raw))[3] <- "bio_from"     ### This dim is bio_from
    ## run eluc_raw["Veg","Forest","Cropland",,"1709"] and check in netcdf file if
    ## Bio from    is set to cropland
    ## bio to      is set to Forest
    ## bio pool    is set to Veg
    ## Check year 1709
    names(dimnames(eluc_raw))[4] <- "reg_land"
    names(dimnames(eluc_raw))[5] <- "year"
    
    eluc_df <- as.data.frame.table(eluc_raw)
    
    eluc_df$Freq[eluc_df$bio_to == "Urban"] <- 0    ## THIS LINE SHOULD BE COMMENTED OUT IF URBAN LAND IS DYNAMIC IN MAGPIE
    eluc_df$Freq[eluc_df$bio_from == "Urban"] <- 0  ## THIS LINE SHOULD BE COMMENTED OUT IF URBAN LAND IS DYNAMIC IN MAGPIE
    message("CAUTION: Emissions related to/from Urban land have been removed from source data.") ## THIS LINE SHOULD BE COMMENTED OUT IF URBAN LAND IS DYNAMIC IN MAGPIE
    
    eluc <- gasser <- as.magpie(eluc_df, temporal = "year", spatial = "reg_land")
    
    ## From Gasser paper (https://doi.org/10.5194/bg-17-4075-2020)
    ## Category 1  corresponds to land cover change (LCC) where forest is replaced by cropland. 
    ## Category 2 is LCC where forest is replaced by anything else (but forest). 
    ## Category 3 is the opposite of 1 and 2: LCC where any type of land but forest is replaced by forest. 
    ## Category 4 is LCC where non-forested natural land is replaced by any anthropogenic land. 
    ## Category 5 is the opposite of 4. 
    ## Category 6 is any LCC occurring among anthropogenic land (e.g., pasture to cropland). 
    ## Category 7 is the sum of wood harvest and LCC occurring from any type of natural land to the same 
    ## type of natural land (e.g., forest to forest). 
    ## Note that the effects of shifting cultivation are included in their corresponding LCC categories due to the model’s structure.
    
    ## From table 5, 
    ## category 1,2,4,6,7 are Gross LUC emissions in MAgPIE
    ## category 3,5 are regrowth emissions in MAgPIE
    
    category1 <- dimSums(gasser[,,"Cropland.Forest"],dim=3)
    category2 <- dimSums(gasser[,,paste0(c("Non-Forest","Pasture","Urban"),".Forest")],dim=3)
    category3 <- dimSums(gasser[,,paste0("Forest.",c("Non-Forest","Cropland","Pasture","Urban"))],dim=3)
    category4 <- dimSums(gasser[,,paste0(c("Cropland","Pasture","Urban"),".Non-Forest")],dim=3)
    category5 <- dimSums(gasser[,,paste0("Non-Forest.",c("Cropland","Pasture","Urban"))],dim=3)
    category6 <- dimSums(gasser[,,c("Cropland.Pasture","Pasture.Cropland","Cropland.Urban","Urban.Cropland","Pasture.Urban","Urban.Pasture")],dim=3)
    category7 <- dimSums(gasser[,,c("Forest.Forest","Non-Forest.Non-Forest")],dim=3)
    
    gross_luc    <- setNames(category1 + category2 + category4 + category6,"gross_luc_emis")
    regrowth_luc <- setNames(category3 + category5,"regrowth_luc_emis")
    
    eluc <- mbind(gross_luc,regrowth_luc)[,1850:2018,]
    
    mapping_file_gasser <- "gasser_mapping_git.csv"
    mapping_gasser <- read.csv(file = mapping_file_gasser, header = TRUE, sep = ";")

    weight <- collapseNames(readSource("FRA2020","forest_area")[,,"forestArea"])
    missing_weight_years <- setdiff(getYears(eluc), getYears(weight))
    weight <- time_interpolate(dataset = weight, interpolated_year = missing_weight_years, extrapolation_type = "linear", integrate_interpolated_years = T)
    weight[weight<0] <- 0
    iso_list <- intersect(getRegions(weight), unique(mapping_gasser$ISO.Alpha3))
    mapping_gasser <- mapping_gasser[mapping_gasser$ISO.Alpha3 %in% iso_list, ]
    regrowth <- toolAggregate(x = eluc[,,"regrowth_luc_emis"], rel = mapping_gasser, weight = weight[iso_list, getYears(eluc), ], from = "GasserReg", to = "ISO.Alpha3")
    
    weight <- collapseNames(readSource("FRA2020","forest_area")[,,"landArea"])
    missing_weight_years <- setdiff(getYears(eluc), getYears(weight))
    weight <- time_interpolate(dataset = weight, interpolated_year = missing_weight_years, extrapolation_type = "constant", integrate_interpolated_years = T)
    iso_list <- intersect(getRegions(weight), unique(mapping_gasser$ISO.Alpha3))
    mapping_gasser <- mapping_gasser[mapping_gasser$ISO.Alpha3 %in% iso_list, ]
    gross <- toolAggregate(x = eluc[,,"gross_luc_emis"], rel = mapping_gasser, weight = weight[iso_list, getYears(eluc), ], from = "GasserReg", to = "ISO.Alpha3")
    
    overall <- setNames(gross+regrowth,"overall") ## Regrowth are negative, gross are positive
    
    out <- mbind(gross,regrowth,overall)

    return(out)
  } else {
    stop("Invalid subtype.")
  }
}
