#' @title calcValidGrassLSUha
#' @description calculates the validation data for production of grass from managed pastures and rangelands separetely
#' @param datasource Currently available: \code{"MAgPIEown"}
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Marcos Alves
#' @seealso
#' \code{\link{calcFAOmassbalance}},
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ValidGrassLSUha")
#' }
#' @importFrom mrcommons toolCell2isoCell

calcValidGrassLSUha<-function(datasource = "MAgPIEown"){
  
  if(datasource=="MAgPIEown") {
  
  mag_years_past <- findset("past")
  biomass <- calcOutput("FAOmassbalance", aggregate = FALSE)[, mag_years_past , "production.dm"][,, "pasture"]
  biomass <- collapseNames(biomass)
  biomass <- toolIso2CellCountries(biomass)
  
  land <- calcOutput("LanduseInitialisation", cellular = TRUE, nclasses = "nine", aggregate = FALSE)[, mag_years_past, ]
  land_total <- dimSums(land, dim = 3)
  grassl_land <- land[, , c("past", "range")]
  grassl_land <- setNames(grassl_land, c("pastr", "range"))
  grassl_shares <- setNames(grassl_land[, , "pastr"] / dimSums(grassl_land, dim = 3), "pastr")
  grassl_shares <- add_columns(grassl_shares, addnm = "range", dim = 3.1)
  grassl_shares[, , "range"] <- 1 - grassl_shares[, , "pastr"]
  grassl_shares[is.nan(grassl_shares) | is.infinite(grassl_shares)] <- 0
  
  mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")
  
  livestock <- setNames(toolCell2isoCell(readSource("GLW3")), "liv_numb")
  livst_split <- livestock * grassl_shares
  livst_split <- collapseNames(livst_split)
  livst_split_ctry <- toolAggregate(livst_split, rel = mapping, to = "iso", from = "celliso")
  livst_share_ctry <- livst_split_ctry[, , "pastr"] / dimSums(livst_split_ctry, dim = 3)
  livst_share_ctry[is.nan(livst_share_ctry) | is.infinite(livst_share_ctry)] <- 0
  livst_share_ctry <- add_columns(livst_share_ctry, addnm = "range", dim = 3.1)
  livst_share_ctry[, , "range"] <- 1 - livst_share_ctry[, , "pastr"]
  
  # I am splitting biomass consumption assuming the share
  # between animals reared on rangelands and pastures correlates linearly
  # with the production of grass in pastures and rangelands in a country. That can be
  # derived by the fact that the feedbaskets assume the same productivity (feed ingreedients shares)
  # within a country.
  
  lsu_split <- biomass * livst_share_ctry / (8.9 * 365 / 1000)
  lsu_split <- toolCountryFill(lsu_split)
  lsu_split[is.nan(lsu_split) | is.na(lsu_split) | is.infinite(lsu_split)] <- 0
  lsu_split <- setNames(lsu_split, paste0("Total lsu|+|Cattle|", reportingnames(getNames(lsu_split, dim = 1)), " (millions)"))
  lsu_split <- add_dimension(lsu_split, dim=3.1, add="scenario", nm="historical")
  lsu_split <- add_dimension(lsu_split, dim=3.2, add="model", nm=datasource)
  }
  
  return(list(x=lsu_split,
              weight=NULL,
              unit="Mt DM/yr",
              description="Grass Production from pastures and rangelands")
  )
}