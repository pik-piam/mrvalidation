#' @title fullENGAGE
#' @description retrieves validation data from moinput for the ENGAGE project
#'
#' @return validation data in IAMC reporting format
#' @author Florian Humpenoeder
#'
#' @export
#' @import magclass
#' @importFrom madrat getConfig setConfig
#'

fullENGAGE <- function() {

  setConfig(cachefolder="/p/projects/rd3mod/inputdata/cache/rev4.18")

  convertENGAGE <- function(x) {
    scenario<-getNames(x,dim=2)
    x <- collapseNames(x,collapsedim = 1)
    x <- collapseNames(x,collapsedim = 1)
    y <- new.magpie(getCells(x),seq(1970,2015,by=1),getNames(x),fill = NA,sets = names(dimnames(x)))
    joint_years <- intersect(getYears(y), getYears(x))
    y[,joint_years,] <- x[,joint_years,]
    y <- add_dimension(y,dim=3.1,add="scenario",scenario)
    y <- add_dimension(y,dim=3.2,add="model","Reference")
    return(y)
  }

  for (reg_map in c("regionmappingENGAGE.csv","regionmappingR10.csv")) {
    print(reg_map)
    setConfig(regionmapping=reg_map)
    out <- NULL

    setConfig(forcecache=FALSE) #bug in variable name in rev <= 4.18
    x <- calcOutput(type="ValidEmissions", datasource="EDGAR_LU",aggregate="REG+GLO")
    x <- convertENGAGE(x)
    out <- mbind(out,x)

    setConfig(forcecache=TRUE)
    x <- calcOutput(type="ValidEmissions", datasource="PRIMAPhist",aggregate="REG+GLO")
    x <- convertENGAGE(x)
    out <- mbind(out,x)

    x <- calcOutput(type="ValidYield", datasource="FAO",aggregate="REG+GLO")
    x <- convertENGAGE(x)
    out <- mbind(out,x)

    x <- calcOutput(type="ValidLand", datasource="FAO_crop_past",aggregate="REG+GLO")
    x <- convertENGAGE(x)
    out <- mbind(out,x)

    x <- calcOutput(type="ValidLand", datasource="FAO_forest",aggregate="REG+GLO")
    x <- convertENGAGE(x)
    out <- mbind(out,x)

    x <- calcOutput(type="ValidLand", datasource="LUH2v2",aggregate="REG+GLO")
    x <- convertENGAGE(x)
    out <- mbind(out,x)

    # x <- calcOutput(type="ValidKcal", datasource="FAO",aggregate="REG+GLO")
    # x <- convertENGAGE(x)
    # out <- mbind(out,x)

    #map  <- toolMappingFile("reportingVariables", "mapping_remmag_engage_validation.csv", readcsv = TRUE)
    write.report(out,"tmp.csv")
    #write.reportProject("tmp.csv",mapping = "/p/projects/rd3mod/inputdata/mappings/reportingVariables/mapping_remmag_engage_validation.csv",file=paste0("ENGAGE_valid_data_",reg_map),format = "default")
  }
}
