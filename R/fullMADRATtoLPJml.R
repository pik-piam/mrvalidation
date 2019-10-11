#' @title fullMADRATtoLPJml
#' @description creates historical outputs to be used by LPJml or other models that require gridded inputs
#' 
#' @return No return; the function writes a number of netcdf files into the output folder of MADRAT
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullVALIDATION}}
#' @examples
#' 
#' \dontrun{ 
#' fullMADRATtoLPJml()
#' }
#' 


fullMADRATtoLPJml<-function(){
  calcOutput("ValidCellularLand",aggregate=FALSE,file="LandAreaPhysical.nc")
  calcOutput("ValidCellularCroparea",aggregate=FALSE,file="CroplandAreaPhysical.nc")
  calcOutput("ValidCellularYields",aggregate=FALSE,file="YieldsPhysical.nc")
  calcOutput("ValidCellularNitrogenBudgetCropland",aggregate=FALSE,file="NitrogenBudgetCropland.nc")
  calcOutput("ValidCellularNitrogenBudgetPasture",aggregate=FALSE,file="NitrogenBudgetPasture.nc")
  calcOutput("ValidCellularNitrogenBudgetNonagland",aggregate=FALSE,file="NitrogenBudgetNonagland.nc")
  calcOutput("ValidCellularResidueDemand",aggregate=FALSE,file="AbovegroundCropResidueDemand.nc")
  calcOutput("ValidCellularCroplandNitrogenInputs",aggregate=FALSE,file="NitrogenInputsByCropType.nc")
  calcOutput("ValidCellularCroplandNitrogenWithdrawals",aggregate=FALSE,file="NitrogenWithdrawalsByCropType.nc")
  calcOutput("ValidCellularCroplandNitrogenWithdrawals",irrigation="rainfed",aggregate=FALSE,file="NitrogenWithdrawalsByCropTypeRainfed.nc")
  calcOutput("ValidCellularCroplandNitrogenWithdrawals",irrigation="irrigated",aggregate=FALSE,file="NitrogenWithdrawalsByCropTypeIrrigated.nc")
  calcOutput("ValidCellularCroplandNitrogenSurplus",aggregate=FALSE,file="NitrogenSurplusByCropType.nc")
}