#' @title fullMADRATTOLPJML
#' @description creates historical outputs to be used by LPJml or other models that require gridded inputs
#' 
#' @return No return; the function writes a number of netcdf files into the output folder of MADRAT
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{fullVALIDATION}}
#' @examples
#' 
#' \dontrun{ 
#' fullMADRATTOLPJML()
#' }
#' 


fullMADRATTOLPJML<-function(){
  calcOutput("ValidGridLand",aggregate=FALSE,file="LandAreaPhysical.nc")
  calcOutput("ValidGridCroparea",aggregate=FALSE,file="CroplandAreaPhysical.nc")
  calcOutput("ValidGridYields",aggregate=FALSE,file="YieldsPhysical.nc")
  calcOutput("ValidGridNitrogenBudgetCropland",aggregate=FALSE,file="NitrogenBudgetCropland.nc")
  calcOutput("ValidGridNitrogenBudgetPasture",aggregate=FALSE,file="NitrogenBudgetPasture.nc")
  #calcOutput("ValidGridNitrogenBudgetNonagland",aggregate=FALSE,file="NitrogenBudgetNonagland.nc")
  calcOutput("ValidGridResidueDemand",aggregate=FALSE,file="AbovegroundCropResidueDemand.nc")
  calcOutput("ValidGridCroplandNitrogenInputs",aggregate=FALSE,file="NitrogenInputsByCropType.nc")
  calcOutput("ValidGridCroplandNitrogenWithdrawals",aggregate=FALSE,file="NitrogenWithdrawalsByCropType.nc")
  calcOutput("ValidGridCroplandNitrogenWithdrawals",irrigation="rainfed",aggregate=FALSE,file="NitrogenWithdrawalsByCropTypeRainfed.nc")
  calcOutput("ValidGridCroplandNitrogenWithdrawals",irrigation="irrigated",aggregate=FALSE,file="NitrogenWithdrawalsByCropTypeIrrigated.nc")
  calcOutput("ValidGridCroplandNitrogenSurplus",aggregate=FALSE,file="NitrogenSurplusByCropType.nc")
}
