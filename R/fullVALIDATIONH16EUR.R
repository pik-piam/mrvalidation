#' fullValidationH16Eur
#'
#' Function that produces the complete validation data for H16 runs, with EUR as an
#' additional, aggregated region.
#'
#' @param rev data revision which should be used as input. Will be converted to
#' \code{\link[base]{numeric_version}} when called via \code{\link[madrat]{retrieveData}}.
#' @param aggregate an aggregation level, such as "region+global", to be used for all outputs
#' that are being aggregated.
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky
#' @seealso
#' \code{\link[madrat]{readSource}},\code{\link[madrat]{getCalculations}},\code{\link[madrat]{calcOutput}}
#' @examples
#' \dontrun{
#' retrieveData("Validation")
#' }
#'
fullVALIDATIONH16EUR <- function(rev = 0.1, aggregate = "region+global+eu") {

  fullVALIDATION(rev, aggregate)

  # Remove NotEUR from validation.mif
  # This is necessary as we want an additional aggregated region EUR
  # similar to GLO. However, madrat aggregation currently only allows
  # for complete mappings in aggregations, thus requiring us to map
  # to EUR/NotEUR. NotEUR in turn will show up in plots later on, which
  # is cumbersome. This function takes care of producting the validation.mif
  # with EUR but without NotEUR.
  if (!file.exists("validation.mif")) {
    stop("File 'validation.mif' does not exist or cannot be read.")
  }
  validations <- magclass::read.report("validation.mif")
  validations <- lapply(validations, function(validationGroup) {
    return(lapply(validationGroup, function(validation) {
      return(validation["NotEUR", , , invert = TRUE])
    }))
  })
  magclass::write.report(validations, "validation.mif")

}
