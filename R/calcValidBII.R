#' @title calcValidBII
#' @description Validation for the Biodiversity Intactness Index (BII), side-by-side
#' from two NHM sources:
#'   * Phillips et al. 2021 (BII-BTE, DOI 10.5519/he1eqmg1), country-level
#'     1970-2014.
#'   * De Palma et al. 2024 NHM v2.1.1 (DOI 10.5519/k33reyb6), cellular
#'     5-yearly 2000-2020, aggregated to ISO here.
#' v2 differs from v1 in three steps: stricter primary-vegetation baseline
#' (minimally/lightly-used primary only, vs v1's all-primary including
#' intensely-used sites), logit (not log) transform, and balanced Bray-Curtis
#' (abundance-weighted) compositional similarity in place of v1's asymmetric
#' Jaccard (species-identity only). v2 absolute BII is markedly lower than v1,
#' with a non-uniform gap that depends on land-use intensity, so the two
#' series are NOT splice-compatible and are kept as separate model labels.
#'
#' MAgPIE's own BII output (modules/44_biodiversity, coefficients from
#' Leclère et al. 2020 "Bending the Curve" via PREDICTS) is on the v1 scale,
#' so it aligns with Phillips et al. 2021 in absolute terms but sits markedly
#' above De Palma et al. 2024 v2 (gap depends on land-use intensity). For
#' absolute-level validation use Phillips; for harmonized (change-from-baseline)
#' plots either series works, but mixing MAgPIE projections with v2 historical
#' on a non-harmonized axis is misleading.
#' Note: v2.1.1's land-use driver is also LUH2 (CSIRO-downscaled via Hoskins
#' et al. 2016), so MAgPIE and v2.1.1 share the LU framework -- only the
#' PREDICTS coefficient version differs.
#' @author Michael Crawford, Florian Humpenoeder
#' @seealso \code{\link{readBII}}, \code{\link{readBIIv2}}, \code{\link{downloadBIIv2}}
#'
#' @return Country-level MAgPIE object: "Phillips et al" + "De Palma et al 2024"
#'   under variable "Biodiversity|BII".
#'
#' @examples
#'
#' \dontrun{
#'    calcOutput("ValidBII")
#' }
#'
#' @importFrom madrat toolAggregate toolCountryFill toolNAreplace

calcValidBII <- function() {

  # Phillips et al. 2021: already country-level.
  biiV1 <- readSource("BII", subtype = "historical", subset = "bii")
  biiV1 <- add_dimension(biiV1, dim = 3.2, add = "model", nm = "Phillips et al")
  getNames(biiV1, dim = "variable") <- "Biodiversity|BII"

  # De Palma et al. 2024 NHM v2.1.1: cellular -> ISO. toolNAreplace masks both
  # value and weight to 0 where either is NA, so toolAggregate's weighted mean
  # ignores water/missing cells instead of propagating NA (the toolAggregate
  # weighted path multiplies x*weight, so raw NAs would zero out whole
  # countries). Same pattern as mrcommons::calcLanduseIntensity, calcValidTau.
  # nm must not contain "." (magclass parses dots as sub-dim separators), so
  # the bare label "De Palma et al 2024" -- v2.1.1 details live in BIIv2 source.
  biiV2cell <- readSource("BIIv2")
  cellArea  <- calcOutput("LandArea", cells = "lpjcell", aggregate = FALSE)
  naFix     <- toolNAreplace(x = biiV2cell, weight = cellArea)
  biiV2     <- toolAggregate(naFix$x, weight = naFix$weight, to = "iso", dim = 1,
                             zeroWeight = "setNA")
  biiV2     <- toolCountryFill(biiV2, fill = NA, verbosity = 2)
  biiV2     <- add_dimension(biiV2, dim = 3.1, add = "scenario", nm = "historical")
  biiV2     <- add_dimension(biiV2, dim = 3.2, add = "model",    nm = "De Palma et al 2024")
  getNames(biiV2, dim = "variable") <- "Biodiversity|BII"

  # mbind unions one differing dim only; v1 and v2 differ on both years and
  # model, so pad years to the union first.
  yrsAll      <- sort(union(getYears(biiV1), getYears(biiV2)))
  missingInV1 <- setdiff(yrsAll, getYears(biiV1))
  missingInV2 <- setdiff(yrsAll, getYears(biiV2))
  if (length(missingInV1)) biiV1 <- add_columns(biiV1, addnm = missingInV1, dim = 2)
  if (length(missingInV2)) biiV2 <- add_columns(biiV2, addnm = missingInV2, dim = 2)
  biiV1 <- biiV1[, yrsAll, ]
  biiV2 <- biiV2[, yrsAll, ]

  bii <- mbind(biiV1, biiV2)

  land     <- calcOutput("FAOLand", aggregate = FALSE)
  landArea <- land[, "y2015", "6601|Land area"]   # land area stable across time

  return(list(x           = bii,
              weight      = landArea,
              unit        = "unitless",
              description = paste("Historical BII from two NHM sources:",
                                  "Phillips et al. 2021 (DOI 10.5519/he1eqmg1, 1970-2014)",
                                  "and De Palma et al. 2024 v2.1.1 (DOI 10.5519/k33reyb6, 2000-2020).",
                                  "v1 and v2 use different methodologies and are NOT on the same",
                                  "absolute scale -- compare within a series, not across.")))
}
