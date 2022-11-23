#' @title calcValidCropareaDiversity
#' @description calculates validation for croparea diversity index. As opposed to
#' \code{magpie4::CropareaDiversityIndex} due to lack of data fallow land is not considered
#'
#' @export
#'
#' @param index can be "shannon", "gini" or "invsimpson" for different types of diversitiy indices
#' @param groupdiv should crop groups be split up into several individual items or not?
#' Choose either FALSE or different (dis)aggregation methods "agg1", "agg2"
#' @return MAgPIE object (unit depends on attributes)
#' @author Patrick v. Jeetze, Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' x <- calcOutput("ValidCropareaDiversity", index = "shannon", groupdiv = "agg1", aggregate = FALSE)
#' }
#'
calcValidCropareaDiversity <- function(index = "shannon", groupdiv = "agg1") {
  area <- calcOutput(type = "ValidCroparea", datasource = "FAO", detail = TRUE, aggregate = FALSE)
  area <- collapseDim(area)

  ### honor to function dineq:::gini.wtd !
  gini <- function(x) {
    weights <- rep(1, length(x))
    weights <- weights / sum(weights)
    order <- order(x)
    x <- x[order]
    weights <- weights[order]
    p <- cumsum(weights)
    nu <- cumsum(weights * x)
    n <- length(nu)
    nu <- nu / nu[n]
    gini <- sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
    return(gini)
  }
  ### honor to function vegan:::diversity !
  shannon <- function(x, base = exp(1)) {
    x <- x / sum(x)
    x <- -x * log(x, base)
    h <- sum(x, na.rm = TRUE)
    return(h)
  }
  invsimpson <- function(x) {
    x <- x / sum(x)
    x <- x * x
    h <- sum(x, na.rm = TRUE)
    h <- 1 / h
    return(h)
  }

  selectIndex <- function(x, index) {
    if (index == "shannon") {
      x <- shannon(x)
    } else if (index == "invsimpson") {
      x <- invsimpson(x)
    } else if (index == "gini") {
      x <- gini(x)
    } else {
      stop("unknown index")
    }
    return(x)
  }

  # as opposed to magpie4::CropareaDiversityIndex due to lack of data fallow land is not considered here

  cropdiv <- function(cellvalue, cropnames) {
    cellvalue <- as.vector(cellvalue)
    names(cellvalue) <- cropnames
    # weights could be improved
    if (groupdiv == "agg1") {
      single <- c(
        "Resources|Land Cover|Cropland|Crops|Cereals|+|Maize (million ha)",
        "Resources|Land Cover|Cropland|Crops|Cereals|+|Tropical cereals (million ha)",
        "Resources|Land Cover|Cropland|Crops|Cereals|+|Rice (million ha)",
        "Resources|Land Cover|Cropland|Crops|Oil crops|+|Soybean (million ha)",
        "Resources|Land Cover|Cropland|Crops|Oil crops|+|Other oil crops incl rapeseed (million ha)",
        "Resources|Land Cover|Cropland|Crops|Oil crops|+|Groundnuts (million ha)",
        "Resources|Land Cover|Cropland|Crops|Oil crops|+|Sunflower (million ha)",
        "Resources|Land Cover|Cropland|Crops|Oil crops|+|Oilpalms (million ha)",
        "Resources|Land Cover|Cropland|Crops|Other crops|+|Potatoes (million ha)",
        "Resources|Land Cover|Cropland|Crops|Sugar crops|+|Sugar cane (million ha)",
        "Resources|Land Cover|Cropland|Crops|Sugar crops|+|Sugar beet (million ha)",
        "Resources|Land Cover|Cropland|Crops|Oil crops|+|Cotton seed (million ha)",
        "Resources|Land Cover|Cropland|Bioenergy crops|+|Short rotation grasses (million ha)"
      )
      mix <- c(
        cellvalue[single],
        rep(cellvalue["Resources|Land Cover|Cropland|+|Forage (million ha)"] / 4, 4),
        rep(cellvalue["Resources|Land Cover|Cropland|Crops|Cereals|+|Temperate cereals (million ha)"] / 2, 2),
        rep(cellvalue["Resources|Land Cover|Cropland|Crops|Other crops|+|Pulses (million ha)"] / 3, 3),
        rep(cellvalue["Resources|Land Cover|Cropland|Bioenergy crops|+|Short rotation trees (million ha)"] / 2, 2),
        rep(cellvalue["Resources|Land Cover|Cropland|Crops|Other crops|+|Tropical roots (million ha)"] / 2, 2),
        rep(cellvalue["Resources|Land Cover|Cropland|Crops|Other crops|+|Fruits Vegetables Nuts (million ha)"] / 10, 10)
      )
    } else if (groupdiv == "agg2") {
      mix <- c(
        cellvalue["Resources|Land Cover|Cropland|Crops|Cereals|+|Temperate cereals (million ha)"], # C3
        cellvalue["Resources|Land Cover|Cropland|Crops|Cereals|+|Maize (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Cereals|+|Tropical cereals (million ha)"], # C4
        cellvalue["Resources|Land Cover|Cropland|Crops|Cereals|+|Rice (million ha)"], # Rice
        cellvalue["Resources|Land Cover|Cropland|Crops|Other crops|+|Pulses (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Oil crops|+|Soybean (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Oil crops|+|Groundnuts (million ha)"], # Legumes
        cellvalue["Resources|Land Cover|Cropland|Bioenergy crops|+|Short rotation grasses (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Sugar crops|+|Sugar cane (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Bioenergy crops|+|Short rotation trees (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Oil crops|+|Oilpalms (million ha)"], # Plantations
        cellvalue["Resources|Land Cover|Cropland|Crops|Other crops|+|Potatoes (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Other crops|+|Tropical roots (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Sugar crops|+|Sugar beet (million ha)"], # roots
        cellvalue["Resources|Land Cover|Cropland|Crops|Oil crops|+|Other oil crops incl rapeseed (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Oil crops|+|Sunflower (million ha)"] +
          cellvalue["Resources|Land Cover|Cropland|Crops|Oil crops|+|Cotton seed (million ha)"], # non-legume oil crops
        rep(cellvalue["Resources|Land Cover|Cropland|+|Forage (million ha)"] / 2, 2), # foddr
        rep(cellvalue["Resources|Land Cover|Cropland|Crops|Other crops|+|Fruits Vegetables Nuts (million ha)"] / 5, 5)
        # fruits vegetables nuts
      )
    } else {
      mix <- cellvalue
    }
    div <- selectIndex(mix, index)

    return(div)
  }
  x <- magpply(area, FUN = cropdiv, DIM = 3, cropnames = getNames(area))
  if (index == "gini") {
    x[is.na(x)] <- 1
    x[x == Inf] <- 1
  } else {
    x[is.na(x)] <- 0
    x[x == Inf] <- 0
  }

  if (index == "shannon") {
    x <- setNames(x, "Biodiversity|Shannon crop area diversity index (unitless)")
  } else if (index == "invsimpson") {
    x <- setNames(x, "Biodiversity|Inverted Simpson crop area diversity index (unitless)")
  } else if (index == "gini") {
    x <- setNames(x, "Biodiversity|Gini crop area diversity index (unitless)")
  }

  land <- calcOutput("FAOLand", aggregate = FALSE)
  landArea <- land[, "y2015", "6601|Land area"] # total land is stable over time, so y2015 is arbitrarily chosen

  if (index == "shannon") {
    desc <- "Shannon crop area diversity index calculated based on historical FAO country-level crop area"
  } else if (index == "invsimpson") {
    desc <- "Inverted Simpson crop area diversity index based on historical FAO country-level crop area"
  } else if (index == "gini") {
    desc <- "Gini crop area diversity index based on historical FAO country-level crop area"
  }

  return(list(
    x = x,
    weight = landArea,
    unit = "unitless",
    description = desc
  ))
}
