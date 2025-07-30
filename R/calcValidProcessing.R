#' @title calcValidProcessing
#' @description calculates the validation data for the share of inputs that go into processing processes,
#' and the share of inputs that secondary products are composed of
#' @param datasource Datasource of validation data.
#' @param detail if FALSE, only larger product categories are reported
#' @param nutrient The nutrient in which the results shall be reported.
#' @param indicator if "primary_to_process", returns the amount of primary product into processing.
#'  if "secondary_from_primary", gives primary into secondary.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David Chen, Benjamin Leon Bodirsky
#' @seealso
#' \code{\link[mrcommons]{calcFoodSupplyPast}},
#' \code{\link{calcValidLivestockShare}}
#' @examples
#' \dontrun{
#' calcOutput("ValidProcessing", indicator = "secondary_from_primary")
#' }
#'
#' @importFrom magpiesets reporthelper summationhelper reportingnames
#' @importFrom magclass add_columns mbind getNames

calcValidProcessing <- function(datasource = "FAO", detail = TRUE, nutrient = "dm", indicator = "primary_to_process") {

  if (indicator == "primary_to_process") {


     if (datasource == "FAO") {
       mb <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , nutrient])    
    } else if (datasource == "FAOpre2010") {
      mb <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE, version = "pre2010")[, , nutrient])   
    } else if (datasource == "FAOpost2010") {
      mb <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE, version = "post2010")[, , nutrient])   
    } else {
      stop("No data exist for the given datasource!")
    }

      processing <- setdiff(c(findset("processing20")), c("milling", "ginning", "breeding"))
      mb2 <- mb[, , c(processing)]
      getNames(mb2, dim = 2) <- reportingnames(getNames(mb2, dim = 2))

      mb3 <- dimOrder(mb2, c(2, 1))
      out <- reporthelper(x = mb3, dim = 3.2, level_zero_name = "_", detail = detail)
      getNames(out) <- gsub("[^[:alnum:][:blank:]\\|]", "", getNames(out))
      getNames(out) <- sub("\\|$", "", getNames(out))
      getNames(out) <- paste0("Demand|Processing|", getNames(out))

      out <- summationhelper(out, sep = "+")

      getNames(out) <- sub(getNames(out), pattern = "Processing|+", replacement = "Processing|++", fixed = TRUE)

      out <- out[, , which(dimSums(out, dim = c(1, 2)) > 0)]

      out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
      out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    names(dimnames(out))[3] <- "scenario.model.variable"

    if (nutrient == "dm") {
      unit <- "Mt DM/yr"
    } else if (nutrient == "nr") {
      unit <- "Mt Nr/yr"
    } else if (nutrient == "p") {
      unit <- "Mt P/yr"
    } else if (nutrient == "k") {
      unit <- "Mt K/yr"
    } else if (nutrient == "ge") {
      unit <- "PJ/yr"
    } else if (nutrient == "wm") {
      unit <- "Mt WM/yr"
    }

    getNames(out) <- paste0(getNames(out), " (", unit, ")")
  } else if (indicator == "secondary_from_primary") {
    
     if (datasource == "FAO") {
       mb <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , nutrient])    
    } else if (datasource == "FAOpre2010") {
      mb <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE, version = "pre2010")[, , nutrient])   
    } else if (datasource == "FAOpost2010") {
      mb <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE, version = "post2010")[, , nutrient])   
    } else {
      stop("No data exist for the given datasource!")
    }      

      mb2 <- mb[, , c("alcohol1", "alcohol2", "alcohol3", "alcohol4", "brans1", "branoil1", "oil1",
                      "oil2", "oilcakes1", "ethanol1", "molasses1", "sugar1", "distillers_grain1")]
      mb2[, , "alcohol1"] <- dimSums(mb2[, , c("alcohol1", "alcohol2", "alcohol3", "alcohol4")], dim = 3.2)
      mb2[, , "oil1"] <- dimSums(mb2[, , c("oil1", "oil2", "branoil1")], dim = 3.2)

      mb3 <- mb2[, , c("alcohol1", "brans1", "oil1", "oilcakes1", "ethanol1", "molasses1", "sugar1",
                       "distillers_grain1")]

      out <- dimOrder(mb3, c(2, 1))

      getNames(out, dim = 2) <- reportingnames(getNames(out, dim = 2))
      getNames(out, dim = 1) <- substr(getNames(out, dim = 1), start = 1, stop = nchar(getNames(out, dim = 1)) - 1)
      getNames(out, dim = 1)[which(getNames(out, dim = 1) == "oil")] <- "oils"
      getNames(out, dim = 1) <- reportingnames(getNames(out, dim = 1))

      getNames(out, dim = 1) <- paste0("Processing|Raw material|Processed into ", getNames(out, dim = 1))
      out <- add_columns(out, addnm = "Processing|Raw material|Processed into Secondary products", dim = 3.1, fill = 0)
      out[, , "Processing|Raw material|Processed into Secondary products"] <- dimSums(out, dim = 3.1)
      out <- add_columns(out, addnm = "dummy", dim = 3.2, fill = 0)
      out[, , "dummy"] <- dimSums(out, dim = 3.2)

      getNames(out) <- sub(getNames(out), pattern = "\\.", replacement = "|")
      out <- summationhelper(out)
      getNames(out) <- sub(getNames(out), pattern = "\\|\\+\\|dummy", replacement = "")
      getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep = " ")

      out <- out[, , where(out != 0)$true$data]

      out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
      out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)



    names(dimnames(out))[3] <- "scenario.model.variable"

    if (nutrient == "dm") {
      unit <- "Mt DM/yr"
    } else if (nutrient == "nr") {
      unit <- "Mt Nr/yr"
    } else if (nutrient == "p") {
      unit <- "Mt P/yr"
    } else if (nutrient == "k") {
      unit <- "Mt K/yr"
    } else if (nutrient == "ge") {
      unit <- "PJ/yr"
    } else if (nutrient == "wm") {
      unit <- "Mt WM/yr"
    }

    getNames(out) <- paste0(getNames(out), " (", unit, ")")
  }
  return(list(x = out,
              weight = NULL,
              unit = unit,
              description = "Agricultural Processing Demand")
  )
}
