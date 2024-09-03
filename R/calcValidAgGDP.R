#' @title calcValidAgGDP
#' @description Validation for agricultural value added gdp (Million 05USD)
#' @param datasource datasource for validation. Options FAO and FAO-consum
#' @return List of magpie object with results on country level, no weight, unit and description.
#' @author Edna J. Molina Bacca
#' @importFrom magclass collapseNames
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' calcOutput("ValidAgGDP")
#' }
#'
calcValidAgGDP <- function(datasource = "FAO") {

   if (datasource == "FAO") {

    prodCrops <- collapseNames(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))
    prodLivst <- collapseNames(calcOutput("Production", products = "kli", aggregate = FALSE, attributes = "dm"))

    pricesCrops <- setYears(calcOutput("PricesProducer", products = "kcr",
                                      calculation = "VoP", aggregate = FALSE)[, 2005, ], NULL)
    pricesLivst <- setYears(calcOutput("PricesProducer", products = "kli",
                                      calculation = "FAO", aggregate = FALSE)[, 2005, ], NULL)

    namesCrops <- intersect(getNames(prodCrops), getNames(pricesCrops))
    namesLivst <- intersect(getNames(prodLivst), getNames(pricesLivst))
    years <- intersect(getYears(prodLivst), getYears(prodCrops))

    vopAll <- dimSums(prodCrops[, years, namesCrops] * pricesCrops[, , namesCrops], dim = 3) +
      dimSums(prodLivst[, years, namesLivst] * pricesLivst[, , namesLivst], dim = 3)

    ## Secondary seed and feed

    # Seed
    seedCrops <- collapseNames(calcOutput("Seed", products = "kcr", attributes = "dm", aggregate = FALSE))
    seedLivst <- collapseNames(calcOutput("Seed", products = "kli", attributes = "dm", aggregate = FALSE))

    # feed
    feed <- collapseNames((calcOutput("FAOmassbalance", aggregate = FALSE)[, , "feed"])[, , "dm"])
    kcr <- findset("kcr")
    kli <- findset("kli")

    feedCrops <- feed[, , kcr]
    feedLivst <- feed[, , kli]

    # Price consumers (World Prices)
    pricesCropsCon <- setYears(calcOutput("IniFoodPrice", products = "kcr", aggregate = FALSE), NULL)
    pricesLivstCon <- setYears(calcOutput("IniFoodPrice", products = "kli", aggregate = FALSE), NULL)

    years <- intersect(getYears(seedCrops),
                     intersect(getYears(feedCrops), getYears(vopAll)))

    valueDemand <- dimSums((seedCrops[, years, ] + feedCrops[, years, ]) * pricesCropsCon, dim = 3) +
      dimSums((seedLivst[, years, ] + feedLivst[, years, ]) * pricesLivstCon, dim = 3)

    out <- vopAll[, years, ] - valueDemand[, years, ]
    out[out < 0] <- 0


   } else if (datasource == "FAO_consum") {

     # Food and material demand
     kall <- findset("kall")
     foodMat <- collapseNames(dimSums((calcOutput("FAOmassbalance",
                   aggregate = FALSE)[, , kall][, , c("food", "other_util")])[, , "dm"], dim = 3.2))

     # Price consumers (World Prices)
     pricesKallCon <- setYears(calcOutput("IniFoodPrice", products = "kall", aggregate = FALSE), NULL)

     # Consumption value and production value should be the same at global level
     out <- dimSums(dimSums(foodMat * pricesKallCon, dim = 3), dim = 1)

   } else {
    stop("unknown datasource")
     }

  getNames(out) <- "Value|Agriculture GDP (million US$2017/yr)"
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

  return(list(x = out,
              weight = NULL,
              unit = "million US$2017/yr",
              description = "Agriculture Value added GDP"))
}
