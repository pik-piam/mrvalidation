calcValidTau <- function(datasource = "FAO2012") {

  if(datasource == "FAO2012") {
    tau    <- readSource("Tau","historical")
    out    <- collapseNames(tau[,,"tau.total"])
    weight <- collapseNames(tau[,,"xref.total"])
    description <- "Historic Trends in Agricultural Land Use Intensity Tau based on FAO yield trends from 2012"
  } else if (datasource == "FAOonline") {
    prod <- collapseDim(readSource("FAO_online","Crop")[,,"production"])
    itemsMap <- toolGetMapping("FAOitems.csv", type = "sectoral", where="mappingfolder")
    prod <- toolAggregate(prod, dim=3.1, rel=itemsMap, partrel=TRUE, from = "ProductionItem", to = "k")
    area <- calcOutput("Croparea", aggregate = FALSE)

    tau <- readSource("Tau", "paper")
    cropMap <- c(tece="tece", rice = "rice_pro", maize = "maiz", trce = "trce",
                 pulses = "puls_pro", tero = "sugr_beet", trro = "cassav_sp",
                 sunflower = "sunflower", soybean = "soybean", groundnut = "groundnut",
                 rapeseed = "rapeseed", sugarcane = "sugr_cane", others = "others")
    tau <- setItems(tau[,,names(cropMap)], unname(cropMap), dim = 3.2)

    elem <- getItems(tau, dim = 3.2)
    years <- intersect(getItems(prod, dim=2), getItems(area, dim=2))

    yield <- prod[,years,elem]/area[,years,elem]
    yield[is.infinite(yield)] <- NaN
    yield <- lowpass(yield, 5)

    tauHist <- collapseDim(yield/setYears(yield[,1995,],NULL)*setYears(tau[,1995,"tau"], NULL))
    weight <- collapseDim(tau[,1995,"xref"][,,elem])
    weight <- magpie_expand(weight, tauHist)
    weight[is.na(tauHist)] <- 0
    tauHist[is.na(tauHist)] <- 0

    rel <- data.frame(from=getItems(tauHist, dim = 3), to="total")
    out <- toolAggregate(tauHist, dim = 3, rel = rel, weight = weight)
    out[out==0 | is.infinite(out)] <- NA
    weight <- toolAggregate(weight, dim = 3, rel = rel)
    weight[is.na(out)] <- 0
    out[is.na(out)] <- 1
    description <- "Historic Trends in Agricultural Land Use Intensity Tau based on FAO yield trends (updated)"
  } else {
    stop("Unknown datasource chosen in calcValidTau!")
  }


  names(dimnames(out))[3] <- "scenario.model.variable"
  getNames(out) <- "historical.dietrich_et_al_2012.Productivity|Landuse Intensity Indicator Tau (Index)"

  return(list(x=out,
              weight=weight,
              min=0,
              max=10,
              unit="-",
              description=description,
              note=c('data uses initialization values for 1995 based on ',
                     'Dietrich J.P., Schmitz C., M\uFCller C., Fader M., Lotze-Campen H., Popp A.,',
                     'Measuring agricultural land-use intensity - A global analysis using a model-assisted approach',
                     'Ecological Modelling, Volume 232, 10 May 2012, Pages 109-118, ISSN 0304-3800, 10.1016/j.ecolmodel.2012.03.002.',
                     'preprint available \u40 http://edoc.gfz-potsdam.de/pik/display.epl?mode=doc&id=5281')))

}
