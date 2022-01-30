calcValidTau <- function() {
  tau    <- readSource("Tau","historical")
  out      <- collapseNames(tau[,,"tau.total"])
  weight <- collapseNames(tau[,,"xref.total"])
  
  names(dimnames(out))[3] <- "scenario.model.variable"
  getNames(out) <- "historical.dietrich_et_al_2012.Productivity|Landuse Intensity Indicator Tau cropland (Index)"
  
  return(list(x=out,
              weight=weight,
              min=0,
              max=10,
              unit="-",
              description="Historic Trends in Agricultural Land Use Intensity Tau based on FAO yield trends",
              note=c('data based on Dietrich J.P., Schmitz C., M\uFCller C., Fader M., Lotze-Campen H., Popp A.,',
                     'Measuring agricultural land-use intensity - A global analysis using a model-assisted approach',
                     'Ecological Modelling, Volume 232, 10 May 2012, Pages 109-118, ISSN 0304-3800, 10.1016/j.ecolmodel.2012.03.002.',
                     'preprint available \u40 http://edoc.gfz-potsdam.de/pik/display.epl?mode=doc&id=5281')))
  
}