#' calcValidIncome
#' 
#' Returns historical development of income and future projections of income dynamics
#' 
#' 
#' @param datasource To switch between historical values of James et al (default) or 
#' with future projections for different scenarios (James_OECD_Nakicenovic) 
#' @return list of magpie object with data and weight. Since intensive and extensive variables 
#' are mixed please keep the mixed_aggregation
#' @author Florian Humpenoeder, Abhijeet Mishra, Kristine Karstens

calcValidIncome <- function(datasource = "James") {

  if(datasource == "James"){
    
    MER   <- calcOutput("GDPpppPast", GDPpppPast = "IHME_USD05_MER_pc",aggregate = FALSE)
    MERpc <- readSource("James", subtype="IHME_USD05_MER_pc")
    
    PPP   <- calcOutput("GDPpppPast", GDPpppPast = "IHME_USD05_PPP_pc",aggregate = FALSE)
    PPPpc <- readSource("James", subtype="IHME_USD05_PPP_pc")
    
    years <-intersect(getYears(MERpc),getYears(MER))
    
    out   <- NULL
    out   <- mbind(MER[,years,],MERpc[,years,],PPP[,years,],PPPpc[,years,])
    
    names <- c("Income (million US$05 MER/yr)","Income (US$05 MER/cap/yr)","Income (million US$05 PPP/yr)","Income (US$05 PPP/cap/yr)")
    
    getNames(out,dim=1) <- names
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm = datasource)
    
    
    # Setting weights correctly for intensive and extensive variables
    pop_weights  <- readSource("WDI",subtype = "SP.POP.TOTL") + 10^-10
    no_weights   <- pop_weights
    no_weights[] <- 0
    
    weight <- NULL
    weight <- mbind(no_weights, pop_weights, no_weights, pop_weights)
    
    getNames(weight) <- names
    weight <- add_dimension(weight, dim=3.1, add="scenario", nm="historical")
    weight <- add_dimension(weight, dim=3.2, add="model", nm=datasource)
    
    years <- intersect(getYears(out),getYears(weight))
    out <- out[,years,]
    weight <- weight[,years,]
    
  } else if(datasource == "James_OECD_Nakicenovic"){
    
    names <- c("Income (million US$05 MER/yr)","Income (US$05 MER/cap/yr)","Income (million US$05 PPP/yr)","Income (US$05 PPP/cap/yr)")
    
    MER   <- collapseNames(calcOutput("GDPppp", GDPpppPast = "IHME_USD05_MER_pc", GDPpppFuture="SRES_SSP_completed",GDPpppCalib="past",aggregate = FALSE))
    MER   <- add_dimension(MER, dim=3.2, add="variable", nm=names[1])
    MERpc <- collapseNames(calcOutput(type = "GDPpc", gdp="MER", aggregate = FALSE))
    MERpc <- add_dimension(MERpc, dim=3.2, add="variable", nm=names[2])
    
    PPP   <- collapseNames(calcOutput("GDPppp", GDPpppFuture="SRES_SSP_completed", aggregate = FALSE,naming="indicator.scenario"))
    PPP   <- add_dimension(PPP, dim=3.2, add="variable", nm=names[3])
    PPPpc <- collapseNames(calcOutput(type = "GDPpc", gdp="PPP", aggregate = FALSE))
    PPPpc <- add_dimension(PPPpc, dim=3.2, add="variable", nm=names[4])
    
    years <-intersect(getYears(MERpc),getYears(MER))
    
    out   <- NULL
    out   <- mbind(MER[,years,],MERpc[,years,],PPP[,years,],PPPpc[,years,])
    
    getSets(out)[3]<-"scenario"
    
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)

    # Setting weights correctly for intensive and extensive variables
    pop_weights <- collapseNames(calcOutput(type = "GDPpc", gdp="MER",supplementary = TRUE, aggregate = FALSE)$weight + 10^-10)
    getSets(pop_weights)[3] <- "scenario"
    pop_weights <- add_dimension(pop_weights, dim=3.2, add="model", nm=datasource)
    
    no_weights <- pop_weights
    no_weights[] <- 0
    
    weight <- NULL
    weight <- mbind(weight, add_dimension(no_weights, dim=3.3, add="variable", nm=names[1]))
    weight <- mbind(weight, add_dimension(pop_weights, dim=3.3, add="variable", nm=names[2]))
    weight <- mbind(weight, add_dimension(no_weights, dim=3.3, add="variable", nm=names[3]))
    weight <- mbind(weight, add_dimension(pop_weights, dim=3.3, add="variable", nm=names[4]))
    
    
  } else {
   
    vcat(verbosity = 1,paste("Wrong selection of parameters in calcOutput"))
    stop("Given datasource currently not supported!")
  }

  return(list(x=out,
              weight=weight,
              mixed_aggregation = TRUE,
              unit="(million) US Dollar 2005 equivalents in MER/yr, MER/cap/yr, PPP/yr, PPP/cap/yr",
              description="Income")
  )
}