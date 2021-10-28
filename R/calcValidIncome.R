#' calcValidIncome
#'
#' Returns historical development of income and future projections of income dynamics
#'
#'
#' @param datasource To switch between historical values of James et al (default) or
#' with future projections for different scenarios (James_OECD_Nakicenovic)
#' @return list of magpie object with data and weight. Since intensive and extensive variables
#' are mixed please keep the mixed_aggregation
#' @importFrom madrat vcat calcOutput
#' @importFrom magclass mbind add_dimension getSets getSets<- collapseNames getYears
#' @importFrom GDPuc convertGDP
#' @author Florian Humpenoeder, Abhijeet Mishra, Kristine Karstens

calcValidIncome <- function(datasource = "James") {
  
  
  if (datasource == "WDI_completed_SSP_completed") {
    
    
    names <- c("Income (million US$05 MER/yr)", "Income (US$05 MER/cap/yr)",
               "Income (million US$05 PPP/yr)", "Income (US$05 PPP/cap/yr)")
    
 
    .tmp <- function(x, nm) {
      getSets(x)[3] <- "scenario"
      return(add_dimension(collapseNames(x), dim = 3.2, add = "variable", nm = nm))
    }
    
    mer   <- .tmp(calcOutput("GDPppp", GDPpppCalib = c("fixHist_IMFgr_return2SSP", "Ariadne"),
                       GDPpppPast = c("WDI_completed", "Eurostat_WDI_completed"), 
                       GDPpppFuture = c("SSP_bezierOut_completed", "SSP2EU_completed_bezierOut"), 
                       aggregate = FALSE),
                  names[1])
    
                 # Convert from 2005 Int$PPP to 2005 US$MER, and use regional averages when conversion factors are missing
                  regmap <- toolGetMapping("regionmappingH12.csv")[,c(2,3)] 
                  names(regmap) <- c("iso3c", "region")                 

                mer <- convertGDP(mer, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2005 US$MER",
                                  with_regions = regmap, replace_NAs = "regional_average")
    getNames(mer, dim=1) <- gsub("gdp_", "",getNames(mer,dim=1))
                
    merpc <- .tmp(calcOutput(type = "GDPpc", unit = "2005 constant US$MER", naming = "scenario", aggregate = FALSE), names[2])
    
    ppp   <- .tmp(calcOutput("GDPppp",  GDPpppCalib = c("fixHist_IMFgr_return2SSP", "Ariadne"),
                             GDPpppPast = c("WDI_completed", "Eurostat_WDI_completed"), 
                             GDPpppFuture = c("SSP_bezierOut_completed", "SSP2EU_completed_bezierOut"),
                             aggregate = FALSE), names[3])
    getNames(ppp, dim=1) <- gsub("gdp_", "",getNames(ppp,dim=1))
    
    ppppc <- .tmp(calcOutput(type = "GDPpc", naming = "scenario", aggregate = FALSE), names[4])
    
    years <- intersect(getYears(merpc), getYears(mer))
    
    out   <- NULL
    out   <- mbind(mer[, years, ], merpc[, years, ], ppp[, years, ], ppppc[, years, ])
    
    getSets(out)[3] <- "scenario"
    
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)
    
    # Setting weights correctly for intensive and extensive variables
    popWeights <- collapseNames(calcOutput(type = "GDPpc", 
                                           unit = "2005 constant US$MER", 
                                           naming = "scenario", 
                                           supplementary = TRUE, 
                                           aggregate = FALSE)$weight
                                + 10^-10)
    getSets(popWeights)[3] <- "scenario"
    popWeights <- add_dimension(popWeights, dim = 3.2, add = "model", nm = datasource)
    
    noWeights <- popWeights
    noWeights[] <- 0
    
    weight <- NULL
    weight <- mbind(weight, add_dimension(noWeights, dim = 3.3, add = "variable", nm = names[1]))
    weight <- mbind(weight, add_dimension(popWeights, dim = 3.3, add = "variable", nm = names[2]))
    weight <- mbind(weight, add_dimension(noWeights, dim = 3.3, add = "variable", nm = names[3]))
    weight <- mbind(weight, add_dimension(popWeights, dim = 3.3, add = "variable", nm = names[4]))
    
    
  } else if (datasource == "James") {

    mer   <- calcOutput("GDPpppPast", GDPpppPast = "IHME_USD05_MER_pc", aggregate = FALSE)
    merpc <- readSource("James", subtype = "IHME_USD05_MER_pc")

    ppp   <- calcOutput("GDPpppPast", GDPpppPast = "IHME_USD05_PPP_pc", aggregate = FALSE)
    ppppc <- readSource("James", subtype = "IHME_USD05_PPP_pc")

    years <- intersect(getYears(merpc), getYears(mer))

    out   <- NULL
    out   <- mbind(mer[, years, ], merpc[, years, ], ppp[, years, ], ppppc[, years, ])

    names <- c("Income (million US$05 MER/yr)", "Income (US$05 MER/cap/yr)",
               "Income (million US$05 PPP/yr)", "Income (US$05 PPP/cap/yr)")

    getNames(out, dim = 1) <- names
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)


    # Setting weights correctly for intensive and extensive variables
    popWeights  <- readSource("WDI", subtype = "SP.POP.TOTL") + 10^-10
    noWeights   <- popWeights
    noWeights[] <- 0

    weight <- NULL
    weight <- mbind(noWeights, popWeights, noWeights, popWeights)

    getNames(weight) <- names
    weight <- add_dimension(weight, dim = 3.1, add = "scenario", nm = "historical")
    weight <- add_dimension(weight, dim = 3.2, add = "model", nm = datasource)

    years <- intersect(getYears(out), getYears(weight))
    out <- out[, years, ]
    weight <- weight[, years, ]

  } else if (datasource == "James_OECD_Nakicenovic") {
  #### deprecated as of rev4.64 ####
    names <- c("Income (million US$05 MER/yr)", "Income (US$05 MER/cap/yr)",
               "Income (million US$05 PPP/yr)", "Income (US$05 PPP/cap/yr)")

    .tmp <- function(x, nm) {
      getSets(x)[3] <- "scenario"
      return(add_dimension(collapseNames(x), dim = 3.2, add = "variable", nm = nm))
    }

    mer   <- .tmp(calcOutput("GDPppp", GDPpppPast = "IHME_USD05_MER_pc_completed", GDPpppFuture = "SSP_completed",
                             GDPpppCalib = "past", aggregate = FALSE), names[1])
    getNames(mer, dim=1) <- gsub("gdp_", "",getNames(mer,dim=1))
    
    merpc <- .tmp(calcOutput(type = "GDPpc", unit = "2005 constant US$MER", naming = "scenario", aggregate = FALSE), names[2])
    ppp   <- .tmp(calcOutput("GDPppp",GDPpppPast = "IHME_USD05_PPP_pc_completed", GDPpppFuture = "SSP_completed", 
                             GDPpppCalib = "past", aggregate = FALSE, naming = "indicator.scenario"), names[3])
    getNames(ppp, dim=1) <- gsub("gdp_", "",getNames(ppp,dim=1))
    
    ppppc <- .tmp(calcOutput(type = "GDPpc", naming = "scenario", aggregate = FALSE), names[4])

    years <- intersect(getYears(merpc), getYears(mer))

    out   <- NULL
    out   <- mbind(mer[, years, ], merpc[, years, ], ppp[, years, ], ppppc[, years, ])

    getSets(out)[3] <- "scenario"

    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    # Setting weights correctly for intensive and extensive variables
    popWeights <- collapseNames(calcOutput(type = "GDPpc", 
                                           unit = "2005 constant US$MER", 
                                           naming = "scenario", 
                                           supplementary = TRUE, 
                                           aggregate = FALSE)$weight
                                + 10^-10)
    getSets(popWeights)[3] <- "scenario"
    popWeights <- add_dimension(popWeights, dim = 3.2, add = "model", nm = datasource)

    noWeights <- popWeights
    noWeights[] <- 0

    weight <- NULL
    weight <- mbind(weight, add_dimension(noWeights, dim = 3.3, add = "variable", nm = names[1]))
    weight <- mbind(weight, add_dimension(popWeights, dim = 3.3, add = "variable", nm = names[2]))
    weight <- mbind(weight, add_dimension(noWeights, dim = 3.3, add = "variable", nm = names[3]))
    weight <- mbind(weight, add_dimension(popWeights, dim = 3.3, add = "variable", nm = names[4]))


  } else {

    vcat(verbosity = 1, paste("Wrong selection of parameters in calcOutput"))
    stop("Given datasource currently not supported!")
  }

  return(list(x = out,
              weight = weight,
              mixed_aggregation = TRUE,
              unit = "(million) US Dollar 2005 equivalents in MER/yr, MER/cap/yr, PPP/yr, PPP/cap/yr",
              description = "Income")
  )
}
