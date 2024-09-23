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
#' @author Florian Humpenoeder, Abhijeet Mishra, Kristine Karstens

calcValidIncome <- function(datasource = "WDI-MI_SSPs-MI") {

  if (datasource == "WDI-MI_SSPs-MI") {

    .tmp <- function(x, nm) {
      getSets(x)[3] <- "scenario"
      add_dimension(collapseNames(x), dim = 3.2, add = "variable", nm = nm)
    }

    names <- c("Income (million US$2017 MER/yr)",
               "Income (US$2017 MER/cap/yr)",
               "Income (million US$2017 PPP/yr)",
               "Income (US$2017 PPP/cap/yr)")

    mer   <- .tmp(calcOutput("GDP", unit = "constant 2017 US$MER", naming = "scenario", aggregate = FALSE), names[1])
    merpc <- .tmp(calcOutput("GDPpc", unit = "constant 2017 US$MER", naming = "scenario", aggregate = FALSE), names[2])
    ppp   <- .tmp(calcOutput("GDP", naming = "scenario", aggregate = FALSE), names[3])
    ppppc <- .tmp(calcOutput("GDPpc", naming = "scenario", aggregate = FALSE), names[4])

    years <- getYears(mer)

    out   <- NULL
    out   <- mbind(mer[, years, ], merpc[, years, ], ppp[, years, ], ppppc[, years, ])

    getSets(out)[3] <- "scenario"

    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    # Setting weights correctly for intensive and extensive variables
    popWeights <- collapseNames(calcOutput(type = "GDPpc",
                                           unit = "constant 2017 US$MER",
                                           naming = "scenario",
                                           supplementary = TRUE,
                                           aggregate = FALSE)$weight
                                + 10^-10)
    getSets(popWeights)[3] <- "scenario"
    popWeights <- add_dimension(popWeights, dim = 3.2, add = "model", nm = datasource)

    noWeights <- popWeights
    noWeights[] <- NA

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
              unit = "(million) US Dollar 2017 equivalents in MER/yr, MER/cap/yr, PPP/yr, PPP/cap/yr",
              description = "Income")
  )
}
