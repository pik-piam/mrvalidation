#' @title calcValidEmissionsPeatland
#' @description Validation data for peatland emissions in CO2eq, taken from the article:
#'      Leifeld, J., Menichetti, L. The underappreciated potential of peatlands in global
#'      climate change mitigation strategies. Nat Commun 9, 1071 (2018).
#'      https://doi.org/10.1038/s41467-018-03406-6
#' @author Michael Crawford
#'
#' @return Global-level MAgPIE object with peatlands emissions in CO2eq for the year 2015
#'
#' @examples
#' \dontrun{
#'    calcOutput("ValidEmissionsPeatland")
#' }

calcValidEmissionsPeatland <- function() {

    globalPeatlandEmissions2015 <- 1.91 # CO2eq for the year 2015

    m <- new.magpie(cells_and_regions = c("GLO"),
                    years = c(2015),
                    names = c("Emissions|GWP100AR5|Land|Peatland"),
                    sets = c("region", "year", "variable"),
                    fill = globalPeatlandEmissions2015)

    m <- add_dimension(m, dim = 3.1, add = "scenario", nm = "historical")
    m <- add_dimension(m, dim = 3.2, add = "model", nm = "Leifeld et al")

    return(list(x           = m,
                weight      = NULL,
                unit        = "CO2eq",
                description = "Estimate of global peatland emissions for the year 2015"))

}
