#' calcValidGini
#'
#' Returns historical development of Gini Coefficient
#'
#' @return list of magpie object with data and weight.
#' @author David M Chen

calcValidGini <- function() {

  out <- readSource("WBPoverty", subtype = "Gini", convert = FALSE)
  getSets(out)[3] <- "scenario"
  getItems(out, dim = 3) <- "historical"

  weight <- calcOutput("Population", aggregate = FALSE)[, , "pop_SSP2"]
  weight <- time_interpolate(weight, interpolated_year = getYears(out),
                             integrate_interpolated_years = TRUE)

  weight <- weight[getItems(out, dim = 1), getItems(out, dim = 2), ]
  weight[which(out == 9999)] <- 0 # make missing data 0 weight

  out <- add_dimension(out, dim = 3.2, add = "model", nm = "World Bank WDI")
  getNames(out, dim = 1) <- "Income|Gini Coefficient (0-1)"

  getSets(weight)[3] <- "scenario"
  getItems(weight, dim = 3) <- "historical"
  weight <- add_dimension(weight, dim = 3.2, add = "model", nm = "World Bank WDI")
  getNames(weight, dim = 1) <- "Income|Gini Coefficient (0-1)"

  return(list(x = out,
              weight = weight + 10^-10,
              unit = "(0-1) Gini Coefficient between 0 and 1",
              description = "Gini Coefficient")
  )
}
