#' @title calcValidFoodExpenditureShare
#' @description validation for food expenditure share
#'
#' @param detail if FALSE, only major food commoditiy groups are shown.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("ValidFoodExpenditureShare")
#' }
#'
calcValidFoodExpenditureShare <- function(detail = FALSE) {
  expenditure <- calcOutput("ValidFoodExpenditure", aggregate = FALSE)
  gdp <- collapseNames(calcOutput("GDPpc", scenario = "SSP2", aggregate = FALSE)[, , ])
  gdp <- gdp[, getYears(expenditure), ]
  pop <- collapseNames(calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[, , ])
  pop <- pop[, getYears(expenditure), ]

  expenditureShr <- expenditure / gdp
  expenditureShr[is.nan(expenditureShr)] <- 0
  getNames(expenditureShr) <- sub(pattern = "\\|Expenditure",
                                  replacement = "|Expenditure Share",
                                  x = getNames(expenditureShr))

  return(list(
    x = expenditureShr,
    weight = gdp * pop,
    unit = "US$2017/US$2017",
    description = "Share of expenditure for different food items"
  ))
}
