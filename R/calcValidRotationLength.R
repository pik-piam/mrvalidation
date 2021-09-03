#' @title calcValidRotationLength
#' @description calculates the validation data for MAgPIE rotation lengths in plantations
#'
#' @param datasource Datasources for validation data
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#'
#' @examples
#' \dontrun{
#' calcOutput("ValidRotationLength")
#' }
#'
calcValidRotationLength <- function(datasource = "FAO 2006") {
  if (datasource == "FAO 2006") {
    valid_rl <- readSource("FAOrotation")

    min <- collapseNames(valid_rl[, , "min"])
    mean <- collapseNames(valid_rl[, , "mean"])
    max <- collapseNames(valid_rl[, , "max"])

    min_temp <- setNames(min[, , 1], "min")
    mean_temp <- setNames(mean[, , 1], "mean")
    max_temp <- setNames(max[, , 1], "max")

    for (reg in getCells(valid_rl)) {
      min_temp[reg, , ] <- min(min[reg, , ], na.rm = TRUE)
      mean_temp[reg, , ] <- mean(mean[reg, , ], na.rm = TRUE)
      max_temp[reg, , ] <- max(max[reg, , ], na.rm = TRUE)
    }

    nm <- "historical"

    out <- setYears(collapseNames(ceiling(mbind(min_temp, mean_temp, max_temp))), "y1990")
    out <- time_interpolate(dataset = out, interpolated_year = c("y1995", "y2000", "y2005"), extrapolation_type = "constant")

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = nm)
    getNames(out, dim = 2) <- paste0(datasource, " ", getNames(out, dim = 2))
    out <- add_dimension(out, dim = 3.3, add = "variable", nm = "Rotation lengths|Harvest")

    out2 <- out
    getNames(out2, dim = 3) <- "Rotation lengths|Establishment"

    out <- mbind(out, out2)

    weight <- setNames(out2[, , 1], NULL)
    weight[weight != 1] <- 1

    return(list(
      x = out,
      weight = weight,
      unit = "years",
      description = "Plantation Rotation length based on Global planted forests thematic study: Results and analysis (2006)"
    ))
  } else {
    stop("No data exist for the given datasource!")
  }
}
