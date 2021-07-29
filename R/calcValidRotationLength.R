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
      min_temp[reg, , ] <- mean(min[reg, , ], na.rm = TRUE)
      mean_temp[reg, , ] <- mean(mean[reg, , ], na.rm = TRUE)
      max_temp[reg, , ] <- mean(max[reg, , ], na.rm = TRUE)
    }

    nm <- "historical"

    out <- setYears(collapseNames(ceiling(max_temp)), "y1990")
    out <- time_interpolate(dataset = out, interpolated_year = c("y1995", "y2000", "y2005"), extrapolation_type = "constant")
    out2 <- out

    getNames(out) <- paste0("Rotation lengths|Harvest", getNames(out))
    getNames(out2) <- paste0("Rotation lengths|Establishment", getNames(out2))

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = nm)
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    out2 <- add_dimension(out2, dim = 3.1, add = "scenario", nm = nm)
    out2 <- add_dimension(out2, dim = 3.2, add = "model", nm = datasource)

    out <- mbind(out, out2)

    # out <- round(toolCountryFill(x = out, fill = median(out),no_remove_warning = TRUE),0)

    weight <- setNames(out2, NULL)
    weight[weight != 1] <- 1

    return(list(
      x = out,
      weight = weight,
      unit = "years",
      description = "Rotation length"
    ))
  } else {
    stop("No data exist for the given datasource!")
  }
}
