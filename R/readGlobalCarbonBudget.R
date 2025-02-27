#' @title readGlobalCarbonBudget
#' @description read the Global Carbon Budget, selecting the models GCB, BLUE, H&C2023, OSCAR and their sub-components
#' Net, Deforestation, Forest regrowth, Other transitions, Wood harvest and other forest management
#'
#' @author Michael Crawford
#'
#' @return a magpie object in Mt CO2 per year
#'
#' @examples
#' \dontrun{
#' readSource("GlobalCarbonBudget")
#' }
#' @importFrom dplyr %>%
#' @importFrom rlang .data

readGlobalCarbonBudget <- function() {

  # -----------------------------------------------------------------------------------------------------------------
  # Emissions from Land-use Change
  eluc <- suppressMessages(readxl::read_excel("GCB.xlsx", sheet = "Land-Use Change Emissions", skip = 36))

  desiredColumns <- c(
    "Emissions|CO2|Land|+|Land-use Change",
    "Emissions|CO2|Land|Land-use Change|+|Deforestation", # includes shifting cultivation
    "Emissions|CO2|Land|Land-use Change|+|Regrowth",
    "Emissions|CO2|Land|Land-use Change|+|Other land conversion", # incongruent definitions
    "Emissions|CO2|Land|Land-use Change|+|Timber"
  )

  yearData <- eluc %>%
    dplyr::select(.data$Year) %>%
    dplyr::slice(-1)

  elucOut <- NULL
  modelNames <- c("GCB", "BLUE", "H&C2023", "OSCAR")
  for (model in modelNames) {
    startCol <- which(names(eluc) == model)
    selectedCols <- startCol:(startCol + length(desiredColumns) - 1)

    modelData <- eluc %>%
      dplyr::select(dplyr::all_of(selectedCols)) %>%
      dplyr::slice(-1)

    names(modelData) <- desiredColumns

    modelData <- dplyr::bind_cols(yearData, modelData) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

    modelOut <- magclass::as.magpie(modelData)
    modelOut <- magclass::add_dimension(modelOut, dim = 3.1, add = "model", nm = model)

    elucOut <- magclass::mbind(elucOut, modelOut)
  }

  # -----------------------------------------------------------------------------------------------------------------
  # Indirect emissions from climate change
  sland <- suppressMessages(readxl::read_excel("GCB.xlsx", sheet = "Terrestrial Sink", skip = 27)) %>%
    select(.data$Year, .data$GCB) %>%
    rename(`Emissions|CO2|Land|+|Indirect` = .data$GCB) %>%
    mutate(`Emissions|CO2|Land|+|Indirect` = .data$`Emissions|CO2|Land|+|Indirect` * -1) # as negative emissions

  slandOut <- magclass::as.magpie(sland)
  slandOut <- magclass::add_dimension(slandOut, dim = 3.1, add = "model", nm = "GCB")

  # -----------------------------------------------------------------------------------------------------------------
  # Net land flux
  gcbEluc <- elucOut[, , "GCB"][, , "Emissions|CO2|Land|+|Land-use Change"]
  gcbSland <- slandOut[, , "GCB"][, , "Emissions|CO2|Land|+|Indirect"]

  gcbNetLandFlux <- gcbEluc
  gcbNetLandFlux[, , ] <- 0
  gcbNetLandFlux[, , ] <- gcbEluc + gcbSland
  magclass::getNames(gcbNetLandFlux, dim = 2) <- "Emissions|CO2|Land"

  # -----------------------------------------------------------------------------------------------------------------
  # Combine output
  allOut <- magclass::mbind(gcbNetLandFlux, slandOut, elucOut)

  # select MAgPIE years
  years <- magclass::getYears(allOut, as.integer = TRUE)
  years <- years[years >= 1995]
  allOut <- allOut[, years, ]

  # convert from Gt C per year to Mt CO2 per year
  allOut <- allOut * (44 / 12) * 1e3

  return(allOut)
}
