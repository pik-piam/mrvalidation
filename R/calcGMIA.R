#' @title calcGMIA
#'
#' @description Filling gaps in the Historical area equipped for irrigation from GMIA.
#'
#' @return list of magpie object with data and weight
#' @author Stephen Wirth, Anne Biewald
#' @examples
#' \dontrun{
#' a <- calcOutput("GMIA", aggregate = "regglo")
#' }
calcGMIA <- function() {
  x <- readSource("GMIA", "all_data_national", convert = TRUE)

  smallIslandStates <- c("ATG", "BRB", "COK", "NRU", "TUV", "NIU", "MDV", "MLT", "GRD",
                         "VCT", "PLW", "LCA", "SGP", "FSM", "BHR", "DMA", "TON",
                         "KIR", "STP", "SYC")

  antarcticORarctic <- c("ATA", "ATF", "GRL", "SJM")
  desertNoAgg <-  c("ESH", "GIB")

  x[antarcticORarctic, , ] <- 0
  x[desertNoAgg, , ] <- 0

  area <- readSource("FAO_online", "Land")
  sudan <- area[c("SDN", "SSD"), 1, "6600|Country area.Area_(1000_ha)"]
  sudanRatio <- sudan / as.numeric(as.numeric(sudan["SDN", , ]) + as.numeric(sudan["SSD", , ]))
  x["SSD", , ] <- as.numeric(x["SDN", , ]) * as.numeric(sudanRatio["SSD", , ])
  x["SDN", , ] <- as.numeric(x["SDN", , ]) * as.numeric(sudanRatio["SDN", , ])

  # Disaggregate values for China: Mainland, Hong Kong, Taiwan, and Macao
  # Based on AEI shares the information from (http://www.fao.org/nr/water/aquastat/irrigationmap/CHN/index.stm)
  x["HKG", , ] <- as.numeric(x["CHN", , ]) * 0.00003
  x["TWN", , ] <- as.numeric(x["CHN", , ]) * 0.0078
  x["CHN", , ] <- as.numeric(x["CHN", , ]) * 0.992
  x["MAC", , ] <- 0


  valSmallIslands <- as.magpie(apply(x[smallIslandStates, , ], MARGIN = c(2, 3), FUN = "mean"))

  for (i in getItems(x, dim = 1.1)) {
    tmp <- dimSums(x[i, , ], dim = c(2, 3))
    if (is.na(tmp)) {
      areaTMP <- area[i, length(getYears(area)), "6600|Country area.Area_(1000_ha)"]
      if (areaTMP != 0) {
        x[i, , ] <- valSmallIslands
      } else      {
        x[i, , ] <- 0
      }
    }
  }

  return(list(x = x,
              weight = NULL,
              unit = "ha",
              min = 0,
              description = "Area equipped for irrigation in ha"))
}
