#'@title calcGMIA
#' 
#' @description Filling gaps in the Historical area equipped for irrigation from GMIA.
#' 
#' @return list of magpie object with data and weight
#' @author Stephen Wirth, Anne Biewald
#' @examples
#' 
#' \dontrun{ 
#' a <- calcOutput("GMIA",aggregate="regglo")
#' }
calcGMIA <- function(){
  x <- readSource("GMIA", "all_data_national", convert = TRUE)
  
  small_island_states <- c("ATG","BRB", "COK","NRU", "TUV","NIU", "MDV", "MLT", "GRD", 
                           "VCT","PLW", "LCA", "SGP", "FSM", "BHR", "DMA", "TON",
                           "KIR", "STP", "SYC")
  
  antarctica_or_arctic <- c("ATA", "ATF", "GRL", "SJM")
  desserts_no_agg <-  c("ESH", "GIB")
  
  x[antarctica_or_arctic,,] <- 0
  x[desserts_no_agg,,] <- 0
  
  area <- readSource("FAO_online", "Land")
  sudan <- area[c("SDN","SSD"),1,"6600|Country area.Area_(1000_ha)"]
  proportion_sudan <- sudan/as.numeric(as.numeric(sudan["SDN",,]) + as.numeric(sudan["SSD",,]))
  x["SSD",,] <- as.numeric(x["SDN",,])*as.numeric(proportion_sudan["SSD",,])
  x["SDN",,] <- as.numeric(x["SDN",,])*as.numeric(proportion_sudan["SDN",,])


  #Dissaggregate values for China: Mainland, Hong Kong, Tiwan, and Macao
  #Based on AEI shares the information from (http://www.fao.org/nr/water/aquastat/irrigationmap/CHN/index.stm)
  x["HKG",,] <- as.numeric(x["CHN",,]) * 0.00003
  x["TWN",,] <- as.numeric(x["CHN",,]) * 0.0078
  x["CHN",,] <- as.numeric(x["CHN",,]) * 0.992
  x["MAC",,] <- 0
  
  
  value_small_islands <- as.magpie(apply(x[small_island_states,,],MARGIN = c(2,3),FUN = "mean"))
  
  for(i in getRegions(x))
  {
    tmp <- dimSums(x[i,,], dim=c(2,3))
    if(is.na(tmp))
    {
      area_tmp <- area[i,length(getYears(area)),"6600|Country area.Area_(1000_ha)"]
      if(area_tmp!=0)
        {
    x[i,,] <- value_small_islands 
      }
      else
      {
        x[i,,] <- 0
      }
    }
  }
  
  return(list(x=x,
              weight=NULL,
              unit="ha",
              min=0,
              description="Area equipped for Irrigation in ha")
         )
  
  }
