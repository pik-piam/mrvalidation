#' @title calcValidPriceIndex
#'
#' @description provides global producer price index from FAO 
#' 
#' @param datasource Options of the source of data:  \code{FAO}. 
#' @param value \code{real} and \code{nominal}
#' @param baseyear Baseyear for normalizing of price index.
#' @param round Rounding of price index to intiger numbers. Default \code{TRUE}.
#'
#' @return List with a magpie object with global nominal and real price index.
#' @author Mishko Stevanovic
#' @seealso
#' \code{\link{readProdPrIndex}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput("ValidPriceIndex", value="real", aggregate=FALSE)
#' }
#' 
#' @importFrom magpiesets findset reporthelper
#' @importFrom magclass wrap setNames
#' @importFrom madrat toolAggregate toolGetMapping

calcValidPriceIndex <- function(datasource="FAO", value="real", baseyear="y2005", round=TRUE){
  if(datasource=="FAO"){
    # FAO prices (already in dm values)
    p_t <- calcOutput("PriceAgriculture", datasource=datasource, aggregate=FALSE)
    ## set prices p_t back to magpie names
    p_t <- collapseNames(p_t)
    
    # FAO production
    q_t <- calcOutput("FAOharmonized", aggregate=FALSE)
    aggregation <- toolGetMapping("FAOitems.rda","sectoral",where="moinput")
    q_t <- toolAggregate(q_t[,,"production"], rel=aggregation, from="FoodBalanceItem", 
                    to="k", dim=3.1, partrel = TRUE, verbosity=2)
    q_t <- collapseNames(q_t)
    ## convert FAO production to DM tonnes
    dm <- 1/readSource("ProductAttributes","Products")[,,"wm"]
    dm <- collapseNames(dm)
    ### common years and names dm factors, quantities and prices
    comms <- intersect(intersect(getNames(dm),getNames(q_t)), getNames(p_t))
    ys <- intersect(getYears(p_t),getYears(q_t))[-1]
    q_t <- q_t[,ys,comms]*dm[,,comms]
    p_t <- p_t[,ys,comms]
    
    # create baseyear values 
    q_0 <- q_t
    p_0 <- p_t
    for(y in intersect(getYears(p_t),getYears(q_t))){
      q_0[,y,] <- setYears(q_t[,baseyear,],y)
      p_0[,y,] <- setYears(p_t[,baseyear,],y)
    }
    
    # calculate Laspeyers price index
    out <- dimSums(p_t * q_0, dim=3, na.rm=TRUE) / dimSums(p_0 * q_0, dim=3, na.rm=TRUE)
    
    out <- out * 100
    out[is.nan(out)|is.infinite(out)] <- 0
    if (round) {out <- round(out)}
    
    out <- setNames(out,paste0("Prices|Food Price Index (Index ",sub("y","",baseyear),"=100)"))
    names(dimnames(out))[3] <- "variable"
    
    #weithts: values of consumption at the initial time step
    weights <- dimSums(p_0 * q_0, dim=3, na.rm=TRUE)
    names(dimnames(weights))[3] <- "variable"
    dimnames(weights)[[3]] <- "Aggregate Initial Consumption Value"
    
    weight       <- weights
    description  <- paste0("Fao Food Price Index, ",sub("y","",baseyear),"=100")
    isocountries <- TRUE 
    unit <- paste0("Index ",baseyear,"=100")
    
  } else if(datasource=="ProdPrIndex"){
    out <- readSource(type="ProdPrIndex")
    out <- out[,,value]
    
    weight       <- NULL
    description  <- "Fao Food Price Index, 2002-2004=100"
    unit         <- "Index 2002-2004=100"
    isocountries <- FALSE
  } 
  
  out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
  out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
  
  return(list(x=out,
              weight=weight,
              unit=unit,
              description=description,
              isocountries=isocountries))
}
